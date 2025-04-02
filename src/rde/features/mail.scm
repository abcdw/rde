;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024, 2025 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2022, 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde features mail)
  #:use-module (rde packages)
  #:use-module (rde exceptions)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module ((rde features mail providers) #:prefix mail-providers:)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde packages mail)
  #:use-module (rde serializers elisp)
  #:use-module (rde home services mail)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home services xdg)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)

  #:export (feature-mail-settings
            feature-emacs-message
            feature-isync
            feature-notmuch
            feature-msmtp
            feature-l2md
            feature-emacs-gnus
            feature-emacs-debbugs
            feature-emacs-smtpmail
            feature-emacs-org-mime
            feature-goimapnotify
            feature-emacs-piem

            mail-account
            mail-account-id
            mail-account-type
            mail-account-fqda
            mail-account-aliases
            mail-account-get-addresses
            mail-account-user
            mail-account-get-user
            mail-account-synchronizer
            mail-account-get-pass-cmd

            mailing-list
            mailing-list-id
            mailing-list-fqda
            mailing-list-synchronizer
            mailing-list-config

            generate-isync-serializer
            %rde-notmuch-saved-searches)

  #:re-export (l2md-repo))



;;;
;;; Records.
;;;

(define-configuration/no-serialization mail-account
  (id
   (symbol #f)
   "Something short to identify and distinguish a mail account.
@code{'work} or @code{'personal} for example.")
  ;; TODO: [Andrew Tropin, 2024-08-09] provider is more apropriate name for
  ;; this field
  (type
   (symbol 'generic)
   "Type of the mail account.  Will be used in different serialization
scenarios, during generation of @file{mbsyncrc} for example.")
  (synchronizer
   (symbol 'isync)
   "Type of application to obtain emails.")
  (fqda
   (string #f)
   "Email address. @code{\"someone@example.com\"} for example.")
  (aliases
   (list-of-strings '())
   "Aliases for @code{fqda}. @code{'(\"admin@example.com\")} for example.")
  (user
   (maybe-string #f)
   "User name.  Will default to @code{fqda}.")
  ;; TODO: Add sign? field. or maybe just annoy users, which doesn't
  ;; have a gpg key for all emails they use?
  (pass-cmd
   (maybe-string #f)
   "Command returning a password.  If value not provided @code{pass
show mail/$fqda} will be used, where @code{$fqda} is a value of fqda
field."))

(define (mail-account-get-pass-cmd mail-account)
  (if (mail-account-pass-cmd mail-account)
      (mail-account-pass-cmd mail-account)
      (string-append "pass show mail/" (mail-account-fqda mail-account))))

(define (mail-account-get-user account)
  "Return the value of user field or fqda if a value for user filed is not
present."
  (or (mail-account-user account)
      (mail-account-fqda account)))

(define (mail-account-get-addresses account)
  "Return a list containing fqda and aliases combined together."
  (cons
   (mail-account-fqda account)
   (mail-account-aliases account)))

(define-configuration/no-serialization mailing-list
  (id
   (symbol #f)
   "Something short to identify and distinguish a mailing lists.
@code{'guix-devel} or @code{'guix-patches} for example.")
  (fqda
   (string #f)
   "Mailing list address. @code{\"guix-devel@gnu.org\"} for example.")
  (config
   (record #f)
   "Mailing list configuration. @code{(l2md-repo ...)} for example.")
  (synchronizer
   (symbol 'l2md)
   "Type of application to obtain mailing list."))



;;;
;;; feature-mail-settings.
;;;

(define (list-of-mail-accounts? lst)
  (and (list? lst) (not (null? lst)) (every mail-account? lst)))

;; TODO: Check that all ML has uniq id
(define (list-of-mailing-lists? lst)
  (and (list? lst) (every mailing-list? lst)))

(define (default-mail-directory-fn config)
  (string-append (get-value 'home-directory config)
                 ;; TODO: Uses XDG_STATE_HOME
                 ;; (get-value 'xdg-blablabla config)
                 "/mail"))

(define* (feature-mail-settings
          #:key
          (mail-accounts #f)
          (mailing-lists '())
          (mail-providers-settings mail-providers:default-providers-settings)
          (mail-directory-fn default-mail-directory-fn))
  "Provide mail-accounts and mail-directory-fn for other mail-related
features."
  (ensure-pred list-of-mail-accounts? mail-accounts)
  (ensure-pred list-of-mailing-lists? mailing-lists)
  (ensure-pred procedure? mail-directory-fn)

  (define (get-home-services config)
    (list
     (simple-service
      'mail-settings-add-mailcap-file
      home-files-service-type
      ;; TODO: Reference xdg-open by full path?
      `((".mailcap" ,(plain-file "mailcap" "text/*; xdg-open %s\n"))))))

  (feature
   (name 'mail-settings)
   (home-services-getter get-home-services)
   (values `((mail-settings . #t)
             (mail-accounts . ,mail-accounts)
             (mailing-lists . ,mailing-lists)
             (mail-providers-settings . ,mail-providers-settings)
             (mail-directory-fn . ,mail-directory-fn)))))


;;;
;;; feature-emacs-message.
;;;

(define (default-message-signature config)
  (format #f "Best regards,\n~a" (get-value 'full-name config)))

(define-public (string-or-boolean-or-procedure? x)
  (or (string? x) (boolean? x) (procedure? x)))

(define* (feature-emacs-message
          #:key
          (message-signature default-message-signature))
  "Configure email sending capabilities provided by @file{message.el}.
@code{mail-signature} can be @code{#t}, @code{#f}, a string or a
function, which accepts config with rde values and returns a string."

  (ensure-pred string-or-boolean-or-procedure? message-signature)

  (define emacs-f-name 'message)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (require-value 'full-name config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define gpg-primary-key (get-value 'gpg-primary-key config #f))
    (define msmtp (get-value 'msmtp config #f))
    (define full-name (get-value 'full-name config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'message)
         (require 'sendmail))

        ,@(if (get-value 'emacs-gnus config #f)
              '((defun rde-message-add-gcc-header ()
                  "Prompt for a Gcc header from `rde-gnus-topic-alist'.
This will allow a message to be stored in the right directory
of the IMAP server (usually \"Sent\").
If this header is missing, the outgoing message will go through,
but it won't appear on the right Maildir directory."
                  (if (gnus-alive-p)
                      (unless (message-fetch-field "Gcc")
                        (message-add-header
                         (format "Gcc: %s"
                                 (let ((groups
                                        (cl-remove-if-not
                                         (lambda (group)
                                           (string-match (rx "Sent" eol) group))
                                         (rde-gnus--get-topic-groups))))
                                   (if (> 1 (length groups))
                                       (completing-read "Account: " groups)
                                     (car groups))))))
                    (error "Gnus is not running.  No GCC header will be inserted")))

                (add-hook 'message-header-setup-hook 'rde-message-add-gcc-header))
              '())

        (with-eval-after-load 'message
          (setq message-hidden-headers '())
          (setq message-kill-buffer-on-exit t)
          (setq message-signature
                ,(match message-signature
                   ((? procedure? e) (e config))
                   ((? string? e) e)
                   (#f 'nil)
                   (_ 't)))
          ,@(cond
             (msmtp
              `((setq sendmail-program
                      ,(file-append msmtp "/bin/msmtp"))
                (setq message-send-mail-function
                      'message-send-mail-with-sendmail)
                (setq message-sendmail-f-is-evil t)
                (setq message-sendmail-extra-arguments
                      '("--read-envelope-from"))))
             ((get-value 'emacs-smtpmail config #f)
              `((setq message-send-mail-function 'smtpmail-send-it)))
             (#t '()))

         ;; MAYBE: Move to feature-sourcehut
         ;; <https://git.sr.ht/~protesilaos/dotfiles/tree/a72ed49ea8/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el#L352>
         (defconst rde-message-patch-control-codes
           '("PROPOSED" "NEEDS_REVISION" "SUPERSEDED"
             "APPROVED" "REJECTED" "APPLIED")
           "Control codes for SourceHut patches.  See
`rde-message-srht-add-email-control-code' for how to apply them.")

         (defun rde-message-srht-add-email-control-code (control-code)
           "Add custom header for SourceHut email controls.  The CONTROL-CODE
is among `rde-notmuch-patch-control-codes'."
           (interactive
            (list (completing-read "Select control code: "
                                   rde-message-patch-control-codes nil t)))
           (if (member control-code rde-message-patch-control-codes)
               (message-replace-header
                "X-Sourcehut-Patchset-Update" control-code)
               (user-error "%s is not specified in
`rde-notmuch-patch-control-codes'" control-code)))

         ,@(if gpg-primary-key
             `((setq mml-secure-openpgp-signers '(,gpg-primary-key))
               (setq mml-secure-openpgp-encrypt-to-self t)
               ;; (setq mml-secure-openpgp-sign-with-sender t)
               (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime))
             '())

         (setq message-citation-line-function
               'message-insert-formatted-citation-line)

         (customize-set-variable
          'message-citation-line-format "On %Y-%m-%d %R, %N wrote:\n")
         (customize-set-variable
          'message-auto-save-directory
          (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                  "/emacs/mail-drafts"))))
      #:summary "\
Email sending interface tweaks"
      #:commentary "\
Citation line format, message signature, gpg and msmtp configurations. "
      #:keywords '(convenience))

     (emacs-xdg-service
      emacs-f-name
      "Emacs (Client) [mailto:]"
      #~(system*
         #$emacs-cmd "--eval"
         (string-append
          "\
(progn
 (set-frame-name \"Reply to Email - Emacs Client\")
 (browse-url-mail \"" (cadr (command-line)) "\"))"))
      #:default-for '(x-scheme-handler/mailto))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; feature-msmtp.
;;;

(define %default-msmtp-settings
  "defaults
tls on
auth on\n")

(define (default-msmtp-serializer provider-settings mail-account)
  "Generates a configuration string from key/value pairs found in
PROVIDER-SETTINGS using type field of MAIL-ACCOUNT.

Example:
@code{host mail.example.com\nport 587\n}"

  (define (provider-settings->msmtp-settings provider-settings)
    (map
     (lambda (x)
       (match x
         (('starttls? . v)
          `(tls_starttls . ,(if v 'on 'off)))
         (_ x)))
     provider-settings))
  (let* ((account-type (mail-account-type mail-account))
         (settings (provider-settings->msmtp-settings
                    (assoc-ref provider-settings account-type))))
    (apply
     string-append
     (map (lambda (p)
            (format #f "~a ~a\n" (car p) (cdr p)))
          settings))))

(define* (feature-msmtp
          #:key
          (msmtp msmtp)
          (msmtp-settings %default-msmtp-settings)
          (msmtp-provider-settings #f)
          (msmtp-serializer default-msmtp-serializer))
  "Configure msmtp MTA.

TLS enabled by default, SMTPS 465 port is used by default.

It is possible to use SMTP+STARTTLS by setting @code{starttls?} to @code{#t},
but not recommended. For SMTP port 587 will used by default.
@url{https://serverfault.com/questions/523804/is-starttls-less-safe-than-tls-ssl}

Also, port can be explicitly specified with @code{port} option (it's very
unlikely you ever need this)."

  (when msmtp-provider-settings
    (warning
     (G_ "'~a' in feature-msmtp is deprecated and ignored, use '~a' in feature-mail-setting instead~%")
     'msmtp-provider-setting 'mail-providers-setting))

  (define (get-home-services config)
    (require-value 'xdg-base-directories-configuration config)
    (require-value 'mail-accounts config
                   "feature-msmtp can't operate without mail-accounts.")

    (define msmtp-provider-settings
      (map (lambda (x)
             (let ((x (mail-providers:add-default-values-to-provider-settings x)))
               (cons (car x) (assoc-ref (cdr x) 'smtp))))
           (get-value 'mail-providers-settings config)))

    (define mail-accs (get-value 'mail-accounts config))
    (list
     (simple-service
      'msmtp-config
      home-xdg-configuration-files-service-type
      (list
       (list
        "msmtp/config"
        (apply
         mixed-text-file
         "msmtp-config"
         msmtp-settings
         (format #f "logfile ~a~a/log/msmtp.log\n"
                 (get-value 'home-directory config)
                 (string-drop
                  (home-xdg-base-directories-configuration-state-home
                   (get-value 'xdg-base-directories-configuration config))
                  (string-length "$HOME")))
          (append-map
           (lambda (acc)
             (map
              (lambda (address)
                (string-append
                 "\n"
                 "account " (symbol->string (mail-account-id acc))
                 "-" address "\n"
                 "from " address "\n"
                 "user " (mail-account-get-user acc) "\n"
                 "passwordeval " (mail-account-get-pass-cmd acc) "\n"
                 (msmtp-serializer msmtp-provider-settings acc)))
              (mail-account-get-addresses acc)))
           mail-accs)))))

     (when (get-value 'git-send-email? config #f)
       (simple-service
        'msmtp-set-git-send-email-cmd
        home-git-service-type
        (home-git-extension
         (config
          `((sendemail
             ((sendmailcmd . ,(file-append msmtp "/bin/msmtp --read-envelope-from")))))))))

     (simple-service
      'msmtp-package
      home-profile-service-type
      (list msmtp))))

  ;; TODO: Implement config serialization or msmtp-home-service
  (feature
   (name 'msmtp)
   (home-services-getter get-home-services)
   (values `((msmtp . ,msmtp)))))


;;;
;;; feature-l2md.
;;;

(define* (feature-l2md
          #:key
          (l2md l2md))
  "Configure l2md MDA."
  (ensure-pred file-like? l2md)

  (define (get-home-services config)
    (require-value 'mail-directory-fn config)
    (require-value 'mailing-lists config)
    (define mail-dir ((get-value 'mail-directory-fn config) config))
    (define mls (filter (lambda (x) (eq? (mailing-list-synchronizer x) 'l2md))
                        (get-value 'mailing-lists config)))
    (define (get-repo-config ml)
      (let ((repo-config (mailing-list-config ml)))
        (if (eq? %unset-value (l2md-repo-maildir repo-config))
            (l2md-repo
             (inherit repo-config)
             (maildir (string-append
                       mail-dir "/lists/" (mailing-list-fqda ml) "/archive")))
            repo-config)))
    ;; <https://git.kernel.org/pub/scm/linux/kernel/git/dborkman/l2md.git/about/>
    ;; Applying patches <https://git.kyleam.com/piem/about/>

    (define add-ml-tag
      (map (lambda (x)
             (format
              #f "notmuch tag +~a -- path:lists/~a/**"
              ;; TODO: Use new tag not to retag already existing entities.
              ;; Do it before new tag will be romved
              ;; TODO: Fix order of items in post-new hook
              (mailing-list-id x) (mailing-list-fqda x)))
           mls))

    (list
     (simple-service
      'l2md-add-tags-to-mailing-list
      home-notmuch-service-type
      (home-notmuch-extension
       (post-new
        (list
         #~(begin (for-each system '#$add-ml-tag))))))

     ;; TODO: Move it to a separate feature and make it conditional
     ;; https://lists.sr.ht/~abcdw/rde-devel/<20221118013128.6520-1-shilling.jake@gmail.com>
     (service home-mcron-service-type
              (home-mcron-configuration
               (jobs (list #~(job '(next-hour)
                                  (lambda ()
                                    (setenv "DISPLAY" ":0")
                                    (system* "mbsync" "-a")
                                    (system* "l2md")))))))
     (service
      home-l2md-service-type
      (home-l2md-configuration
       (l2md l2md)
       (oneshot 1)
       (repos (map get-repo-config mls))))))

  (feature
   (name 'l2md)
   (home-services-getter get-home-services)
   (values `((l2md . ,l2md)))))


;;;
;;; feature-emacs-gnus
;;;

(define* (feature-emacs-gnus
          #:key
          (posting-styles '())
          (group-parameters '())
          (gnus-directory "~/.cache/emacs/gnus")
          (topic-alist '())
          (topic-topology '())
          (mail-account-ids #f)
          (message-archive-method #f)
          (message-archive-group #f)
          (gnus-key "g"))
  "Configure the Gnus newsreader.
If MAIL-ACCOUNT-IDS is not provided, use all the mail accounts.
POSTING-STYLES allow you to set different email posting information based on
the current newsgroup or article.  See @pxref{Posting Styles,,,gnus} for more
details on how to write them.
GROUP-PARAMETERS allow you to tailor the settings of particular groups.  See
@pxref{Group Parameters,,,gnus} for more information on its syntax.
TOPIC-ALIST and TOPIC-TOPOLOGY let you declaratively categorize groups into
topics with your preferred hierarchy."
  (ensure-pred elisp-config? posting-styles)
  (ensure-pred path? gnus-directory)
  (ensure-pred elisp-config? group-parameters)
  (ensure-pred elisp-config? topic-alist)
  (ensure-pred elisp-config? topic-topology)
  (ensure-pred maybe-list? mail-account-ids)
  (ensure-pred maybe-list? message-archive-method)
  (ensure-pred maybe-list? message-archive-group)
  (ensure-pred string? gnus-key)

  (define emacs-f-name 'gnus)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Gnus."
    (require-value 'mail-accounts config)

    (define mail-accounts
      (if mail-account-ids
          (filter (lambda (x)
                    (member (mail-account-id x) mail-account-ids))
                  (get-value 'mail-accounts config))
          (get-value 'mail-accounts config)))
    (define mail-dir ((get-value 'mail-directory-fn config) config))
    (define (smtp-settings provider)
      (assoc-ref
       (assoc-ref
        (get-value 'mail-providers-settings config) provider)
       'smtp))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defgroup rde-gnus nil
          "Customizations for the Gnus newsreader."
          :group 'rde)
        (defcustom rde-gnus-topic-topology nil
          "Topics topology for Gnus."
          :group 'rde-gnus
          :type 'list)
        (defcustom rde-gnus-topic-alist nil
          "Alist of Gnus topics."
          :group 'rde-gnus
          :type 'list)
        (defvar rde-gnus-subscribed-p nil
          "Whether we're currently subscribed to Gnus groups.")
        (defun rde-gnus--get-topic-groups ()
          "Return a flattened list of groups from `rde-gnus-topic-alist'."
          (flatten-list (mapcar (lambda (topic)
                                  (cdr topic))
                                rde-gnus-topic-alist)))

        (defun rde-gnus-get-article-participants ()
          "Retrieve the participants from the current article."
          (if (and (gnus-alive-p)
                   (message-fetch-field "from")
                   (message-fetch-field "to"))
              (with-current-buffer gnus-article-buffer
                (string-join
                 (remove-if
                  (lambda (address)
                    (string-match user-mail-address address))
                  (append
                   (split-string (message-fetch-field "from") ", ")
                   (split-string (message-fetch-field "to") ", ")))
                 ", "))
            ""))

        (defun rde-gnus-shr-browse-url-new-window ()
          "When using shr, open links in a new window."
          (interactive)
          (shr-browse-url nil nil t))

        (define-minor-mode rde-gnus-topic-mode
          "Apply Gnus topic settings declaratively and subscribe to groups."
          :group 'rde-gnus
          (setq gnus-topic-topology rde-gnus-topic-topology)
          (setq gnus-topic-alist rde-gnus-topic-alist)
          (unless rde-gnus-subscribed-p
            (mapc (lambda (topic)
                    (gnus-subscribe-hierarchically topic))
                  (rde-gnus--get-topic-groups)))
          (setq rde-gnus-subscribed-p t))

        (setq rde-gnus-topic-alist ',topic-alist)
        (setq rde-gnus-topic-topology ',topic-topology)
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,gnus-key) 'gnus))
        (setq mail-user-agent 'gnus-user-agent)
        ,@(if (get-value 'emacs-dired config #f)
              '((add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))
              '())
        (with-eval-after-load 'gnus
          (setq gnus-use-full-window nil)
          (setq gnus-use-cache t)
          ,@(if (get-value 'emacs-advanced-user? config #f)
                `((setq gnus-novice-user nil))
              '())
          (setq gnus-interactive-exit nil)
          (setq gnus-thread-sort-functions
                '(gnus-thread-sort-by-most-recent-date
                  (not gnus-thread-sort-by-number)))
          (setq gnus-permanently-visible-groups "^nnmaildir")
          (setq gnus-parameters ',group-parameters)
          (setq gnus-directory ,gnus-directory)
          (setq gnus-home-directory (locate-user-emacs-file "gnus"))
          (setq gnus-cache-directory
                ,(string-append gnus-directory "/news/cache"))
          (setq gnus-kill-files-directory
                ,(string-append gnus-directory "/news"))
          (setq gnus-article-save-directory
                ,(string-append gnus-directory "/news"))
          (setq gnus-large-newsgroup 100)
          ,@(if message-archive-method
                `((setq gnus-message-archive-method ',message-archive-method))
                '())
          ,@(if message-archive-group
                `((setq gnus-message-archive-group ',message-archive-group))
                '())
          (setq gnus-update-message-archive-method t)
          (setq gnus-posting-styles
                '(,@(map (lambda (mail-acc)
                           (let* ((id (symbol->string (mail-account-id mail-acc)))
                                  (email (mail-account-fqda mail-acc))
                                  (provider (mail-account-type mail-acc))
                                  (host (assoc-ref (smtp-settings provider) 'host))
                                  (port (or (assoc-ref (smtp-settings provider) 'port)
                                            587)))
                             `(,id
                               (address ,email)
                               ("Gcc" ,(string-append "nnmaildir+" id ":sent"))
                               ,@(if (get-value 'msmtp config #f)
                                     '()
                                     `(("X-Message-SMTP-Method"
                                        ,(format
                                          #f "smtp ~a ~a ~a"
                                          host port email)))))))
                         mail-accounts)
                  ,@posting-styles))
          (setq gnus-select-method '(nnnil))
          (setq gnus-secondary-select-methods
                '(,@(if (get-value 'isync config #f)
                        (map (lambda (mail-acc)
                               (let ((id (symbol->string
                                          (mail-account-id mail-acc))))
                                 `(nnmaildir
                                   ,id
                                   (directory
                                    ,(string-append mail-dir "/accounts/" id)))))
                             mail-accounts)
                      '())
                  (nntp "gwene"
                        (nntp-address "news.gwene.org"))
                  (nnfolder "archive"
                            (nnfolder-directory
                             ,(string-append mail-dir "/archive"))
                            (nnfolder-active-file
                             ,(string-append mail-dir "/archive/active"))
                            (nnfolder-get-new-mail nil)
                            (nnfolder-inhibit-expiry t)))))
        (with-eval-after-load 'mail-source
          (setq mail-source-directory ,(string-append gnus-directory "/mail"))
          (setq mail-default-directory ,gnus-directory))
        (with-eval-after-load 'gnus-start
          (setq gnus-dribble-directory ,gnus-directory)
          (setq gnus-startup-file ,(string-append gnus-directory "/.newsrc"))
          (setq gnus-subscribe-newsgroup-method
                'gnus-subscribe-hierarchically)
          (setq gnus-check-new-newsgroups nil)
          (setq gnus-save-killed-list nil))
        (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
        (add-hook 'gnus-group-mode-hook 'hl-line-mode)
        (with-eval-after-load 'gnus-sum
          (setq gnus-summary-goto-unread 'never)
          (setq gnus-thread-hide-subtree t))
        (with-eval-after-load 'nndraft
          (setq nndraft-directory
                ,(string-append gnus-directory "/mail/drafts")))
        (add-hook 'gnus-topic-mode-hook 'rde-gnus-topic-mode)
        (with-eval-after-load 'gnus-topic
          (setq gnus-gcc-mark-as-read t)
          (setq gnus-server-alist
                '(("archive" nnfolder "archive"
                   (nnfolder-directory
                    ,(string-append gnus-directory "/mail/archive"))
                   (nnfolder-get-new-mail nil)
                   (nnfolder-inhibit-expiry t)))))
        (with-eval-after-load 'gnus-art
          (let ((map gnus-article-mode-map))
            (define-key map (vector 'remap 'shr-mouse-browse-url)
              'shr-mouse-browse-url-new-window)
            (define-key map (vector 'remap 'shr-browse-url)
              'rde-gnus-shr-browse-url-new-window))
          (setq gnus-visible-headers
                '("^From:" "^To:" "^Cc:" "^Subject:" "^Newsgroups:" "^Date:"
                  "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
                  "^X-Mailer:" "^Message-ID:" "^In-Reply-To:" "^References:"))
          (setq gnus-sorted-header-list gnus-visible-headers))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-debbugs
;;;

(define* (feature-emacs-debbugs
          #:key
          (emacs-debbugs emacs-debbugs)
          (default-packages
           (list "emacs" "guix" "guix-patches"))
          (default-severities
           (list "serious" "important" "normal" "tagged")))
  "Configure the Debbugs user interface for Emacs.
Set the default packages to retrieve bugs for with DEFAULT-PACKAGES and the
default severities with which bugs should be filered with DEFAULT-SEVERITIES."
  (ensure-pred file-like? emacs-debbugs)
  (ensure-pred list? default-packages)
  (ensure-pred list? default-severities)

  (define emacs-f-name 'debbugs)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Debbugs."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'debbugs-gnu
          (require 'xdg)
          (setq debbugs-gnu-persistency-file
                (expand-file-name "emacs/debbugs"
                                  (xdg-cache-home)))
          (setq debbugs-gnu-default-packages (list ,@default-packages))
          (setq debbugs-gnu-default-severities (list ,@default-severities))))
      #:elisp-packages (list emacs-debbugs))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-debbugs)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-smtpmail
;;;

(define* (feature-emacs-smtpmail
          #:key
          (mail-acc-id #f))
  "Configure smtpmail, a simple mail protocol for sending mail from Emacs.
If no MAIL-ACC-ID is provided, no account-specific settings will be
configured."
  (ensure-pred maybe-symbol? mail-acc-id)

  (define emacs-f-name 'smtpmail)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to smtpmail."
    (require-value 'mail-accounts config)

    (define mail-acc
      (if mail-acc-id
          (car (filter (lambda (acc)
                         (eq? (mail-account-id acc) mail-acc-id))
                       (get-value 'mail-accounts config)))
          #f))

    (define smtp-provider-settings
      (assoc-ref
       (assoc-ref (get-value 'mail-providers-settings config)
                  (and=> mail-acc mail-account-type))
       'smtp))
    (define smtp-host
      (assoc-ref smtp-provider-settings 'host))
    (define smtp-port
      (or (assoc-ref smtp-provider-settings 'port) 587))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'smtpmail
          (require 'xdg)
          ,@(if mail-acc-id
                `((setopt smtpmail-smtp-user ,(mail-account-get-user mail-acc))
                  (setopt smtpmail-smtp-service ,smtp-port)
                  (setopt smtpmail-smtp-server ,smtp-host)
                  (setopt smtpmail-default-smtp-server ,smtp-host))
                '())
          (setopt smtpmail-stream-type 'starttls)
          (setopt smtpmail-queue-dir
                  (expand-file-name "emacs/smtpmail/queued-mail"
                                    (xdg-cache-home)))
          (setopt smtpmail-debug-info t))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-org-mime
;;;

(define* (feature-emacs-org-mime
          #:key
          (emacs-org-mime emacs-org-mime)
          (html-element-styles '()))
  "Configure org-mime for wysiwyg html mime mail composition via Org Mode.
You can change the exported HTML-ELEMENT-STYLES by providing an association
list of ELEMENT . STYLE such as:

@lisp
'((\"pre\" . \"color: #E6E1Dc; background-color: #232323; padding: 0.5em;\")
  (\"blockquote\" . \"border-left: 2px solid gray; padding-left: 4px;\"))
@end lisp

The above darkens exported code-blocks and adds a border and some padding
to offset block quotes."
  (ensure-pred file-like? emacs-org-mime)
  (ensure-pred alist? html-element-styles)

  (define emacs-f-name 'org-mime)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to org-mime."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'org-mime))
        (defun rde-org-mime-change-element-styles ()
          "Apply extra css styles when exporting html email."
          ,@(map (lambda (el)
                   `(org-mime-change-element-style
                     ,(car el) ,(cdr el)))
                 html-element-styles))

        ,@(if (null? html-element-styles)
              '()
              '((add-hook 'org-mime-html-hook
                          'rde-org-mime-change-element-styles)))
        (with-eval-after-load 'org
          (define-key org-mode-map (kbd "C-c M-o")
            'org-mime-org-buffer-htmlize))
        (with-eval-after-load 'message
          (let ((map message-mode-map))
            (define-key map (kbd "C-c M-z") 'org-mime-htmlize)
            (define-key map (kbd "C-c M-o") 'org-mime-edit-mail-in-org-mode)))
        (with-eval-after-load 'org-mime
          (setq org-mime-export-options
                '(:section-numbers nil :with-author nil :with-toc nil))))
      #:elisp-packages (list emacs-org-mime))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-org-mime)))
   (home-services-getter get-home-services)))


;;;
;;; feature-goimapnotify
;;;

(define* (feature-goimapnotify
          #:key
          (mail-account-ids #f)
          (goimapnotify go-gitlab.com-shackra-goimapnotify-next)
          (notify? #f))
  "Set up and configure goimapnotify to listen on IMAP mailbox changes.  If
MAIL-ACCOUNT-IDS is not provided, use all the mail accounts.  You can also
control whether to NOTIFY? when new emails arrive."
  (ensure-pred maybe-list? mail-account-ids)
  (ensure-pred file-like? goimapnotify)
  (ensure-pred boolean? notify?)

  (define (get-home-services config)
    "Return home services related to goimapnotify."
    (require-value 'mail-accounts config)

    (define mail-accounts
      (if mail-account-ids
          (filter (lambda (x)
                    (member (mail-account-id x) mail-account-ids))
                  (get-value 'mail-accounts config))
          (get-value 'mail-accounts config)))

    (list
     (service
      home-goimapnotify-service-type
      (home-goimapnotify-configuration
       (goimapnotify goimapnotify)
       (config
        `#(,@(map
              (lambda (acc)
                `((host . ,(assoc-ref
                            (assoc-ref
                             (assoc-ref
                              (get-value 'mail-providers-settings config)
                              (mail-account-type acc))
                             'smtp)
                            'host))
                  (port . 143)
                  (tls . #f)
                  (tlsOptions . ((rejectUnauthorized . #t)))
                  (username . ,(mail-account-fqda acc))
                  (passwordCmd . ,(mail-account-get-pass-cmd acc))
                  (xoauth2 . #f)
                  (alias . ,(mail-account-id acc))
                  (trigger . 20)
                  (boxes
                   .
                   #(((mailbox . "Inbox")
                      ,@(if (get-value 'isync config #f)
                            (list
                             (cons
                              'onNewMail
                              #~(format
                                 #f "~s"
                                 (string-join
                                  (list #$(get-value-eval 'mbsync config)
                                        #$@((get-value
                                             'mail-acc->isync-args config)
                                            acc))
                                  " "))))
                            '())
                      ,@(if notify?
                            (cond
                             ((get-value 'emacs-ednc config #f)
                              (list
                               (cons 'onNewMailPost
                                     #~(format
                                        #f "~s"
                                        (string-join
                                         (list
                                          #$(file-append
                                             (get-value 'emacs config)
                                             "/bin/emacsclient")
                                          "-e"
                                          (format
                                           #f "'~s'"
                                           '(notifications-notify
                                             :app-name "goimapnotify"
                                             :title "New email received"
                                             :timeout 5000))))))))
                             (else '()))
                            '()))))))
              mail-accounts)))))))

  (feature
   (name 'goimapnotify)
   (values `((goimapnotify . ,goimapnotify)))
   (home-services-getter get-home-services)))


;;;
;;; feature-piem
;;;

(define* (feature-emacs-piem
          #:key
          (emacs-piem emacs-piem)
          (piem-inboxes '()))
  "The tool for working with public-inbox archives and for applying patches (not
only from public inbox).  It has integration with notmuch and possibly a few
other tools (get more info in the documentation of the project:
@url{https://docs.kyleam.com/piem/})."
  (ensure-pred file-like? emacs-piem)
  (ensure-pred list? piem-inboxes)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      'emacs-piem
      config
      `((with-eval-after-load 'piem
          (setq piem-inboxes ',(get-value 'piem-inboxes config)))
        (with-eval-after-load 'notmuch
          (add-hook 'notmuch-show-mode-hook 'piem-notmuch-mode)))
      #:elisp-packages (list (get-value 'emacs-piem config)))))

  (feature
   (name 'emacs-piem)
   (values `((emacs-piem . ,emacs-piem)
             (piem-inboxes . ,piem-inboxes)))
   (home-services-getter get-home-services)))


;;;
;;; feature-isync.
;;;

(define (prep-str sym str)
  (symbol-append sym '- (string->symbol str)))

(define (isync-channel id local remote)
  `((Channel ,(prep-str id local))
    (Near ,(format #f ":~a-local:~a" id local))
    (Far ,(format #f ":~a-remote:~a" id remote))
    ,#~""))

(define (isync-group-with-channels id isync-mapping)
  (append
   (append-map
    (lambda (x) (isync-channel id (car x) (cdr x)))
    isync-mapping)
   `((Group ,(symbol-append id))
     ,@(map
        (lambda (x) (list 'Channel (prep-str id (car x))))
        isync-mapping)
     ,#~"")))

(define* (%generate-isync-serializer imap-settings)
  (let ((host (assoc-ref imap-settings 'host))
        (folder-mapping (or (assoc-ref imap-settings 'folder-mapping)
                            mail-providers:generic-folder-mapping))
        (options (assoc-ref imap-settings 'extra-options)))
    (ensure-pred string? host)
    (ensure-pred list? folder-mapping)
    (define (isync-settings mail-directory mail-account)
      (let* ((id       (mail-account-id mail-account))
             (account  (symbol->string id))
             (user     (mail-account-get-user mail-account))
             (pass-cmd (mail-account-get-pass-cmd mail-account))
             (subfolders (or (assoc-ref options 'subfolders) 'Verbatim))
             (auth-mechs (assoc-ref options 'auth-mechs))
             (cipher-string (assoc-ref options 'cipher-string))
             (pipeline-depth (assoc-ref options 'pipeline-depth))
             (port (assoc-ref options 'port)))
        (ensure-pred symbol? subfolders)
        `(,#~(string-append "# Account '" #$(symbol->string id)
                            " starts here")
          (IMAPAccount ,id)
          (Host ,host)
          ,@(if (integer? port) `((Port ,port)) '())
          (User ,user)
          (PassCmd ,pass-cmd)
          ,@(if (symbol? auth-mechs) `((AuthMechs ,auth-mechs)) '())
          (TLSType IMAPS)
          (CertificateFile /etc/ssl/certs/ca-certificates.crt)
          ,@(if (symbol? cipher-string) `((CipherString ,cipher-string)) '())
          ,@(if (integer? pipeline-depth) `((PipelineDepth ,pipeline-depth)) '())
          ,#~""
          (IMAPStore ,(symbol-append id '-remote))
          (Account ,id)
          ,#~""
          (MaildirStore ,(symbol-append id '-local))
          (SubFolders ,subfolders)
          (Path ,(string-append mail-directory "/accounts/" account "/"))
          (Inbox ,(string-append mail-directory "/accounts/" account "/inbox"))
          ,#~""
          ,@(isync-group-with-channels id folder-mapping))))
    isync-settings))

(define (%get-isync-serializers mail-providers-settings)
  (map (lambda (provider-settings)
         (let ((provider (car provider-settings))
               (settings (assoc-ref (cdr provider-settings) 'imap)))
           `(,provider . ,(%generate-isync-serializer settings))))
       mail-providers-settings))

(define default-isync-global-settings
  `((Create Both)
    (Expunge Both)
    (SyncState *)
    ,#~""))

(define* (feature-isync
          #:key
          (isync isync)
          (mail-account-ids #f)
          (isync-global-settings default-isync-global-settings)
          (isync-serializers #f)
          (get-isync-serializers %get-isync-serializers)
          (isync-verbose #f))
  "Setup and configure isync.  If MAIL-ACCOUNT-IDS not provided use all
mail accounts.  ISYNC-VERBOSE controls output verboseness of
@file{mbsync}."
  (ensure-pred file-like? isync)
  (ensure-pred maybe-list? mail-account-ids)
  (ensure-pred list? isync-global-settings)
  (ensure-pred boolean? isync-verbose)

  (when isync-serializers
    (warning
     (G_ "'~a' in feature-isync is deprecated and ignored, use '~a' instead~%")
     'isync-serializers 'get-isync-serializers))

  ;; Sync mail deletion
  ;; https://notmuchmail.org/pipermail/notmuch/2016/023112.html
  ;; http://tiborsimko.org/mbsync-duplicate-uid.html

  ;; https://notmuchmail.org/emacstips/#index25h2
  (define (get-home-services config)
    (require-value 'mail-accounts config
                   "feature-isync can't operate without mail-accounts.")

    (let* ((mail-accounts
            (filter (lambda (x) (eq? (mail-account-synchronizer x) 'isync))
                    (get-value 'mail-accounts config)))
           (mail-directory-fn (get-value 'mail-directory-fn config))
           (mail-directory    (mail-directory-fn config))
           (isync-serializers (get-isync-serializers
                               (get-value 'mail-providers-settings config))))

      (define (serialize-mail-acc mail-acc)
        (let* ((provider (mail-account-type mail-acc))
               (serializer (assoc-ref isync-serializers provider)))
          (unless (procedure? serializer)
            (raise-exception
             (config-exception "Serializer for ~a is missing" (list provider))))
          (serializer mail-directory mail-acc)))

      (list
       (simple-service
        'isync-ensure-mail-dirs-exists
        home-activation-service-type
        #~(map mkdir-p
               '#$(map (lambda (acc)
                         (string-append mail-directory "/accounts/"
                                        (symbol->string (mail-account-id acc))))
                       mail-accounts)))
       (service
        home-isync-service-type
        (home-isync-configuration
         (isync isync)
         (config
          (append
           isync-global-settings
           (append-map serialize-mail-acc mail-accounts))))))))

  ;; Necessary to go get this package for proper mbsync configuration.
  ;; That's because home-isync-service-type wraps the isync binary,
  ;; and we need this wrapper for isync to read the right config.
  (define (mbsync config)
    (file-append
     (car
      (service-value
       (fold-services
        (instantiate-missing-services
         (cons* (service home-profile-service-type '())
                (get-home-services config)))
        #:target-type home-profile-service-type)))
     "/bin/mbsync"))

  (define (mail-acc->isync-args mail-acc)
    (list (if isync-verbose "-V" "")
          (symbol->string (mail-account-id mail-acc))))

  (feature
   (name 'isync)
   (values `((isync . #t)
             (mbsync . ,mbsync)
             (mail-acc->isync-args . ,mail-acc->isync-args)))
   (home-services-getter get-home-services)))


;;;
;;; feature-notmuch.
;;;

;; Do it in Guile : https://git.oscarnajera.com/dotfiles/tree/bin/tagmail
(define* (default-get-notmuch-configuration config
           #:key
           (extra-tag-updates-post '()))
  (define full-name (get-value 'full-name config))
  (define mail-accounts (get-value 'mail-accounts config))
  (define emails (map mail-account-fqda mail-accounts))
  (define ids    (map mail-account-id   mail-accounts))
  (define mail-directory ((get-value 'mail-directory-fn config) config))

  (define make-id-tag
    ;; It can be tempting to tag only new emails, but there is a case, where
    ;; it won't work: imagine you obtained email via l2md or personal inbox,
    ;; reindexed notmuch database and later obtained the same email in you
    ;; work inbox.  In this case the email won't be tagged with work tag,
    ;; because it's not new email, but already indexed one and it's quite
    ;; surpising to accidentially find such email in a different place, when
    ;; you expected it to be in your work inbox search.
    (map (lambda (x)
           (format
            #f "notmuch tag +~a -- path:accounts/~a/**"
            (mail-account-id x) (mail-account-id x)))
         mail-accounts))

  (define tag-updates-post
    (append
     '("notmuch tag +inbox -- path:/accounts\\/.*\\/inbox/"
       "notmuch tag +draft -- path:/accounts\\/.*\\/drafts/"
       "notmuch tag +sent  -- path:/accounts\\/.*\\/sent/"
       "notmuch tag +trash -- path:/accounts\\/.*\\/trash/"
       "notmuch tag +spam  -- path:/accounts\\/.*\\/spam/"
       "notmuch tag +list  -- path:/lists\\/.*/"
       "notmuch tag +todo -inbox -sent  -- tag:inbox and tag:sent"
       ;; If file was moved out of folder on server remove respective tag
       "notmuch tag -inbox -- not path:/accounts\\/.*\\/inbox/ and tag:inbox"
       "notmuch tag -trash -- not path:/accounts\\/.*\\/trash/ and tag:trash"
       "notmuch tag -spam  -- not path:/accounts\\/.*\\/spam/  and tag:spam")
     extra-tag-updates-post
     '("notmuch tag -new -- tag:new")))

  (define (move-out-untagged-messages tag)
    "If tag was removed -> move out of the related folder."
    (format #f "notmuch search --output=files \
'path:/.*\\/~a/ and not tag:~a' | grep '/~a/' | \
while IFS= read -r f; do mv -v \"$f\" \
\"$(echo ${f} | sed 's;/~a/;/archive/;' | sed 's/,U=[0-9]*:/:/')\"; done"
            tag tag tag tag))

  (define* (move-in-tagged-messages
            tag
            #:key (exclude-dir "nothing-will-match-this"))
    (format #f "notmuch search --output=files \
'not path:/.*\\/~a/ and tag:~a' | grep -v \"/~a/\" | \
while IFS= read -r f; do mv -v \"$f\" \
\"$(echo ${f} | sed 's;/[[:alnum:]]*/cur/;/~a/cur/;' | sed 's/,U=[0-9]*:/:/')\"; done"
            tag tag exclude-dir tag))
  (define delete-deleted-messages
    "notmuch search --output=files tag:deleted | \
while IFS= read -r f; do rm -v \"$f\"; done")

  (define move-rules
    (append
     (map move-out-untagged-messages '(inbox trash spam))
     (map move-in-tagged-messages '(trash spam))
     (list (move-in-tagged-messages 'inbox #:exclude-dir "archive")
           delete-deleted-messages)))

  (home-notmuch-extension
   (pre-new
    (list
     (with-imported-modules
      '((guix build utils))
      #~(begin
          (for-each system '#$move-rules)))))
   (post-new
    (list
     #~(begin (for-each system '#$make-id-tag)
              (for-each system '#$tag-updates-post))))
   (config
    `((user ((name . ,full-name)
             (primary_email . ,(car emails))
             (other_email . ,(cdr emails))))
      (database ((path . ,mail-directory)
                 (mail_root . ,mail-directory)))
      (maildir ((synchronize_flags . true)))
      (search ((exclude_tags . (trash spam deleted))))
      (new ((tags . new)
            (ignore . (.mbsyncstate .uidvalidity
                                    .mbsyncstate.new .mbsyncstate.journal))))
      (query ,(get-value 'notmuch-queries config))))))

(define (notmuch-redefined-functions config)
  ;; Remove leading arrows for mails without threads
  ;; Make the width used by notmuch-jump prompt to be 80%
  `((defun rde-notmuch-tree-insert-tree (tree depth tree-status first last)
      "Insert the message tree TREE at depth DEPTH in the current thread.

A message tree is another name for a single sub-thread: i.e., a
message together with all its descendents."
      (let ((msg (car tree))
            (replies (cadr tree)))
        (cond
         ((and (< 0 depth) (not last))
          (push "├" tree-status))
         ((and (< 0 depth) last)
          (push "└" tree-status))
         ((and (eq 0 depth) first last)
          ;; Choice between these two variants is a matter of taste.
          ;; (push "─" tree-status))
          (push " " tree-status))
         ((and (eq 0 depth) first (not last))
          (push "┬" tree-status))
         ((and (eq 0 depth) (not first) last)
          (push "└" tree-status))
         ((and (eq 0 depth) (not first) (not last))
          (push "├" tree-status)))
        (unless (eq 0 depth)
          (push (concat (if replies "┬" "─") ">") tree-status))
        (setq msg (plist-put msg :first (and first (eq 0 depth))))
        (setq msg (plist-put msg :tree-status tree-status))
        (setq msg (plist-put msg :orig-tags (plist-get msg :tags)))
        (notmuch-tree-goto-and-insert-msg msg)
        (pop tree-status)
        (pop tree-status)
        (if last
            (push " " tree-status)
            (push "│" tree-status))
        (notmuch-tree-insert-thread replies (+ 1 depth) tree-status)))
    (advice-add 'notmuch-tree-insert-tree :override
                'rde-notmuch-tree-insert-tree)

    (defun rde-notmuch-jump (action-map prompt)
      "Interactively prompt for one of the keys in ACTION-MAP.

Displays a summary of all bindings in ACTION-MAP in the
minibuffer, reads a key from the minibuffer, and performs the
corresponding action.  The prompt can be canceled with C-g or
RET.  PROMPT must be a string to use for the prompt.  PROMPT
should include a space at the end.

ACTION-MAP must be a list of triples of the form
  (KEY LABEL ACTION)
where KEY is a key binding, LABEL is a string label to display in
the buffer, and ACTION is a nullary function to call.  LABEL may
be null, in which case the action will still be bound, but will
not appear in the pop-up buffer."
      (let* ((items (notmuch-jump--format-actions action-map))
             ;; Format the table of bindings and the full prompt
             (table
              (with-temp-buffer
               (notmuch-jump--insert-items
                (floor (* (frame-width) 0.8)) items)
               (buffer-string)))
             (full-prompt
              (concat table "\n\n"
                      (propertize prompt 'face 'minibuffer-prompt)))
             ;; By default, the minibuffer applies the minibuffer face to
             ;; the entire prompt.  However, we want to clearly
             ;; distinguish bindings (which we put in the prompt face
             ;; ourselves) from their labels, so disable the minibuffer's
             ;; own re-face-ing.
             (minibuffer-prompt-properties
              (notmuch-plist-delete
               (copy-sequence minibuffer-prompt-properties)
               'face))
             ;; Build the keymap with our bindings
             (minibuffer-map (notmuch-jump--make-keymap action-map prompt))
             ;; The bindings save the the action in notmuch-jump--action
             (notmuch-jump--action nil))
        ;; Read the action
        (read-from-minibuffer full-prompt nil minibuffer-map)
        ;; If we got an action, do it
        (when notmuch-jump--action
          (funcall notmuch-jump--action))))
    (advice-add 'notmuch-jump :override 'rde-notmuch-jump)))

(define %rde-notmuch-saved-searches
  '((:name "TODO" :query "tag:todo" :key "t")
    (:name "Inbox" :query "tag:inbox" :key "i")
    (:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "w")
    (:name "Drafts" :query "tag:draft" :key "d")
    (:name "Flagged" :query "tag:flagged" :key "f")
    (:name "Sent" :query "tag:sent" :key "s")
    (:name "All mail" :query "*" :key "a")))

;; TODO: revisit this package
;; https://github.com/larkery/emacs/blob/master/site-lisp/notmuch-attachment-list.el
;; https://michal.sapka.me/2023/notmuch

;; TODO: [Andrew Tropin, 2025-03-13] Add notmuch-addr for better address
;; completion.

;; TODO: [Andrew Tropin, 2024-05-07] Add bug-reference support, similiar to
;; bug-reference-setup-from-mail-alist, extandable from feature-sourcehut,
;; feature-guix and others
(define* (feature-notmuch
          #:key
          (notmuch notmuch)
          (emacs-notmuch emacs-notmuch)
          (emacs-consult-notmuch emacs-consult-notmuch)
          (emacs-ol-notmuch emacs-ol-notmuch)
          (get-notmuch-configuration default-get-notmuch-configuration)
          (notmuch-saved-searches %rde-notmuch-saved-searches)
          (notmuch-search-oldest-first #t)
          (notmuch-queries '())
          (extra-tag-updates-post '()))
  "Configure notmuch and Emacs UI for it if emacs enabled.
@code{notmuch-queries} is a list of pairs of query name and query, it added to
notmuch config and can be used anywhere in search via @code{query:query-name}."
  (ensure-pred file-like? notmuch)
  (ensure-pred file-like? emacs-notmuch)
  (ensure-pred file-like? emacs-ol-notmuch)
  (ensure-pred file-like? emacs-consult-notmuch)
  (ensure-pred procedure? get-notmuch-configuration)
  (ensure-pred list? %rde-notmuch-saved-searches)
  (ensure-pred boolean? notmuch-search-oldest-first)
  (ensure-pred alist? notmuch-queries)
  (ensure-pred list? extra-tag-updates-post)

  (define f-name 'notmuch)

  (define (get-home-services config)
    (define emails
      (append-map
       mail-account-get-addresses
       (get-value 'mail-accounts config)))

    (define fcc-dirs
      (append-map
       (lambda (account)
         ;; email address can differ from user name, also it can be a few
         ;; email addresses attached to on user name.
         (let ((dir (string-append
                     "accounts/"
                     (symbol->string (mail-account-id account)) "/sent")))
           (map (lambda (x) (cons x dir))
                (mail-account-get-addresses account))))
       (get-value 'mail-accounts config)))

    (list
     (service
      home-notmuch-service-type
      (home-notmuch-configuration
       (notmuch notmuch)))
     (simple-service
      'notmuch-service
      home-notmuch-service-type
      (get-notmuch-configuration
       config
       #:extra-tag-updates-post extra-tag-updates-post))
     (when (get-value 'emacs config #f)
       (rde-elisp-configuration-service
        f-name
        config
        ;; https://protesilaos.com/dotemacs/#h:a196812e-1644-4536-84ba-687366867def
        ;; https://codeberg.org/jao/elibs/src/branch/main/notmuch.org
        `((eval-when-compile
           (require 'notmuch))

          (autoload 'notmuch-mua-mail "notmuch-mua")
          ;; Copied definition from notmuch-mua becasue it's not
          ;; available until notmuch-mua loaded.

          ;; FIXME: C-x m doesn't accept prefix argument, so I can't
          ;; pick sender.
          (define-mail-user-agent 'notmuch-user-agent
            'notmuch-mua-mail
            'notmuch-mua-send-and-exit
            'notmuch-mua-kill-buffer
            'notmuch-mua-send-hook)
          (setq mail-user-agent 'notmuch-user-agent)

          (with-eval-after-load 'rde-keymaps
            (define-key rde-app-map (kbd "n") 'notmuch))

          ,@(if (get-value 'emacs-consult config #f)
                '((define-key global-map (kbd "M-s n") 'consult-notmuch-tree))
                '())

          (define-key global-map (kbd "s-m") 'notmuch-jump-search)
          (setq notmuch-saved-searches ',notmuch-saved-searches)
          (setq notmuch-search-oldest-first
                ,(if notmuch-search-oldest-first 't 'nil))

          (with-eval-after-load
           'notmuch-mua
           ;; (setq sendmail-program "true")
           (require 'notmuch))

          (with-eval-after-load
           'notmuch
           (setq notmuch-fcc-dirs ',fcc-dirs)
           (setq notmuch-identities ',emails)
           (setq notmuch-address-use-company nil)

           (defun rde-notmuch-address-setup ()
             "Function doing nothing"
             nil)
           (advice-add 'notmuch-address-setup :override
                       'rde-notmuch-address-setup)

           (require 'cape)
           (defun rde-notmuch-message-mode ()
             "Add completion at point functions made from company backends."
             (setq-local
              completion-at-point-functions
              (append
               ;; FIXME: doesn't work well with consult-completion-in-region
               ;; list of candidates not get updated when removing characters.
               (list (cape-company-to-capf 'notmuch-company))
               completion-at-point-functions)))
           (add-hook 'notmuch-message-mode-hook 'rde-notmuch-message-mode)

           (setq notmuch-unthreaded-show-out nil)

           (setq notmuch-show-empty-saved-searches t)
           (setq notmuch-mua-cite-function 'message-cite-original-without-signature)

           (defvar rde-notmuch-todo-tags '("+todo" "-inbox"))
           (defvar rde-notmuch-spam-tags '("+spam" "-inbox"))
           (defvar rde-notmuch-trash-tags '("+trash" "-inbox" "-draft"))
           (defvar rde-notmuch-delete-tags '("+deleted" "-inbox" "-draft"))
           (setq notmuch-archive-tags '("-inbox" "-todo"))
           (setq notmuch-tagging-keys
                 '(("a" notmuch-archive-tags "Archive")
                   ("r" notmuch-show-mark-read-tags "Mark read")
                   ("f" ("+flagged") "Flag (favorite)")
                   ;; ("w" ("+watch") "Watch")
                   ("t" rde-notmuch-todo-tags "Mark as todo")
                   ("s" rde-notmuch-spam-tags "Mark as spam")
                   ("d" rde-notmuch-trash-tags "Trash")
                   ("D" rde-notmuch-delete-tags "Delete")))

           ;; https://notmuchmail.org/emacstips/#index25h2
           (defun rde-notmuch-show-view-as-patch ()
             "View the the current message as a patch."
             (interactive)
             (let* ((id (notmuch-show-get-message-id))
                    (msg (notmuch-show-get-message-properties))
                    (part (notmuch-show-get-part-properties))
                    (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
                    (diff-default-read-only t)
                    (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
                    (map (make-sparse-keymap)))
               (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
               (switch-to-buffer buf)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert subject)
                 (insert (notmuch-get-bodypart-text msg part nil)))
               (set-buffer-modified-p nil)
               (diff-mode)
               (let ((new-ro-bind (cons 'buffer-read-only map)))
                 (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
               (goto-char (point-min))))
           (define-key 'notmuch-show-part-map "d" 'rde-notmuch-show-view-as-patch)

           (define-key notmuch-search-mode-map "w"
             (lambda ()
               (interactive)
               (notmuch-tag
                (concat
                 "id:" (car (notmuch-query-get-message-ids
                             (notmuch-search-find-thread-id))))
                (list "+watch"))
               (notmuch-tree-next-message)))
           (define-key notmuch-search-mode-map "d"
             (lambda ()
               (interactive)
               (notmuch-search-add-tag rde-notmuch-trash-tags)
               (notmuch-tree-next-message)))
           (define-key notmuch-search-mode-map "D"
             (lambda ()
               (interactive)
               (notmuch-search-add-tag rde-notmuch-delete-tags)
               (notmuch-tree-next-message)))
           (define-key notmuch-search-mode-map "T"
             (lambda ()
               (interactive)
               ;; TODO: Change it to only matched, not the whole thread
               (notmuch-search-add-tag rde-notmuch-todo-tags)
               (notmuch-tree-next-message)))

           (defun rde-notmuch-show-view-html-part ()
             "Open the text/html part of the current message using
`notmuch-show-view-part'."
             (interactive)
             (save-excursion
              (goto-char
               (prop-match-beginning
                (text-property-search-forward
                 :notmuch-part
                 "text/html"
                 (lambda (value notmuch-part)
                   (equal (plist-get notmuch-part :content-type) value)))))
              (notmuch-show-view-part)))
           (define-key notmuch-show-part-map "h" 'rde-notmuch-show-view-html-part)

           (mapcar
            (lambda (x)
              (add-to-list 'notmuch-show-stash-mlarchive-link-alist x))
            `(("yhetil" . "https://yhetil.org/")
              ("mail-archive" . "https://www.mail-archive.com/search?l=mid&q=")
              ("rde-devel" .
               (lambda (x)
                 (concat "https://lists.sr.ht/~abcdw/rde-devel/<" x ">")))))
           (setq notmuch-show-stash-mlarchive-link-default "yhetil")

           ,@(notmuch-redefined-functions config)

           (setq notmuch-search-result-format
                 '(("date" . "%12s ")
                   ("count" . "%-7s ")
                   ("authors" . "%-20s ")
                   ("subject" . "%-80s  ")
                   ("tags" . "(%s)")))
           (setq notmuch-tree-result-format
                 '(("date" . "%12s  ")
                   ("authors" . "%-20s")
                   ((("tree" . "%s")
                     ("subject" . "%s"))
                    . " %-88s ")
                   ("tags" . "(%s)")))
           (setq notmuch-unthreaded-result-format
                 '(("date" . "%12s  ")
                   ("authors" . "%-20s")
                   ((("subject" . "%s"))
                    . " %-88s ")
                   ("tags" . "(%s)")))

;;            (defun rde--notmuch-refresh-buffer (&optional _)
;;              "Wrapper around `notmuch-refresh-this-buffer', which accepts
;; optional arguments to use the function inside hook."
;;              (notmuch-refresh-this-buffer))

;;            (defun rde--add-notmuch-hello-hooks ()
;;              "Add mode local hooks for notmuch-hello buffers."
;;              (add-hook 'window-size-change-functions 'rde--notmuch-refresh-buffer))

;;            (add-hook 'notmuch-hello-mode-hook 'rde--add-notmuch-hello-hooks)

           (setq notmuch-show-logo nil)))
        #:summary "\
Tons of configurations and additions for notmuch email client"
        #:commentary "\
Set default MUA, adjust view, add auxiliary functions and keybindings."
        #:keywords '(convenience)
        #:elisp-packages
        (append
         (list (get-value 'emacs-cape config emacs-cape)
               emacs-notmuch emacs-ol-notmuch)
         (if (get-value 'emacs-consult config #f)
             (list emacs-consult-notmuch)
             '()))))))

  (feature
   (name f-name)
   (values `((notmuch . ,notmuch)
             (notmuch-queries . ,notmuch-queries)))
   (home-services-getter get-home-services)))


;;; Mail subsystem refactoring:

;; TODO: Add piem for apply emails and stuff

;; TODO: [Andrew Tropin, 2023-12-27] Add offline sending of emails
;; https://jao.io/blog/relaying-mail-to-multiple-smarthosts.html

;; observations on mail related features by Benoit Joly
;; https://lists.sr.ht/~abcdw/rde-devel/<878rh4byyg.fsf@benoitj.ca>


;;;
;;; Example of setting up custom email providers
;;;

(define (example-custom-provider-configuration)
  "This is an example configuration with a few helpers for interactive
exploration of resulting msmtp and isync configuration files produces by RDE's
email subsystem."

  ;; TODO: [Andrew Tropin, 2025-01-24] Move to (rde features) to make it a
  ;; public API accessible to more people.
  (define (get-configs rde-cfg)
    "Takes rde config and returns a list of configuration files produced by it."
    (service-value
     (fold-services
      ((@ (gnu home) home-environment-services)
       (rde-config-home-environment rde-cfg))
      #:target-type home-files-service-type)))

  (define custom-folder-mapping
    '(("inbox"   . "INBOX")
      ("sent"    . "Sent")
      ("drafts"  . "Drafts")
      ("archive" . "Archive")
      ("trash"   . "Trash")
      ("spam"    . "Spam")))

  (define custom-providers-settings
    `((example
       (smtp (host . "mail.example.com")
             (starttls? . #t))
       (imap (host . "imap.exmaple.com")
             (folder-mapping . ,custom-folder-mapping)))
      (example-no-starttls-with-generic-folder-mapping
       (smtp (host . "mail.example.com"))
       (imap (host . "imap.exmaple.com")
             (folder-mapping . ,mail-providers:generic-folder-mapping)))))

  (define tmp-config
    (rde-config
     (features (list
                ((@ (rde features base) feature-user-info)
                 #:user-name "bob"
                 #:full-name "Bob Anderson"
                 #:email "happy@example.com")
                (feature-mail-settings
                 #:mail-accounts (list
                                  (mail-account
                                   (id   'primary)
                                   (fqda "test@example.com")
                                   (type 'example)))
                 #:mail-providers-settings custom-providers-settings)

                ((@ (rde features xdg) feature-xdg))
                (feature-msmtp)
                (feature-isync
                 #:isync-serializers %default-isync-serializers)))))

  (define previous-config
    "defaults\ntls on\nauth on\nlogfile /home/bob/.local/var/lib/log/msmtp.log\n\naccount primary-test@example.com\nfrom test@example.com\nuser test@example.com\npasswordeval pass show mail/test@example.com\nhost mail.example.com\ntls_starttls on\n")

  (define msmtp+isync-configs-folder
    (car
     ((@ (rde api store) build)
      (file-union
       "msmtp+isync"
       `(("isync-config"
          ,(car (assoc-ref (get-configs tmp-config) ".config/isync/mbsyncrc")))
         ("msmtp-config"
          ,(car (assoc-ref (get-configs tmp-config) ".config/msmtp/config"))))))))

  (call-with-input-file
      (car
       ((@ (rde api store) build)
        (car (assoc-ref (get-configs tmp-config) ".config/msmtp/config"))))
    (lambda (port)
      (string=? previous-config
                ((@ (ice-9 textual-ports) get-string-all) port)))))
