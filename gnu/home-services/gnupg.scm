(define-module (gnu home-services gnupg)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu home-services files)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-26)

  #:export (home-gnupg-service-type
	    home-gnupg-configuration
            home-gpg-configuration
            home-gpg-agent-configuration))

;;; Commentary:
;;
;; Service for installing and configuring gpg and gpg-agent.
;;
;; (service home-gnupg-service-type
;;            (home-gnupg-configuration
;;             (gpg-config
;;              (home-gpg-configuration
;;               (extra-config
;;                '((cert-digest-algo . "SHA256")
;;                  (default-preference-list . ("SHA512"
;;                                              "SHA384"
;;                                              "SHA256"
;;                                              "SHA224"
;;                                              "AES256"
;;                                              "AES192"
;;                                              "Uncompressed"))
;;                  (with-fingerprint? . #t)))))
;;             (gpg-agent-config
;;              (home-gpg-agent-configuration
;;               (ssh-agent? #t)
;;               (pinentry-flavor 'emacs)
;;               (extra-options '("--verbose"))
;;               (extra-config
;;                '((max-cache-ttl . 86400)))))))
;;
;;; Code:

(define (serialize-field field-name val)
  (cond
   ((list? val) (serialize-list field-name val))
   ((boolean? val) (serialize-boolean field-name val))
   (else (format #f "~a ~a~%" field-name val))))

(define (serialize-list field-name lst)
  (serialize-field field-name (format #f "~a" (string-join lst))))

(define (serialize-boolean field-name boolean)
  (let* ((stringified (maybe-object->string field-name))
         (field-name (if (string-suffix? "?" stringified)
                         (string-drop-right stringified 1)
                         field-name)))
    (if boolean
        (serialize-field field-name "")
        (serialize-field "" ""))))

(define (serialize-alist field-name val)
  (generic-serialize-alist string-append serialize-field val))

(define serialize-string serialize-field)

(define-enum pinentry-flavor
  '(tty emacs gtk2 qt gnome3 rofi efl))

(define (serialize-pinentry-flavor field-name val)
  (let ((pinentry-program #~(string-append "pinentry-program "
                                           #$(file-append
                                              (specification->package
                                               (format #f "pinentry-~a" val))
                                              "/bin/pinentry")
                                           "\n")))
    (if (equal? val 'emacs)
        #~(string-append #$pinentry-program
                         "allow-emacs-pinentry\n"
                         "allow-loopback-pinentry\n")
        pinentry-program)))

(define ssh-agent? boolean?)
(define (serialize-ssh-agent field-name val)
  (serialize-field "enable-ssh-support" val))

(define (ssh-key? lst)
  (let ((keygrip (first lst)))
    (if (= (string-length keygrip) 40)
        #t
        (raise (formatted-message
                (G_ "The keygrip of the GnuPG key must be of length 40, was given: ~s")
                keygrip)))))

(define (ssh-keys-list? lst)
  (when (every ssh-key? lst) #t))

(define (serialize-ssh-key lst)
  (string-append (string-join (map maybe-object->string lst)) "\n"))

(define (serialize-ssh-keys-list field-name val)
  (apply string-append (map serialize-ssh-key
                            val)))

;; Dummy procedures, the real logic is handled in `home-gnupg-files-service'.
(define (serialize-home-gpg-configuration field-name val) "")
(define (serialize-home-gpg-agent-configuration field-name val) "")
(define (serialize-extra-options field-name val) "")
(define extra-options? list?)

(define-configuration home-gpg-configuration
  (extra-config
   (alist '())
   "Association list of key-value pair configuration.  The following configuration:
@lisp
(extra-config
  '((cert-digest-algo . \"SHA256\")
    (no-comments . #f)
    (default-preference-list . (\"SHA512\" \"SHA384\"))))
@end lisp

would yield:

@example
cert-digest-algo SHA256
no-comments
default-preference-list SHA512 SHA384
@end example")
  (extra-content
   (string-or-gexp "")
   "Extra content for the @code{gpg.conf} file, useful if you already
have a configuration for gpg."))

(define-configuration home-gpg-agent-configuration
  (ssh-agent?
   (ssh-agent #f)
   "Whether to use gpg-agent for SSH keys.  If you enable this option,
you should also look at the @code{ssh-keys} option.")
  (ssh-keys
   (ssh-keys-list '())
   "List of lists of GnuPG keys to expose as SSH keys.  The list
contains sublists corresponding to a GnuPG key, the sublist can
contain three elements.  The first one is the keygrip of the key, the
second is the caching TTL in seconds, the third is a flag to give to
the key, currently, only ``confirm'' is supported.  The keygrip is
mandatory, the other two elements are optional
(@pxref{Configuration,,,gpg-agent,gpg-agent}).  The following
snippet:

@lisp
(ssh-keys
  '((\"34B62F25E277CF13D3C6BCEBFD3F85D08F0A864B\" 0 \"confirm\")
    (\"4B62F25E277CF13D3C6BCEBFD3F85D08F0DA32VD\")))
@end lisp

yields the following in @file{sshcontrol}:

@example
34B62F25E277CF13D3C6BCEBFD3F85D08F0A864B 0 confirm
4B62F25E277CF13D3C6BCEBFD3F85D08F0DA32VD
@end example")
  (pinentry-flavor
   (pinentry-flavor 'gtk2)
   (string-append "Which pinentry interface to use.  Valid options are: "
                  (list->human-readable-list
                   (enum-value pinentry-flavor)
                   #:cumulative? #t
                   #:proc (cut format #f "``~a''" <>))))
  (extra-options
   (extra-options '())
   "Extra CLI options to give to @command{gpg-agent}.")
  (extra-config
   (alist '())
   "Association list of key-value pair configuration, works the same
way as the @code{extra-config} field for
@code{home-gpg-configuration}.  The following configuration:

@lisp
(extra-config
  '((default-cache-ttl . 80000)
    (pinentry-invisible-char \"@@\")))
@end lisp

would yield:

@example
default-cache-ttl 80000
pinentry-invisible-char @@
@end example")
  (extra-content
   (string-or-gexp "")
   "Extra content for the @code{gpg-agent.conf} file, useful if you already
have a configuration for gpg-agent."))

;; TODO: Add homedir option?
(define-configuration home-gnupg-configuration
  (package
    (package gnupg)
    "GnuPG package to use.")
  (gpg-config
   (home-gpg-configuration (home-gpg-configuration))
   "Configuration for the @code{gpg} executable")
  (gpg-agent-config
   (home-gpg-agent-configuration (home-gpg-agent-configuration))
   "Configuration for the @code{gpg-agent}"))

(define (home-gnupg-environment-variables-service config)
  "Add SSH_AUTH_SOCK variable to user's environment."
  (if (home-gpg-agent-configuration-ssh-agent?
       (home-gnupg-configuration-gpg-agent-config config))
      `(("SSH_AUTH_SOCK" .
	 ,#~(string-append
	     "$("
	     #$(file-append gnupg "/bin/gpgconf")
	     " --list-dirs agent-ssh-socket)")))
      '()))

(define (home-gpg-agent-file config)
  (mixed-text-file
       "gpg-agent.conf"
       (serialize-configuration
        (home-gnupg-configuration-gpg-agent-config config)
        (filter-configuration-fields home-gpg-agent-configuration-fields
                                     '(ssh-keys)
                                     #t))
       (home-gpg-agent-configuration-extra-content
        (home-gnupg-configuration-gpg-agent-config config))))

(define (home-gpg-sshcontrol-file config)
  (mixed-text-file
         "sshcontrol"
         (serialize-configuration
          (home-gnupg-configuration-gpg-agent-config config)
          (filter-configuration-fields home-gpg-agent-configuration-fields
                                       '(ssh-keys)))))

(define (home-gpg-file config)
  (mixed-text-file
       "gpg.conf"
       (serialize-configuration
        (home-gnupg-configuration-gpg-config config)
        home-gpg-configuration-fields)
       (home-gpg-configuration-extra-content
        (home-gnupg-configuration-gpg-config config))))

(define (home-gnupg-files-service config)
  ;; Don't create file if empty
  (filter (compose not null?)
          ;; TODO: Pass directly to gpg-agent to avoid symlink?
          `(("gnupg/gpg-agent.conf"
             ,(home-gpg-agent-file config))
            ,(if (null? (home-gpg-agent-configuration-ssh-keys
                         (home-gnupg-configuration-gpg-agent-config config)))
                 '()
                 '("gnupg/sshcontrol"
                   (home-gpg-sshcontrol-file config)))
            ("gnupg/gpg.conf"
             ,(home-gpg-file config)))))

(define (home-gnupg-shepherd-service config)
  (let ((provision-list `(gpg-agent
                          ,@(if (home-gpg-agent-configuration-ssh-agent?
                                 (home-gnupg-configuration-gpg-agent-config config))
                                '(ssh-agent) '()))))
    (list
     (shepherd-service
      (documentation "Run and control gpg-agent.")
      (provision provision-list)
      ;; TODO: Use --supervised gpg-agent and sockets
      ;; (start #~(make-forkexec-constructor
      ;;           (append (list #$(file-append gnupg "/bin/gpg-agent")
      ;;                         ;; "--supervised"
      ;;                         "--options"
      ;;                         #$(home-gpg-agent-file config))
      ;; (quote #$(home-gpg-agent-configuration-extra-options
      ;;         (home-gnupg-configuration-gpg-agent-config config))))))
      ;;
      ;; FIXME: any command using gpg hangs up and waiting for
      ;; something if updatestartuptty wasn't executed. Necessary to
      ;; restart gpg-agent.  With gpgconf and without updatestartuptty
      ;; it just throws: "agent refused operation".
      (start #~(make-system-constructor
                (string-join
                 (append
                  (list #$(file-append gnupg "/bin/gpg-agent")
			"--daemon"
			"--options"
                        #$(home-gpg-agent-file config))
                  (quote #$(home-gpg-agent-configuration-extra-options
                            (home-gnupg-configuration-gpg-agent-config config)))))))
      ;; (stop #~(make-kill-destructor))
      (stop #~(make-system-destructor "gpgconf --kill gpg-agent"))))))

(define (home-gnupg-profile-service config)
  (list (home-gnupg-configuration-package config)))

(define (home-gnupg-run-on-reconfigure-service config)
  #~(let ((gnupg-path (or (getenv "GNUPGHOME")
			 (string-append (getenv "HOME") "/.gnupg"))))
      ;; Prevent WARNING: unsafe permissions on homedir
      (chmod gnupg-path #o700)
      (display "Updating startup tty for gpg...")
      (system "gpg-connect-agent updatestartuptty /bye > /dev/null")
      (display " done\n")))

(define home-gnupg-service-type
  (service-type (name 'home-gnupg)
                (extensions
                 (list (service-extension
                        home-run-on-reconfigure-service-type
                        home-gnupg-run-on-reconfigure-service)
		       (service-extension
                        home-environment-variables-service-type
                        home-gnupg-environment-variables-service)
                       (service-extension
                        home-shepherd-service-type
                        home-gnupg-shepherd-service)
                       (service-extension
                        home-files-service-type
                        home-gnupg-files-service)
                       (service-extension
                        home-profile-service-type
                        home-gnupg-profile-service)))
                (default-value (home-gnupg-configuration))
                (description "Install and configure gpg and gpg-agent.")))

(define (generate-home-gnupg-documentation)
  (generate-documentation
   `((home-gnupg-configuration
      ,home-gnupg-configuration-fields
      (gpg-config home-gpg-configuration)
      (gpg-agent-config home-gpg-agent-configuration))
     (home-gpg-configuration
      ,home-gpg-configuration-fields)
     (home-gpg-agent-configuration
      ,home-gpg-agent-configuration-fields))
   'home-gnupg-configuration))

;; TODO: Add stuff to bash/zsh/fish config
;;   GPG_TTY=$(tty)
;;   export GPG_TTY
;;   gpg-connect-agent updatestartuptty /bye > /dev/null
