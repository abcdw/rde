(define-module (rde features mail)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (gnu packages mail)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services mail)

  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)

  #:export (feature-mail-settings
            feature-isync
            feature-notmuch

            mail-account
            mail-account-id
            mail-account-type
            mail-account-user
            mail-account-synchronizer
            mail-account-get-pass-cmd))


(define-configuration/no-serialization mail-account
  (id
   (symbol #f)
   "Something short to identify and distinguish a mail account.
@code{'work} or @code{'personal} for example.")
  ;; TODO: Add aliases?
  (type
   (symbol 'generic)
   "Type of the mail account.  Will be used in different serialization
scenarios, during generation of @file{mbsyncrc} for example.")
  (synchronizer
   (symbol 'isync)
   "Type of application to obtain emails.")
  (user
   (string #f)
   "Email. @code{\"someone@example.com\"} for example.")
  (pass-cmd
   (maybe-string #f)
   "Command returning a password.  If value not provided @code{pass show mail/$user} will be used, where @code{$user} is a value of user field."))

(define (mail-account-get-pass-cmd mail-account)
  (if (mail-account-pass-cmd mail-account)
      (mail-account-pass-cmd mail-account)
      (string-append "pass show mail/" (mail-account-user mail-account))))

(define (list-of-mail-accounts? lst)
  (and (list? lst) (not (null? lst)) (any mail-account? lst)))

(define (default-mail-directory-fn config)
  (string-append (get-value 'home-directory config)
                 ;; TODO: Uses XDG_STATE_HOME
                 ;; (get-value 'xdg-blablabla config)
                 "/mail"))

(define* (feature-mail-settings
          #:key
          (mail-accounts #f)
          (mail-directory-fn default-mail-directory-fn))
  "Provide mail-accounts and mail-directory-fn for other mail-related
features."
  (ensure-pred list-of-mail-accounts? mail-accounts)
  (ensure-pred procedure? mail-directory-fn)

  (feature
   (name 'mail-settings)
   (values `((mail-settings . #t)
             (mail-accounts . ,mail-accounts)
             (mail-directory-fn . ,mail-directory-fn)))))


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

(define (generate-isync-serializer host folders-mapping)
  (define (isync-settings mail-directory mail-account)
    (let* ((id       (mail-account-id mail-account))
           (user     (mail-account-user mail-account))
           (pass-cmd (mail-account-get-pass-cmd mail-account)))
      `(,#~(string-append "# Account '" #$(symbol->string id)
                          " starts here")
        (IMAPAccount ,id)
        (Host ,host)
        (User ,user)
        (PassCmd ,pass-cmd)
        (SSLType IMAPS)
        ,#~""
        (IMAPStore ,(symbol-append id '-remote))
        (Account ,id)
        ,#~""
        (MaildirStore ,(symbol-append id '-local))
        (Subfolders Verbatim)
        (Path ,(string-append mail-directory "/" user "/"))
        (Inbox ,(string-append mail-directory "/" user "/inbox"))
        ,#~""
        ,@(isync-group-with-channels id folders-mapping))))
  isync-settings)

;; Directory names has lowercased spelling to match notmuch tags
(define gmail-folder-mapping
  '(("inbox"  . "INBOX")
    ("sent"   . "[Gmail]/Sent Mail")
    ("drafts" . "[Gmail]/Drafts")
    ("all"    . "[Gmail]/All Mail")
    ("trash"  . "[Gmail]/Trash")
    ("spam"   . "[Gmail]/Spam")))

(define gmail-isync-settings
  (generate-isync-serializer "imap.gmail.com" gmail-folder-mapping))

(define (generic-isync-settings mail-directory mail-account)
  `(,#~"# Do not know how to serialize generic accounts :("))

(define %default-isync-serializers
  `((gmail . ,gmail-isync-settings)
    (generic . ,generic-isync-settings)))

(define default-isync-global-settings
  `((Create Both)
    (Expunge Both)
    (SyncState *)
    ,#~""))

(define* (feature-isync
          #:key
          (mail-account-ids #f)
          (isync-global-settings default-isync-global-settings)
          (isync-serializers %default-isync-serializers)
          (isync-verbose #f))
  "Setup and configure isync.  If MAIL-ACCOUNT-IDS not provided use all
mail accounts.  ISYNC-VERBOSE controls output verboseness of
@file{mbsync}."
  (ensure-pred maybe-list? mail-account-ids)
  (ensure-pred list? isync-serializers)
  (ensure-pred list? isync-global-settings)

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
           (mail-directory    (mail-directory-fn config)))

      (define (serialize-mail-acc mail-acc)
        ((assoc-ref isync-serializers (mail-account-type mail-acc))
         mail-directory mail-acc))

      (list
       (service
        home-isync-service-type
        (home-isync-configuration
         (config
          (append
           isync-global-settings
           (append-map serialize-mail-acc mail-accounts))))))))

  ;; MAYBE: Wrap it in a program-file to make it possible to call it
  ;; with system*
  (define (isync-synchronize-cmd-fn mail-acc)
    (string-append "mbsync "
                   (if isync-verbose "-V " "")
                   (symbol->string (mail-account-id mail-acc))))

  (feature
   (name 'isync)
   (values `((isync . #t)
             (isync-synchronize-cmd-fn . ,isync-synchronize-cmd-fn)))
   (home-services-getter get-home-services)))


(define (default-get-notmuch-configuration config)
  (require-value 'mail-settings config)
  (require-value 'full-name config)
  (define full-name (get-value 'full-name config))
  (define mail-accounts (get-value 'mail-accounts config))
  (define emails (map mail-account-user mail-accounts))
  (define ids    (map mail-account-id   mail-accounts))

  (define mail-directory ((get-value 'mail-directory-fn config) config))
  (define (mail-dir email) (string-append mail-directory "/" email))
  (define mail-directories (map mail-dir emails))

  (define (sync-cmd ma)
    ((get-value
      (symbol-append (mail-account-synchronizer ma) '-synchronize-cmd-fn)
      config)
     ma))

  (define sync-cmds (map sync-cmd mail-accounts))

  (define make-id-tag
    (map (lambda (x)
           (format
            #f "notmuch tag +~a -- path:~a/** and tag:new"
            (mail-account-id x) (mail-account-user x)))
         mail-accounts))

  (define tag-updates-post
    '("notmuch tag +inbox -- path:/.*\\/inbox/"
      "notmuch tag +draft -- path:/.*\\/drafts/"
      "notmuch tag +sent  -- path:/.*\\/sent/"
      "notmuch tag +trash -- path:/.*\\/trash/"
      "notmuch tag +spam  -- path:/.*\\/spam/"
      ;; If file was moved out of folder on server remove respective tag
      "notmuch tag -inbox -- not path:/.*\\/inbox/ and tag:inbox"
      "notmuch tag -trash -- not path:/.*\\/trash/ and tag:trash"
      "notmuch tag -spam  -- not path:/.*\\/spam/  and tag:spam"

      "notmuch tag -new -- tag:new"))

  ;; (define tag-updates-post-insert
  ;;   "notmuch tag +sent -new -- path:/.*\\/sent/ and tag:new")

  (define (move-out-untagged-messages tag)
    "If tag was removed -> move out of the related folder."
    (format #f "for f in $(notmuch search --output=files \
'path:/.*\\/~a/ and not tag:~a' | grep '/~a/'); \
do mv -v $f $(echo $f | sed 's;/~a/;/all/;' | sed 's/,U=[0-9]*:/:/'); done"
            tag tag tag tag))

  (define* (move-in-tagged-messages
            tag
            #:optional (exclude-dir "nothing-will-match-this"))
    (format #f "for f in $(notmuch search --output=files \
'not path:/.*\\/~a/ and tag:~a' | grep -v \"/~a/\"); \
do mv -v $f $(echo $f | sed 's;/[[:alnum:]]*/cur/;/~a/cur/;' | sed 's/,U=[0-9]*:/:/'); done"
            tag tag exclude-dir tag))

  (define move-rules
    (append
     (map move-out-untagged-messages '(inbox trash spam))
     (map move-in-tagged-messages '(trash spam))
     (list (move-in-tagged-messages 'inbox "all"))))

  (home-notmuch-extension
   (pre-new
    (list
     (with-imported-modules '((guix build utils))
       #~(begin
           (map (@@ (guix build utils) mkdir-p) '#$mail-directories)
           (map system '#$move-rules)
           (map system '#$sync-cmds)))))
   (post-new
    (list
     #~(begin (map system '#$make-id-tag)
              (for-each system '#$tag-updates-post))))
   (config
    `((user ((name . ,full-name)
             (primary_email . ,(car emails))
             (other_email . ,(cdr emails))))
      (database ((path . ,mail-directory)
                 (mail_root . ,mail-directory)))
      (maildir ((synchronize_flags . true)))
      (search ((exclude_tags . (trash spam))))
      (new ((tags . new)
            (ignore . (.mbsyncstate .uidvalidity))))))))

(define* (feature-notmuch
          #:key (get-notmuch-configuration default-get-notmuch-configuration))
  "Configure notmuch and Emacs UI for it if emacs enabled."
  (ensure-pred procedure? get-notmuch-configuration)

  (define f-name 'notmuch)

  (define (get-home-services config)
    (define emails (map mail-account-user (get-value 'mail-accounts config)))

    (define fcc-dirs
      (map (lambda (x) (cons x (string-append x "/sent"))) emails))

    (list
     (simple-service
      'test-ext
      home-notmuch-service-type
      (get-notmuch-configuration config))
     (when (get-value 'emacs config)
       (elisp-configuration-service
        f-name
        ;; https://github.com/SeTSeR/nixos-config/blob/master/modules/users/smakarov/emacs/modules/mail.el
        ;; https://protesilaos.com/dotemacs/#h:a196812e-1644-4536-84ba-687366867def
        ;; TODO: Try pipe message to git am
        `((define-key global-map (kbd "C-c a n") 'notmuch)
          (with-eval-after-load
           'notmuch
           (setq notmuch-fcc-dirs ',fcc-dirs)
           (setq notmuch-tree-result-format
                 '(("date" . "%12s  ")
                   ("authors" . "%-20s")
                   ((("tree" . "%s")
                     ("subject" . "%s"))
                    . " %-80s ")
                   ("tags" . "(%s)")))
           (setq notmuch-tagging-keys
                 '(("a" notmuch-archive-tags "Archive")
                   ("u" notmuch-show-mark-read-tags "Mark read")
                   ("f" ("+flagged") "Flag")
                   ("s" ("+spam" "-inbox") "Mark as spam")
                   ("d" ("+trash" "-inbox") "Delete (trash)")))

           ;; (advice-remove 'notmuch-tree-insert-tree #'rde-notmuch-tree-insert-tree)
           ;; Remove leading arrows for mails without threads
           (defun rde-notmuch-tree-insert-tree (orig-f tree depth tree-status first last)
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


           (advice-add 'notmuch-tree-insert-tree :around 'rde-notmuch-tree-insert-tree)

           (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
           (setq-default notmuch-search-oldest-first nil)
           (setq notmuch-show-logo nil)))
        #:elisp-packages (list notmuch)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: Add feature-msmtp
;; Think about delayed email sending
