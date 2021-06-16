(define-module (rde features mail)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services mail)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)

  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)

  #:export (feature-mail-settings
            feature-isync

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


;; TODO: Add feature-msmtp
;; Think about delayed email sending
