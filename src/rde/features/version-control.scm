(define-module (rde features version-control)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde packages)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:export (feature-git))

(define (ssh:ssh-key? key)
  (not (null? (filter (lambda (s) (string-prefix? s key))
                      '("ssh-rsa"
                        "ssh-dss"
                        "ecdsa-sha2-nistp256"
                        "ssh-ed25519"
                        "sk-ssh-ed25519@openssh.com"
                        "sk-ecdsa-sha2-nistp256@openssh.com")))))

;; This comment can be more related to states:
;; per-project configurations can be included with includeIf
;; directive. (inside global config? or during repo initialization?)
(define* (feature-git
          #:key
          (git git)
          (sign-commits? #t)
          (git-sign-key #f)
          (git-send-email? #t)
          (extra-config '()))
  "Setup and configure Git."
  (ensure-pred any-package? git)
  (ensure-pred maybe-string? git-sign-key)
  (ensure-pred boolean? sign-commits?)
  (ensure-pred boolean? git-send-email?)
  (ensure-pred list? extra-config)

  (define (git-home-services config)
    "Returns home services related to Git."
    (require-value 'full-name config)
    (require-value 'email config)

    (let* ((gpg-primary-key (get-value 'gpg-primary-key config))
           (git-sign-key (or git-sign-key gpg-primary-key))
           (ssh-key? (and (string? git-sign-key) (ssh:ssh-key? git-sign-key)))
           (sign-key (if ssh-key?
                         (string-append "key::" git-sign-key)
                         git-sign-key)))
      (when sign-commits?
        (unless (string? sign-key)
          (raise (formatted-message
                  (G_ "Ensure that correct `git-sign-key' or `gpg-primary-key' \
is provided or disable `sign-commits?' Current sign-key value is ~a")
                  sign-key))))
      (list
       (when git-send-email?
         (simple-service
          'git-send-email-package
          home-profile-service-type
          (list (list git "send-email"))))
       (service
        home-git-service-type
        (home-git-configuration
         (package git)
         (ignore
          '("*~"
            "*.\\#\\*"
            "*.\\#*\\#"))
         (config
          `((core
             (,@(if (get-value 'emacs-client config)
                    `((editor . ,(file-append
                                  (get-value 'emacs-client config)
                                  " --reuse-frame")))
                    '())))
            (user
             ((name . ,(get-value 'full-name config))
              (email . ,(get-value 'email config))
              ,@(if sign-commits?
                    `((signingkey . ,sign-key))
                    '())))
            (merge
             ;; diff3 makes it easier to solve conflicts with smerge, zdiff3
             ;; should make a conflict scope smaller, but guile-git fails if
             ;; this option is set.
             ((conflictStyle . diff3)))
            ;; TODO: [Andrew Tropin, 2024-05-06] Add better defaults for
            ;; rebase workflow

            ;; TODO: [Andrew Tropin, 2024-05-06] Update manual related to git
            ;; feature

            ;; https://youtu.be/Md44rcw13k4?t=1081
            ;; rebase.updateRefs true
            ;; https://andrewlock.net/working-with-stacked-branches-in-git-is-easier-with-update-refs/

            ;; (rebase)
            (diff
             ;; histogram should be smarter about diff generation.
             ((algorithm . histogram)))
            (commit
             (,@(if sign-commits?
                    '((gpgsign . #t))
                    '())))
            (sendemail
             ((annotate . #t)))

            ,@(if (and sign-commits? ssh-key?)
                  `((gpg ((format . ssh))))
                  '())

            ,@extra-config)))))))

  (feature
   (name 'git)
   (values (make-feature-values git git-send-email?))
   (home-services-getter git-home-services)))
