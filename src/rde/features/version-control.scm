(define-module (rde features version-control)
  #:use-module (rde features)
  #:use-module (rde predicates)
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
          (require-signed-commits-on-push? #t)
          (extra-config '()))
  "Setup and configure Git.

When @code{require-signed-commits-on-push?} is @code{#t} (the default),
a pre-push hook is installed that rejects pushes containing unsigned
commits.  To opt out for a specific repository run:

  git config rde.requireSignedCommitsOnPush false"
  (ensure-pred file-like? git)
  (ensure-pred maybe-string? git-sign-key)
  (ensure-pred boolean? sign-commits?)
  (ensure-pred boolean? git-send-email?)
  (ensure-pred boolean? require-signed-commits-on-push?)
  (ensure-pred list? extra-config)

  (define (git-home-services config)
    "Returns home services related to Git."

    (define pre-push-hook-text
      (plain-file
       "pre-push-text"
       "\
#!/bin/sh
# Verify all pushed commits are GPG-signed.
# Disable per-repository with:
#   git config rde.requireSignedCommitsOnPush false

if git config --bool rde.requireSignedCommitsOnPush 2>/dev/null \
   | grep -q false; then
  exit 0
fi

zero=$(git hash-object --stdin </dev/null | tr '[0-9a-f]' '0')
rc=0

while read local_ref local_oid remote_ref remote_oid; do
  if test \"$local_oid\" = \"$zero\"; then
    continue
  fi
  if test \"$remote_oid\" = \"$zero\"; then
    range=\"$local_oid\"
  else
    range=\"$remote_oid..$local_oid\"
  fi
  for commit in $(git rev-list \"$range\"); do
    if ! git verify-commit \"$commit\" >/dev/null 2>&1; then
      echo >&2 \"error: unsigned commit $commit (branch $local_ref)\"
      rc=1
    fi
  done
done

if test \"$rc\" -ne 0; then
  echo >&2 \"\"
  echo >&2 \"tip: to disable this check for this repository run:\"
  echo >&2 \"  git config rde.requireSignedCommitsOnPush false\"
fi

exit $rc
"))

    (define pre-push-hook
      (computed-file
       "pre-push"
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils))
             (copy-file #$pre-push-hook-text #$output)
             (chmod #$output #o755)))))

    (let* ((gpg-primary-key (get-value 'gpg-primary-key config #f))
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
             (,@(if (get-value 'emacs-client config #f)
                    `((editor . ,(file-append
                                  (get-value 'emacs-client config)
                                  " --reuse-frame")))
                    '())
              (hooksPath . "~/.config/git/hooks")))
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

            ;; rebase.autoStash true
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
             ((annotate . #t)
              ,@(if (get-value 'rde-advanced-user? config)
                    '((confirm . auto))
                    '())))

            ,@(if (and sign-commits? ssh-key?)
                  `((gpg ((format . ssh))))
                  '())

            ,@extra-config))))

       (when require-signed-commits-on-push?
         (simple-service
          'git-require-signed-commits-on-push
          home-xdg-configuration-files-service-type
          `(("git/hooks/pre-push" ,pre-push-hook)))))))

  (feature
   (name 'git)
   (values (make-feature-values git git-send-email?))
   (home-services-getter git-home-services)))
