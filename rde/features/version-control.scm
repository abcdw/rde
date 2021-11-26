(define-module (rde features version-control)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde packages)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)

  #:export (feature-git))

(define* (feature-git
	  #:key
	  (sign-commits? #t)
	  (git-gpg-sign-key #f)
          (send-email? #f)
          (extra-config '()))
  "Setup and configure Git."
  (ensure-pred maybe-string? git-gpg-sign-key)
  (ensure-pred boolean? sign-commits?)
  (ensure-pred boolean? send-email?)
  (ensure-pred list? extra-config)

  (define (git-home-services config)
    "Returns home services related to Git."
    (require-value 'full-name config)
    (require-value 'email config)
    (when send-email?
      (require-value 'msmtp config
                     "The msmtp feature has to be enabled to configure git-sendemail."))

    (let ((gpg-sign-key (or git-gpg-sign-key
			    (get-value 'gpg-primary-key config))))
      (when sign-commits?
	(ensure-pred string? gpg-sign-key))
      (list
       (when send-email?
         (simple-service
          'git-send-email-package
          home-profile-service-type
          (list (list git "send-email"))))
       (service
	home-git-service-type
	(home-git-configuration
         (ignore
	  '("*~"
	    "*.\\#\\*"
	    "*.\\#*\\#"))
	 (config
	  `((user
	     ((name . ,(get-value 'full-name config))
	      (email . ,(get-value 'email config))
	      ,@(if sign-commits?
		    `((signingkey . ,gpg-sign-key))
		    '())))
	    (commit
	     (,@(if sign-commits?
		    '((gpgsign . #t))
		    '())))
	    (sendemail
	     ((annotate . #t)
              ,@(if send-email?
                    `((smtpserver . ,(file-append
                                      (get-value 'msmtp config)
                                      "/bin/msmtp")))
                    '())))

            ,@extra-config)))))))

  (feature
   (name 'git)
   (values '((git . #t)))
   (home-services-getter git-home-services)))
