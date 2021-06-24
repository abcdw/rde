(define-module (rde features version-control)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu services)

  #:export (feature-git))

(define* (feature-git
	  #:key
	  (sign-commits? #t)
	  (git-gpg-sign-key #f))
  "Setup and configure Git."
  (ensure-pred maybe-string? git-gpg-sign-key)
  (ensure-pred boolean? sign-commits?)

  (define (git-home-services config)
    "Returns home services related to Git."
    (require-value 'full-name config)
    (require-value 'email config)
    (require-value 'ssh config)

    (let ((gpg-sign-key (or git-gpg-sign-key
			    (get-value 'gpg-primary-key config))))
      (when sign-commits?
	(ensure-pred string? gpg-sign-key))
      (list
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
	     ((annotate . #t))))))))))

  (feature
   (name 'git)
   (values '((git . #t)))
   (home-services-getter git-home-services)))
