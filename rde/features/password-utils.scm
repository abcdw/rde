(define-module (rde features password-utils)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services state)
  #:use-module (gnu services)

  #:export (feature-password-store))


(define* (feature-password-store
	  #:key
	  (remote-password-store-url #f))
  "Setup and configure password manager."
  ;; (ensure-pred maybe-url? remote-password-store-url)

  (define (password-store-home-services config)
    "Returns home services related to password-store."
    (require-value 'gpg-primary-key config)
    (require-value 'home-directory config)
    (list (service home-password-store-service-type)
	  (simple-service
	   'add-password-store-git-state
	   home-state-service-type
	   (list
	    (state-git
	    ;;; TODO: Rewrite it to xdg-state-home or rework states.
	     (string-append
	      (get-value 'home-directory config)
	      "/.local/var/lib/password-store")
	     remote-password-store-url)))))

  (feature
   (name 'password-store)
   (values '((pass . #t)
	     (password-store . #t)))
   (home-services-getter password-store-home-services)))
