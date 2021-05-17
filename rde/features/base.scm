(define-module (rde features base)
  #:use-module (rde features)

  #:export (feature-user-info))

(define* (feature-user-info
	  #:key user-name full-name email
	  (home-directory (format #f "/home/~a" user-name)))
  "Provides basic information about user for all features."
  (ensure-pred string? user-name)
  (ensure-pred string? full-name)
  (ensure-pred string? email)
  (ensure-pred string? home-directory)

  (feature
   (name 'user-info)
   (values (make-feature-values
	    user-name full-name email home-directory))))
