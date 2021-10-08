(define-module (rde features xdg)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages freedesktop)

  #:export (feature-xdg)

  #:re-export (home-xdg-base-directories-configuration
	       home-xdg-user-directories-configuration))


(define* (feature-xdg
	  #:key
	  (xdg-base-directories-configuration
	   (home-xdg-base-directories-configuration))
	  (xdg-user-directories-configuration
	   (home-xdg-user-directories-configuration)))
  "Set XDG base (with a few extensions) and user directories.

Set the value to \"$HOME\" for any user directory if you don't need
it.  No other environment variables allowed in user directories."
  (ensure-pred home-xdg-base-directories-configuration?
	       xdg-base-directories-configuration)
  (ensure-pred home-xdg-user-directories-configuration?
	       xdg-user-directories-configuration)

  (define (xdg-home-services config)
    (list
     (simple-service
      'add-xdg-packages
      home-profile-service-type
      (list xdg-utils xdg-user-dirs desktop-file-utils))
     ;; This service always present in essential services, that is why
     ;; we need to extend it to override configuration.
     (simple-service
      'set-xdg-base-directories-values
      home-xdg-base-directories-service-type
      xdg-base-directories-configuration)
     (service
      home-xdg-user-directories-service-type
      xdg-user-directories-configuration)))

  (feature
   (name 'xdg)
   (home-services-getter xdg-home-services)
   (values
    (make-feature-values xdg-base-directories-configuration
			 xdg-user-directories-configuration))))
