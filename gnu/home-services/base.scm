(define-module (gnu home-services base)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
  #:use-module (gnu services)
  #:use-module (ice-9 match)

  #:export (home-generic-service)
  #:re-export (simple-service))


(define* (home-generic-service
	  name
	  #:key
	  (files '())
	  (packages '())
	  (extensions '()))
  "Creates a service which extends home-profile with PACKAGES and
home-files with FILES.  EXTENSIONS is an alist of pairs @code{(target
. function)}."
  (let* ((profile-extension (service-extension
			     home-profile-service-type
			     (const packages)))
	 (files-extension   (service-extension
			     home-files-service-type
			     (const files)))
	 (more-extensions   (map
			     (match-lambda
			       ((target . value)
				(service-extension
				 target
				 (const value))))
			     extensions))
	 (type              (service-type
			     (name name)
			     (extensions
			      (append
			       more-extensions
			       (list profile-extension
				     files-extension))))))
    (service type #f)))
