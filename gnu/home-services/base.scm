(define-module (gnu home-services base)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
  #:use-module (gnu services)
  #:export (home-generic-service)
  #:re-export (simple-service))


(define* (home-generic-service name #:key (files '()) (packages '()))
  "Creates a service which extends home-profile with PACKAGES and
home-files with FILES."
  (let* ((profile-extension (service-extension
			     home-profile-service-type
			     (const packages)))
	 (files-extension   (service-extension
			     home-files-service-type
			     (const files)))
         (type              (service-type
			     (name name)
			     (extensions
			      (list profile-extension
				    files-extension)))))
    (service type #f)))
