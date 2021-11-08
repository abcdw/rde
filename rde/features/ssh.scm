(define-module (rde features ssh)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu services)

  #:export (feature-ssh)

  #:re-export (home-ssh-configuration
	       ssh-host
	       ssh-match))


(define* (feature-ssh
	  #:key
	  (ssh-configuration (home-ssh-configuration))
          (extra-config '()))
  "Setup and configure SSH."
  (ensure-pred home-ssh-configuration? ssh-configuration)
  (ensure-pred list-of-ssh-host-or-ssh-match? extra-config)

  (define home-ssh-service-extra-config
    (when (not (null? extra-config))
      (simple-service 'home-ssh-service-extra-config
                      home-ssh-service-type
                      extra-config)))

  (define (ssh-home-services config)
    "Returns home services related to SSH."
    (list (service home-ssh-service-type
		   ssh-configuration)
          home-ssh-service-extra-config))

  (feature
   (name 'ssh)
   (values '((ssh . #t)))
   (home-services-getter ssh-home-services)))
