(define-module (rde features tmux)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages tmux)
  #:use-module (guix gexp)

  #:export (feature-tmux))

(define* (feature-tmux
	  #:key
	  config-file
	  (package tmux))
  "Configure tmux."
  (ensure-pred maybe-file-like? config-file)
  (ensure-pred any-package? package)

  (define (tmux-home-services config)
    "Returns home services related to tmux."
    (list
     (simple-service
      'home-tmux-tmux-conf
      home-xdg-configuration-files-service-type
      (filter list?
              (list (when config-file
                      (list "tmux/tmux.conf" config-file)))))
     (simple-service
      'home-tmux-package
      home-profile-service-type
      (list package))))

  (feature
   (name 'tmux)
   (values `((tmux . #t)))
   (home-services-getter tmux-home-services)))
