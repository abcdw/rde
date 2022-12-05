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
          tmux-conf
          (tmux tmux))
  "Configure tmux."
  (ensure-pred maybe-file-like? tmux-conf)
  (ensure-pred file-like? tmux)

  (define (tmux-home-services config)
    "Returns home services related to tmux."
    (list
     (simple-service
      'home-tmux-tmux-conf
      home-xdg-configuration-files-service-type
      (filter list?
              (list (when tmux-conf
                      (list "tmux/tmux.conf" tmux-conf)))))
     (simple-service
      'home-tmux-package
      home-profile-service-type
      (list tmux))))

  (feature
   (name 'tmux)
   (values `((tmux . ,tmux)))
   (home-services-getter tmux-home-services)))
