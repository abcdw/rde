(define-module (rde features tmux)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services base)
  #:use-module (gnu services)
  #:use-module (gnu packages tmux)
  #:use-module (guix gexp)

  #:export (feature-tmux))


(define* (feature-tmux
	  #:key
	  config-file
	  (package tmux))
  "Configure tmux."
  (ensure-pred maybe-file-like? config-file)
  (ensure-pred package? package)

  (define (tmux-home-services config)
    "Returns home services related to tmux."
    ;; TODO: Implement home service and rewrite to it to make this
    ;; feature extendable.
    (list
     (home-generic-service
      'home-tmux
      #:files
      (filter list?
	      (list (when config-file
		      (list "config/tmux/tmux.conf" config-file))))
      #:packages (list package))))

  (feature
   (name 'tmux)
   (values `((tmux . #t)))
   (home-services-getter tmux-home-services)))
