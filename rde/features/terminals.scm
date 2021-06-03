(define-module (rde features terminals)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services base)
  #:use-module (gnu services)
  #:use-module (gnu packages terminals)
  #:use-module (guix gexp)

  #:export (feature-alacritty))

(define* (feature-alacritty
	  #:key
	  config-file)
  "Configure Alacritty terminal."
  (ensure-pred maybe-file-like? config-file)

  ;; TODO: Implement home service and rewrite to it to make this
  ;; feature extendable.
  (define (alacritty-home-services config)
    "Returns home services related to Alacritty."
    (list
     (home-generic-service
      'home-alacritty
      #:files
      (filter list?
	      (list (when config-file
		      (list "config/alacritty/alacritty.yml" config-file))))
      #:packages (list alacritty))))

  (feature
   (name 'alacritty)
   (values `((terminal . #t) (alacritty . #t)))
   (home-services-getter alacritty-home-services)))
