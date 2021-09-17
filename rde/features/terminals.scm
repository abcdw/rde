(define-module (rde features terminals)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features fontutils)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services terminals)
  #:use-module (gnu services)
  #:use-module (gnu packages terminals)
  #:use-module (guix gexp)

  #:export (feature-alacritty))

(define* (feature-alacritty
	  #:key
	  config-file
	  (package alacritty)
          (default-terminal? #t))
  "Configure Alacritty terminal."
  (ensure-pred maybe-file-like? config-file)
  (ensure-pred package? package)

  ;; TODO: Implement home service and rewrite to it to make this
  ;; feature extendable.
  (define (alacritty-home-services config)
    "Returns home services related to Alacritty."
    (define font-mono (get-value 'font-monospace config))
    (list
     (service
      home-alacritty-service-type
      (home-alacritty-configuration
       (package package)
       (config
        `((window . ((padding . ((x . 10)
                                 (y . 5)))))
          ,@(if font-mono
                `((font . ((normal . ((style . Semilight)
                                      (family . ,(font-name font-mono))))
                        (size . ,(font-size font-mono)))))
              '())
          ,@(if config-file
                `((import . #(,config-file)))
                '())))))))

  (feature
   (name 'alacritty)
   (values `((alacritty . ,package)
             ,@(when default-terminal?
                 `((default-terminal . ,(file-append package "/bin/alacritty")))
                 '())))
   (home-services-getter alacritty-home-services)))
