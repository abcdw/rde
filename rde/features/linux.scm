(define-module (rde features linux)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services wm)
  #:use-module (guix gexp)
  
  #:export (feature-backlight))

(define* (feature-backlight
	  #:key
	  (default-brightness 100)
	  (step 10)
	  (package brightnessctl))
  "Setup and configure brightness of various devices.  PACKAGE is
expected to be a brightnessctl."
  (ensure-pred brightness? default-brightness)
  (ensure-pred package? package)
  (ensure-pred brightness? step)

  (define (step->symbol op)
    (symbol-append (string->symbol (number->string step)) '% op))
  (define (backlight-home-services config)
    (list
     (simple-service
      'add-backlight
      home-profile-service-type
      (list package))
     (simple-service
      'backlight-add-brightness-control-to-sway
      home-sway-service-type
      `((bindsym XF86MonBrightnessUp exec
		 ,(file-append brightnessctl "/bin/brightnessctl")
		 set ,(step->symbol '+))
	(bindsym XF86MonBrightnessDown exec
		 ,(file-append brightnessctl "/bin/brightnessctl")
		 set ,(step->symbol '-))))))

  (define (backlight-system-services config)
    (list
     (simple-service
      'backlight-set-brightness-on-startup
      shepherd-root-service-type
      (list (shepherd-service
             (provision '(startup-brightness))
             (requirement '(virtual-terminal))
             (start #~(lambda ()
                        (invoke #$(file-append package "/bin/brightnessctl")
				"set" (string-append
				     (number->string #$default-brightness) "%"))))
             (respawn? #f))))
     (udev-rules-service
      'backlight-add-udev-rules
      package)))

  (feature
   (name 'backlight)
   (values '((backlight . #t)))
   (home-services-getter backlight-home-services)
   (system-services-getter backlight-system-services)))
