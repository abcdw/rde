(define-module (rde features linux)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu home-services wm)
  #:use-module (guix gexp)

  #:export (feature-backlight
	    feature-pipewire))

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
      'backlight-add-packages
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
             (one-shot? #t))))
     (udev-rules-service
      'backlight-add-udev-rules
      package)))

  (feature
   (name 'backlight)
   (values '((backlight . #t)))
   (home-services-getter backlight-home-services)
   (system-services-getter backlight-system-services)))


(define* (feature-pipewire
          #:key
          (package pipewire-latest))
  ""
  (define (home-pipewire-services _)
    (list
     ;; TODO: Make home-alsa-service-type
     (simple-service
      'pipewire-add-asoundrd
      home-files-service-type
      `(("config/alsa/asoundrc"
         ,(mixed-text-file
           "asoundrc"
           #~(string-append
              "<"
	      #$(file-append
                 package "/share/alsa/alsa.conf.d/50-pipewire.conf")
	      ">\n<"
	      #$(file-append
                 package "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
              ">\n"
              "
pcm_type.pipewire {
  lib " #$(file-append package
                       "/lib/alsa-lib/libasound_module_pcm_pipewire.so") "
}

ctl_type.pipewire {
  lib " #$(file-append package
                       "/lib/alsa-lib/libasound_module_ctl_pipewire.so") "
}
")))))


     (simple-service
      'pipewire-add-shepherd-daemons
      home-shepherd-service-type
      (list
       (shepherd-service
        (requirement '(dbus-home))
        (provision '(pipewire))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append package "/bin/pipewire")))))
       (shepherd-service
        (requirement '(pipewire))
        (provision '(pipewire-media-session))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append package "/bin/pipewire-media-session")))))
       (shepherd-service
        (requirement '(pipewire))
        (provision '(pipewire-pulse))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append package "/bin/pipewire-pulse")))))))
     (simple-service
      'pipewire-add-packages
      home-profile-service-type
      (append
       ;; TODO: Should be in feature-sway
       (list package)))))

  (define (system-pipewire-services _)
    (list
     (udev-rules-service
      'pipewire-add-udev-rules
      package)))

  (feature
   (name 'pipewire)
   (values `((pipewire . #t)))
   (home-services-getter home-pipewire-services)
   (system-services-getter system-pipewire-services)))
