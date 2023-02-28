(define-module (rde home services bittorrent)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bittorrent)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (rde serializers json)
  #:export (home-transmission-configuration
            home-transmission-service-type))

(define-configuration/no-serialization home-transmission-configuration
  (transmission
   (package transmission)
   "The transmission package to use.")
  (settings
   (json-config '())
   "Transmission configuration."))

(define (home-transmission-shepherd-service config)
  (list
   (shepherd-service
    (provision '(transmission))
    (start #~(make-forkexec-constructor
              (list #$(file-append
                       (home-transmission-configuration-transmission config)
                       "/bin/transmission-daemon")
                    "--foreground")))
    (stop #~(make-kill-destructor)))))

(define (add-transmission-configuration config)
  `(("transmission-daemon/settings.json"
     ,(apply mixed-text-file "settings.json"
             (serialize-json-config
              (home-transmission-configuration-settings config))))))

(define (home-transmission-profile-service config)
  (list (home-transmission-configuration-transmission config)))

(define home-transmission-service-type
  (service-type
   (name 'home-transmission)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-transmission-profile-service)
     (service-extension
      home-xdg-configuration-files-service-type
      add-transmission-configuration)
     (service-extension
      home-shepherd-service-type
      home-transmission-shepherd-service)))
   (description "Launch a transmission daemon from the user space.")
   (default-value (home-transmission-configuration))))
