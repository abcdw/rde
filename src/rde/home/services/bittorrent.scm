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
  #:use-module (ice-9 match)
  #:export (home-transmission-configuration
            home-transmission-service-type))

(define-maybe/no-serialization string)

(define-configuration/no-serialization home-transmission-configuration
  (transmission
   (package transmission)
   "The transmission package to use.")
  (settings
   (json-config '())
   "Transmission configuration.")
  (auto-start?
   (boolean #f)
   "Whether to autostart the transmission daemon.")
  (download-dir
   maybe-string
   "Where to download torrent data to."))

(define home-transmission-shepherd-service
  (match-lambda
    (($ <home-transmission-configuration> transmission _ auto-start? download-dir)
     (list
      (shepherd-service
       (provision '(transmission))
       (auto-start? auto-start?)
       (start #~(make-forkexec-constructor
                 (list
                  #$(file-append transmission "/bin/transmission-daemon")
                  "--foreground" "--no-incomplete-dir"
                  #$@(if (maybe-value-set? download-dir)
                         (list "--download-dir" download-dir)
                         '()))))
       (stop #~(make-kill-destructor)))))))

(define (add-transmission-configuration config)
  (if (not (null? (home-transmission-configuration-settings config)))
      `(("transmission-daemon/settings.json"
         ,(apply mixed-text-file "settings.json"
                 (serialize-json-config
                  (home-transmission-configuration-settings config)))))
      '()))

(define home-transmission-service-type
  (service-type
   (name 'home-transmission)
   (extensions
    (list
     (service-extension home-xdg-configuration-files-service-type
                        add-transmission-configuration)
     (service-extension home-shepherd-service-type
                        home-transmission-shepherd-service)))
   (description "Launch a transmission daemon from the user space.")
   (default-value (home-transmission-configuration))))
