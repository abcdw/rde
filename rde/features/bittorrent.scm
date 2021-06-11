(define-module (rde features bittorrent)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages bittorrent)
  #:use-module (guix gexp)

  #:export (feature-transmission))

(define* (feature-transmission
          #:key
          (package transmission)
          (auto-start? #t))
  "Setup and configure Transmission"

  (define (transmission-home-services _)
    (list
     (simple-service
      'transmission-add-shepherd-daemon
      home-shepherd-service-type
      (list
       ;; TODO: Make home-transmission service for Guix Home
       (shepherd-service
        (provision '(transmission))
        (auto-start? auto-start?)
        (start #~(make-forkexec-constructor
                  (list #$(file-append package "/bin/transmission-daemon")
                        "--foreground")))
        (stop  #~(make-kill-destructor)))))))

  (feature
   (name 'transmission)
   (values `((transmission . #t)))
   (home-services-getter transmission-home-services)))
