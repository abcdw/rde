(define-module (rde features video)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu packages video)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services video)
  #:use-module (guix gexp)
  #:export (feature-mpv))

(define* (feature-mpv
          #:key
          (mpv mpv))
  "Setup and configure mpv."
  (ensure-pred any-package? mpv)

  (define (get-home-services config)
    (list
     (service
      home-mpv-service-type
      (home-mpv-configuration
       (package mpv)
       (default-options
         `((script . ,(file-append mpv-mpris "/lib/mpris.so"))
           (keep-open . #t)
           (save-position-on-quit . #t)))))))

  (feature
   (name 'mpv)
   (values (make-feature-values mpv))
   (home-services-getter get-home-services)))
