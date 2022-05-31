(define-module (rde system install)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system install))

(define guix-with-substitute-mirror
  (operating-system
    (inherit installation-os)
    (services
     (modify-services (operating-system-user-services installation-os)
      (guix-service-type
       config =>
       (guix-configuration
        (inherit config)
        (substitute-urls '("http://ci.guix.trop.in"
                           "https://bordeaux.guix.gnu.org"))))))))

guix-with-substitute-mirror
