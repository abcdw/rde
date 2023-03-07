(define-module (rde system install)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (rde features)
  #:use-module (rde features system)
  #:use-module (rde features base)
  #:export (guix-with-substitute-mirror
            live-os))

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

(define live-file-systems
  (list (file-system
          (mount-point "/")
          (device (file-system-label "Guix_image"))
          (type "ext4"))
        (file-system
          (mount-point "/tmp")
          (device "none")
          (type "tmpfs")
          (check? #f))))

(define* (live-os
          #:key
          (kernel linux-libre)
          (kernel-firmware '())
          (guix-substitute-urls '())
          (guix-authorized-keys '())
          (custom-system-services '())
          (supplementary-system-packages '())
          (supplementary-features '())) ;; user-preferences
  (rde-config
   (initial-os installation-os)
   (features
    (append
     supplementary-features
     (list
      (feature-file-systems
       #:file-systems live-file-systems)
      (feature-kernel
       #:kernel kernel
       #:firmware kernel-firmware)
      (feature-base-packages
       #:system-packages supplementary-system-packages)
      (feature-custom-services
       #:feature-name-prefix 'live
       #:system-services custom-system-services)
      (feature-base-services
       #:guix-substitute-urls guix-substitute-urls
       #:guix-authorized-keys guix-authorized-keys))))))
