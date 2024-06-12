(define-module (rde-configs hosts cloud)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features system)

  #:use-module (rde system services admin)
  #:use-module (rde system services guix)
  #:use-module (rde system services cloud-init)

  #:use-module (guix gexp)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (gnu image)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image))


;;; Hardware/host specifis features


;; Use virtio in your hypervisor to make root partition appear on /dev/vda1
(define cloud-file-systems
  (list
   (file-system
     (mount-point "/")
     (device "/dev/vda1")
     (type "ext4"))))

(define sudoers-extra-service
  (simple-service
   'sudoers-extra
   sudoers-service-type
   (list "%wheel ALL=(ALL) NOPASSWD: ALL")))

(define (find-resource-in-load-path file)
  ;; Guix sets %load-extensions to only .scm, which makes it impossible to use
  ;; %search-load-path function for finding resources.
  (let* ((old-load-extensions %load-extensions)
         (_ (set! %load-extensions '("")))
         (result (%search-load-path file))
         (_ (set! %load-extensions old-load-extensions)))
    result))

(define cloud-extra-services
  (feature-custom-services
   #:feature-name-prefix 'cloud-extra
   #:system-services
   (list
    (service dhcp-client-service-type)
    (service cloud-init-service-type)
    (service openssh-service-type
             (openssh-configuration
              (openssh openssh-sans-x)
              (permit-root-login #t)
              (password-authentication? #f)
              (authorized-keys
               `(("root"
                  ,(local-file
                    (canonicalize-path
                     (find-resource-in-load-path
                      "rde-configs/files/ssh/public-keys/abcdw"))))))))
    sudoers-extra-service)))

(define-public cloud-features
  (list
   (feature-host-info
    #:host-name "cloud"
    #:timezone  "Etc/UTC")
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-bootloader)
     (targets '("/dev/vda"))
     (terminal-outputs '(console))))
   (feature-base-services)
   (feature-file-systems
    #:file-systems cloud-file-systems)
   cloud-extra-services))

(define cloud-config
  (rde-config
   (features
    cloud-features)))

(define-public cloud-image
  (image-with-os
   (image
    (inherit mbr-disk-image)
    (name 'cloud-rde)
    (format 'compressed-qcow2))
   (rde-config-operating-system cloud-config)))
