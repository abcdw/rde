(define-module (rde-configs hosts live)
  #:use-module (rde features base)
  #:use-module (rde features system)

  #:use-module (rde system services admin)
  #:use-module (rde system services guix)

  #:use-module (gnu services)
  #:use-module (gnu system file-systems))


;;; Hardware/host specifis features

;; TODO: Switch from UUIDs to partition labels For better
;; reproducibilty and easier setup.  Grub doesn't support luks2 yet.

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

(define sudoers-extra-service
  (simple-service
   'sudoers-extra
   sudoers-service-type
   (list "%wheel ALL=(ALL) NOPASSWD: ALL")))

(define live-extra-services
  (feature-custom-services
   #:feature-name-prefix 'live-extra
   #:system-services
   (list
    sudoers-extra-service
    (service cow-store-service-type))))

(define-public %live-features
  (list
   (feature-host-info
    #:host-name "live"
    #:issue "This is rde.  Welcome.  Login and password - guest and guest.\n"
    ;; ls `guix build tzdata`/share/zoneinfo
    #:timezone  "Europe/Kiev")
   live-extra-services
   (feature-hidpi)
   (feature-file-systems
    #:file-systems live-file-systems)))
