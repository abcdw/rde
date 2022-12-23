(define-module (abcdw hosts live)
  #:use-module (rde features base)
  #:use-module (rde features system)
  ;; #:use-module (rde features wm)
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

(define-public %live-features
  (list
   (feature-host-info
    #:host-name "live"
    ;; ls `guix build tzdata`/share/zoneinfo
    #:timezone  "Europe/Kiev")
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   ;; (feature-bootloader)
   (feature-file-systems
    #:file-systems live-file-systems)))
