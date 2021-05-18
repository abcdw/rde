;; This is an operating system configuration template for a "wayland"
;; setup with sway, emacs-pgtk and ungoogled-chromium where the root
;; partition is encrypted with LUKS.

(define-module (rde system desktop)
  #:use-module (gnu system)
  ;; #:use-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu system keyboard)
  #:use-module ((gnu system install) #:prefix gnu-system-install:)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu system nss)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services xorg)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu services dbus)
  #:use-module (gnu services security-token)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:export (os))

(define os
  (operating-system
   (host-name "antelope")
   (timezone "Europe/Moscow")
   (locale "en_US.utf8")

   ;; Choose US English keyboard layout.  The "altgr-intl"
   ;; variant provides dead keys for accented characters.

   (keyboard-layout
    (keyboard-layout "us,ru" "dvorak,"
		     #:options '("grp:win_space_toggle" "ctrl:nocaps")))
   ;; Use the UEFI variant of GRUB with the EFI System
   ;; Partition mounted on /boot/efi.
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")
                (keyboard-layout keyboard-layout)))

   ;; Specify a mapped device for the encrypted root partition.
   ;; The UUID is that returned by 'cryptsetup luksUUID'.

   (mapped-devices (list (mapped-device
                          (source (uuid "0e51ee1e-49ef-45c6-b0c3-6307e9980fa9"))
                          (target "enc")
                          (type luks-device-mapping))))

   (file-systems (cons* (file-system
                         (device "/dev/mapper/enc")
                         (mount-point "/")
                         (type "btrfs")
			 (options "subvol=root")
                         ;; (flags '(no-atime))
                         (dependencies mapped-devices))
			(file-system
                         (device "/dev/mapper/enc")
                         (mount-point "/boot")
                         (type "btrfs")
			 (options "subvol=boot")
                         (dependencies mapped-devices))
			(file-system
                         (device "/dev/mapper/enc")
                         (mount-point "/gnu")
                         (type "btrfs")
			 (options "subvol=gnu")
                         (dependencies mapped-devices))
			(file-system
                         (device "/dev/mapper/enc")
                         (mount-point "/home")
                         (type "btrfs")
			 (options "subvol=home")
                         (dependencies mapped-devices))
			(file-system
                         (device "/dev/mapper/enc")
                         (mount-point "/var/log")
                         (type "btrfs")
			 (options "subvol=log")
                         (dependencies mapped-devices))
			(file-system
                         (device "/dev/mapper/enc")
                         (mount-point "/data")
                         (type "btrfs")
			 (options "subvol=data")
                         (dependencies mapped-devices))
			(file-system
			 (mount-point "/boot/efi")
                         (type "vfat")
			 (device (uuid "8C99-0704" 'fat32)))
			%base-file-systems))

   ;; Create user `bob' with `alice' as its initial password.
   (users (cons (user-account
                 (name "bob")
                 (comment "Alice's brother")
                 (password (crypt "alice" "$6$abc"))
                 (group "users")
                 (supplementary-groups '("wheel" "netdev"
                                         "audio" "video")))
		%base-user-accounts))

   ;; This is where we specify system-wide packages.
   (packages (append
	      (map specification->package+output
		   '(;; System packages
		     "nss-certs"))
	      %base-packages-disk-utilities
	      %base-packages))

   (kernel-loadable-modules (list v4l2loopback-linux-module))
   (services
    (append
     (list
      ;; (simple-service
      ;;  'add-xdg-desktop-portals
      ;;  dbus-root-service-type
      ;;  (list xdg-desktop-portal xdg-desktop-portal-wlr))
      (simple-service 'switch-to-tty2 shepherd-root-service-type
                      (list (shepherd-service
                             (provision '(kdb))
                             (requirement '(virtual-terminal))
                             (start #~(lambda ()
                                        (invoke #$(file-append kbd "/bin/chvt") "2")))
                             (respawn? #f))))
      (service pcscd-service-type)
      (screen-locker-service swaylock "swaylock")
      (udev-rules-service
       'backlight
       (file->udev-rule "90-backlight.rules"
			(file-append light "/lib/udev/rules.d/90-backlight.rules"))))
     (remove (lambda (service)
	       (member (service-kind service)
		       (list gdm-service-type
			     screen-locker-service-type)))
	     %desktop-services)))

   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))

;; (use-modules (guix store)
;; 	     (guix derivations))
;; (with-store store
;;     (run-with-store store (operating-system-derivation os)))
os
