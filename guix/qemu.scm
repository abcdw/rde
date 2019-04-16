;; This is an operating system configuration template
;; for a "desktop" setup with GNOME and Xfce where the
;; root partition is encrypted with LUKS.

(use-modules (gnu) (gnu system nss)
             ;; (gnu packages package-management)
             (ice-9 pretty-print))
(use-service-modules desktop)
(use-package-modules certs gnome wm package-management)


(operating-system
  (host-name "antelope")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")))

  ;; Specify a mapped device for the encrypted root partition.
  ;; The UUID is that returned by 'cryptsetup luksUUID'.
  (mapped-devices
   (list (mapped-device
          (source (uuid "a132b140-09fd-4085-93c8-e2f2a5749984"))
          (target "my-partition")
          (type luks-device-mapping))))

  (file-systems (cons* (file-system
                        (device "/dev/mapper/my-partition")
                        (mount-point "/")
                        (type "ext4")
                        (dependencies mapped-devices))
                       (file-system
                        (device "/dev/sda1")
                        (mount-point "/boot/efi")
			                  (type "vfat"))
                       %base-file-systems))

  ;(pretty-print %base-file-systems)

  (users (cons (user-account
                (name "bob")
                (comment "Alice's brother")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video"))
                (home-directory "/home/bob"))
               %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages (cons* nss-certs         ;for HTTPS access
                   gvfs              ;for user mounts
                   sway
                   nix
                   %base-packages))

  ;; Add GNOME and/or Xfce---we can choose at the log-in
  ;; screen with F1.  Use the "desktop" services, which
  ;; include the X11 log-in service, networking with
  ;; NetworkManager, and more.
  (services (cons*
             ;; (service gnome-desktop-service-type)
             ;; (service xfce-desktop-service-type)
             %desktop-services
             ))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))

;; (display %desktop-services)
