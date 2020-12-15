;; This is an operating system configuration template
;; for a "desktop" setup with GNOME and Xfce where the
;; root partition is encrypted with LUKS.

(use-modules (gnu) (gnu system nss))
(use-service-modules desktop xorg)
(use-package-modules certs gnome)

(operating-system
  (host-name "antelope")
  (timezone "Europe/Paris")
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
                         (source "0e51ee1e-49ef-45c6-b0c3-6307e9980fa9")
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
                         ;; (flags '(no-atime))
                         (dependencies mapped-devices))
		       ;; (file-system
		       ;; 	(mount-point "/boot/efi")
                       ;;  (type "vfat"))
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
		  '("htop"
		    "font-iosevka" "font-dejavu" "font-gnu-unifont"
		    "emacs-next-pgtk" "emacs-guix" "emacs-use-package"
		    "emacs-magit"
		    "dmenu" "alacritty"
		    "ungoogled-chromium-wayland"
		    "sway" "wofi" "waybar" "light"
		    "git" "gnupg" "make" "iwd"
		    "gparted" "grub"
		    "glibc" "nss-certs"))
	     %base-packages-disk-utilities
	     %base-packages))
    
  ;; Add GNOME and Xfce---we can choose at the log-in screen
  ;; by clicking the gear.  Use the "desktop" services, which
  ;; include the X11 log-in service, networking with
  ;; NetworkManager, and more.
  (services (append (list (service gnome-desktop-service-type)
                          (service xfce-desktop-service-type)
                          (set-xorg-configuration
                           (xorg-configuration
                            (keyboard-layout keyboard-layout))))
                    %desktop-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
