;; This is an operating system configuration template
;; for a "desktop" setup with GNOME and Xfce where the
;; root partition is encrypted with LUKS.

(use-modules (gnu) (guix) (srfi srfi-1))
(use-modules (gnu) (gnu system nss))
(use-modules (nongnu packages linux)
	     (nongnu system linux-initrd))
(use-service-modules desktop xorg networking)
(use-package-modules certs gnome terminals emacs fonts emacs-xyz guile
		     xorg tmux wm suckless admin)

(operating-system
  (host-name "ixy")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (keyboard-layout
   (keyboard-layout "us,ru" "dvorak,"
		    #:options '("grp:win_space_toggle" "ctrl:nocaps")))

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/vda")
               (keyboard-layout keyboard-layout)
               (terminal-outputs '(console))))
  (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda1")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "abcdw")
                (password "")                     ;no password
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Our /etc/sudoers file.  Since 'guest' initially has an empty password,
  ;; allow for password-less sudo.
  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=NOPASSWD: ALL\n"))

  ;; This is where we specify system-wide packages.
  (packages (append (list

		     alacritty xterm tmux
		     i3-gaps dmenu i3status
		     htop

		     font-iosevka
		     emacs
		     emacs-guix
		     emacs-geiser
		     ;; for HTTPS access
                     nss-certs
                     ;; for user mounts
                     gvfs)
                    %base-packages
		    ))

  (services
   (append (list
            ;; (service slim-service-type
            ;;          (slim-configuration
            ;;           ;; (auto-login? #t)
            ;;           (default-user "abcdw")
            ;;           (xorg-configuration
            ;;            (xorg-configuration
            ;;             (keyboard-layout keyboard-layout)))))
	    (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout))))
	   	 %desktop-services))
  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
