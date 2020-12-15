(define-module (rde system install)
  #:use-module (gnu system)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu system keyboard)
  #:use-module ((gnu system install) #:prefix gnu-system-install:)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu services dbus)
  #:use-module (gnu services security-token)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (rde emacs packages)
  #:export (installation-os))


(define installation-os
  (operating-system
    (inherit gnu-system-install:installation-os)

    ;; TODO: Move layout to flavoured config?
    (keyboard-layout
     (keyboard-layout "us,ru" "dvorak,"
		      #:options '("grp:win_space_toggle" "ctrl:nocaps")))

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

    (services
     (append
      (list (service pcscd-service-type)
	    ;; (dbus-service)
	    (polkit-service)
	    (service mingetty-service-type (mingetty-configuration
                                            (tty "tty8")))
	    (elogind-service))
      (operating-system-user-services gnu-system-install:installation-os)))

    (setuid-programs (list (file-append shadow "/bin/passwd")
			   (file-append sudo "/bin/sudo")))))

(pretty-print (map service-kind (operating-system-services installation-os)))
installation-os
