(define-module (rde system install)
  #:use-module (gnu system)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages fonts)
  #:use-module (gnu system keyboard)
  #:use-module ((gnu system install) #:prefix gnu-system-install:)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu services security-token)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (rde emacs packages)
  #:export (installation-os))

(define installation-os
  (operating-system
    (inherit gnu-system-install:installation-os)
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

    (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=NOPASSWD: ALL\n"))

    (services
     (append
      (list
       ;; (service documentation-service-type "tty2")
       (service pcscd-service-type)
       (service console-font-service-type
                (map (match-lambda
                       ("tty2"
                        '("tty2" . "LatGrkCyr-8x16"))
		       ("tty3"
			`("tty3" . ,(file-append
				     font-terminus
				     "/share/consolefonts/ter-132n")))
                       (tty
                        ;; Use a font that doesn't have more than 256
                        ;; glyphs so that we can use colors with varying
                        ;; brightness levels (see note in setfont(8)).
                        `(,tty . "lat9u-16")))
                     '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6"))))
      (cons*
       (modify-services
	(remove (lambda (service)
                  (member (service-kind service)
                          (list
                           gdm-service-type
			   console-font-service-type
                           )))
		%desktop-services)))))))

(pretty-print (map service-kind (operating-system-services installation-os)))
installation-os
