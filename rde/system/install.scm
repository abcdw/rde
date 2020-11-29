(define-module (rde system install)
  #:use-module (gnu system)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages bash)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system install)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

(define bash-instead-of-u-boot-tools
  (package-input-rewriting `((,u-boot-tools . ,bash-minimal))))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (keyboard-layout
     (keyboard-layout "us,ru" "dvorak,"
		      #:options '("grp:win_space_toggle" "ctrl:nocaps")))

    ;; (firmware (list linux-firmware))
    (firmware (cons* iwlwifi-firmware
                     %base-firmware))
    (packages (append
	       (map specification->package+output
		    '("htop" "font-iosevka"
		      "emacs" "emacs-guix"
		      "emacs-use-package"
		      "i3-gaps" "i3status"
		      "dmenu" "xterm" "alacritty"
		      "ungoogled-chromium" "icecat"
		      "sway"
		      "gparted"))
	       %base-packages))

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
    ))

;; (bash-instead-of-u-boot-tools
;;  )

installation-os-nonfree
