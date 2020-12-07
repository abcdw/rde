(define-module (rde system install)
  #:use-module (gnu system)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages bash)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system install)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system accounts)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

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

    (file-systems
     ;; Note: the disk image build code overrides this root file system with
     ;; the appropriate one.
     (cons* (file-system
              (mount-point "/")
              (device (file-system-label "Guix_image"))
              (type "ext4"))

            ;; Make /tmp a tmpfs instead of keeping the overlayfs.  This
            ;; originally was used for unionfs because FUSE creates
            ;; '.fuse_hiddenXYZ' files for each open file, and this confuses
            ;; Guix's test suite, for instance (see
            ;; <http://bugs.gnu.org/23056>).  We keep this for overlayfs to be
            ;; on the safe side.
            (file-system
              (mount-point "/tmp")
              (device "none")
              (type "tmpfs")
              (check? #f))

            ;; XXX: This should be %BASE-FILE-SYSTEMS but we don't need
            ;; elogind's cgroup file systems.
            (list %pseudo-terminal-file-system
                  %shared-memory-file-system
                  %efivars-file-system
                  %immutable-store)))

    (users (list (user-account
                  (name "guest")
                  (group "users")
                  (supplementary-groups '("wheel")) ; allow use of sudo
                  (password "")
                  (comment "Guest of GNU"))))

    (packages (append
	       (map specification->package+output
		    '("htop" "font-iosevka"
		      "emacs-next-pgtk" "emacs-guix" "emacs-use-package"
		      "emacs-magit"
		      "dmenu" "alacritty"
		      "ungoogled-chromium-wayland"
		      "sway" "wofi" "waybar" "light"
		      "git" "make" "iwd"
		      "gparted"))
	       %base-packages-disk-utilities
	       %base-packages))

    (services
     (append
      (list (service sddm-service-type
		     (sddm-configuration
		      (display-server "wayland")
		      (xorg-configuration
                       (xorg-configuration
                        (keyboard-layout keyboard-layout)))
		      (auto-login-user "guest")
		      (auto-login-session "sway.desktop"))))
      (cons*
       (modify-services
	(remove (lambda (service)
                  (member (service-kind service)
                          (list
                           gdm-service-type
                           )))
		%desktop-services))) ;;end of remove services
       ))
    ))

(pretty-print (map service-kind (operating-system-services installation-os)))
installation-os-nonfree
