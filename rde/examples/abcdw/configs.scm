(define-module (rde examples abcdw configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features password-utils)
  #:use-module (rde features version-control)
  #:use-module (rde features fontutils)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features emacs)
  #:use-module (rde features linux)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (rde features docker)
  ;; #:use-module (gnu services)
  ;; #:use-module (gnu services nix)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages)
  #:use-module (rde packages)
  #:use-module (guix gexp)
  #:use-module (ice-9 match))


;;; User-specific features

;; Initial user's password hash will be available in store, so it's
;; use this feature with care
;; (display (crypt "hi" "$6$abc"))

(define* (mail-acc id user #:optional (type 'gmail))
  "Make a simple mail-account with gmail type by default."
  (mail-account
   (id   id)
   (fqda user)
   (type type)))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define %abcdw-features
  (list
   (feature-user-info
    #:user-name "bob"
    #:full-name "Andrew Tropin"
    #:email "andrew@trop.in"

    ;; WARNING: This option can reduce the explorability by hiding
    ;; some helpful messages and parts of the interface for the sake
    ;; of minimalistic, less distractive and clean look.  Generally
    ;; it's not recommended to use it.
    #:emacs-advanced-user? #t)
   (feature-gnupg
    #:gpg-primary-key "74830A276C328EC2"
    #:gpg-smart-card? #t)
   (feature-password-store
    #:remote-password-store-url "ssh://abcdw@olorin.lan/~/state/password-store")

   (feature-mail-settings
    #:mail-accounts (list (mail-acc 'work       "andrew@trop.in")
                          (mail-acc 'personal   "andrewtropin@gmail.com"))
    #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))
                          (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                    '("https://yhetil.org/guix-bugs/0"))
                          (mail-lst 'guix-patches "guix-patches@gnu.org"
                                    '("https://yhetil.org/guix-patches/1"))))

   (feature-keyboard
    #:keyboard-layout %dvorak-layout)))

;;; TODO: Add documentation about starting guile repl
;;; TODO: feature-wallpapers https://wallhaven.cc/
;;; TODO: feature-icecat
;;; TODO: feature-bash?
;;; TODO: feature-battery
;; PipeWire/iwd:
;; https://github.com/J-Lentz/iwgtk
;; https://github.com/krevedkokun/guix-config/blob/master/system/yggdrasil.scm


;;; Generic features should be applicable for various hosts/users/etc

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

;;; WARNING: The order can be important for features extending
;;; services of other features.  Be careful changing it.
(define %main-features
  (list
   (feature-custom-services
    #:system-services
    (list
     ;; (service nix-service-type)
     )
    #:home-services
    (list
     ;; ((@ (gnu services) simple-service)
     ;;  'extend-shell-profile
     ;;  (@ (gnu home-services shells) home-shell-profile-service-type)
     ;;  (list
     ;;   #~(string-append
     ;;      "alias superls="
     ;;      #$(file-append (@ (gnu packages base) coreutils) "/bin/ls"))))
     ))

   (feature-base-services)
   (feature-desktop-services)
   (feature-docker)

   (feature-fonts)
   (feature-pipewire)
   (feature-backlight)

   (feature-alacritty
    #:config-file (local-file "./config/alacritty/alacritty.yml"))
   (feature-tmux
    #:config-file (local-file "./config/tmux/tmux.conf"))
   (feature-zsh)
   (feature-ssh)
   (feature-git)

   (feature-sway
    #:extra-config
    `((include ,(local-file "./config/sway/config"))))
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   (feature-sway-statusbar)

   (feature-direnv)
   (feature-emacs
    #:additional-elisp-packages
    (append
     (pkgs "emacs-yasnippet" "emacs-elfeed" "emacs-hl-todo")))
   (feature-emacs-appearance)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #t)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-keycast)

   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)
   (feature-emacs-message)
   (feature-emacs-erc
    #:erc-nick "abcdw"
    #:erc-autojoin-channels-alist
    '(("irc.libera.chat" "#guix" "#emacs" "#tropin" "#rde")
      ("irc.oftc.net"    "#pipewire" "#wayland")))
   (feature-emacs-elpher)
   (feature-emacs-telega)
   (feature-emacs-pdf-tools)

   ;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
   (feature-emacs-git)
   ;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>
   (feature-emacs-org
    #:org-directory "~/work/abcdw/notes")
   (feature-emacs-org-roam
    ;; TODO: Rewrite to states
    #:org-roam-directory "~/work/abcdw/notes/notes")

   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)
   (feature-notmuch
    #:notmuch-saved-searches
    (cons* '(:name "Work Inbox" :query "tag:work and tag:inbox" :key "W")
           %rde-notmuch-saved-searches))

   (feature-transmission #:auto-start? #f)

   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (music "$HOME/music")
     (videos "$HOME/vids")
     (pictures "$HOME/pics")
     (documents "$HOME/docs")
     (download "$HOME/dl")
     (desktop "$HOME")
     (publicshare "$HOME")
     (templates "$HOME")))
   (feature-base-packages
    #:home-packages
    (append
     (pkgs
      "alsa-utils" "mpv" "youtube-dl" "imv"
      "obs" "obs-wlrobs"
      "icecat"
      "ungoogled-chromium-wayland" "ublock-origin-chromium"
      "nyxt"
      "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-standard"
      "ripgrep" "curl" "make")))))

(define %laptop-features
  (list ))


;;; Hardware/host specifis features

;; TODO: Switch from UUIDs to partition labels For better
;; reproducibilty and easier setup.  Grub doesn't support luks2 yet.

(define ixy-mapped-devices
  (list (mapped-device
         (source (uuid "0e51ee1e-49ef-45c6-b0c3-6307e9980fa9"))
         (target "enc")
         (type luks-device-mapping))))

(define ixy-file-systems
  (append
   (map (match-lambda
	  ((subvol . mount-point)
	   (file-system
	     (type "btrfs")
	     (device "/dev/mapper/enc")
	     (mount-point mount-point)
	     (options (format #f "subvol=~a" subvol))
	     (dependencies ixy-mapped-devices))))
	'((root . "/")
	  (boot . "/boot")
	  (gnu  . "/gnu")
	  (home . "/home")
	  (data . "/data")
	  (log  . "/var/log")))
   (list
    (file-system
      (mount-point "/boot/efi")
      (type "vfat")
      (device (uuid "8C99-0704" 'fat32))))))

(define %ixy-features
  (list
   (feature-host-info
    #:host-name "ixy"
    #:timezone  "Europe/Moscow")
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   ;; (feature-bootloader)
   (feature-file-systems
    #:mapped-devices ixy-mapped-devices
    #:file-systems   ixy-file-systems)
   (feature-hidpi)))


;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(define-public ixy-config
  (rde-config
   (features
    (append
     %abcdw-features
     %main-features
     %ixy-features))))

;; TODISCUSS: Make rde-config-os/he to be a feature instead of getter?
(define ixy-os
  (rde-config-operating-system ixy-config))
(define ixy-he
  (rde-config-home-environment ixy-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      (_ ixy-he))))

;; (pretty-print-rde-config ixy-config)
;; (use-modules (gnu services)
;; 	     (gnu services base))
;; (display
;;  (filter (lambda (x)
;; 	   (eq? (service-kind x) console-font-service-type))
;; 	 (rde-config-system-services ixy-config)))

;; (use-modules (rde features))
;; ((@@ (ice-9 pretty-print) pretty-print)
;;  (map feature-name (rde-config-features ixy-config)))

(dispatcher)
