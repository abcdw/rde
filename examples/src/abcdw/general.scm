(define-module (abcdw general)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features networking)
  #:use-module (rde features fontutils)
  #:use-module (rde features docker)
  #:use-module (rde features virtualization)
  #:use-module (rde features finance)
  #:use-module (rde features markup)
  #:use-module (rde features video)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (rde features wm)

  #:use-module (rde features terminals)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features version-control)
  #:use-module (rde features tmux)
  #:use-module (rde features ssh)

  #:use-module (guix gexp))

(define-public %base-features
  (list
   ;; TODO: merge them into feature-base
   (feature-base-services)
   (feature-base-packages)
   (feature-desktop-services)

   (feature-pipewire)
   (feature-backlight #:step 10)
   (feature-networking)

   (feature-transmission #:auto-start? #f)
   (feature-ledger)
   (feature-mpv
    #:extra-mpv-conf '((speed . 1.61)))
   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)))

(define-public %dev-features
  (list
   (feature-markdown)
   (feature-docker)
   (feature-qemu)))

(define-public %ui-features
  (list
   (feature-fonts
    #:font-monospace (font "Iosevka" #:size 11 #:weight 'regular)
    ;; #:font-monospace (font "Fira Mono" #:size 14 #:weight 'semi-light)
    ;; #:font-packages (list font-fira-mono)
    #:default-font-size 11)

   ;; https://sr.ht/~tsdh/swayr/
   ;; https://github.com/ErikReider/SwayNotificationCenter
   ;; https://github.com/swaywm/sway/wiki/i3-Migration-Guide

   ;; https://github.com/natpen/awesome-wayland
   (feature-sway)
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   ;; (feature-sway-statusbar
   ;;  #:use-global-fonts? #f)
   (feature-waybar)
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config '((screenshots)
                     (effect-blur . 7x5)
                     (clock)))

   ))

(define-public %cli-features
  (list
   (feature-alacritty
    ;; TODO: Rename to alacritty-yml
    #:config-file (local-file "./config/alacritty/alacritty.yml")
    #:default-terminal? #f
    #:backup-terminal? #t
    #:software-rendering? #f)
   (feature-vterm)
   (feature-tmux
    #:tmux-conf (local-file "./config/tmux/tmux.conf"))
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-bash)
   (feature-direnv)
   (feature-git)
   (feature-ssh)))

(define-public %general-features
  (append
   %base-features
   %dev-features
   %cli-features
   %ui-features))
