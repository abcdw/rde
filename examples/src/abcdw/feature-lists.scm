(define-module (abcdw feature-lists)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features networking)
  #:use-module (rde features fontutils)
  #:use-module (rde features docker)
  #:use-module (rde features virtualization)
  #:use-module (rde features finance)
  #:use-module (rde features image-viewers)
  #:use-module (rde features markup)
  #:use-module (rde features video)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (rde features irc)
  #:use-module (rde features wm)
  #:use-module (rde features web-browsers)

  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)

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
   (feature-ungoogled-chromium
    #:default-browser? #t)
   (feature-ledger)
   (feature-imv)
   (feature-mpv
    #:extra-mpv-conf '((speed . 1.61)))))

(define-public %mail-features
  (list
   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)))

(define-public %dev-features
  (list
   (feature-markdown)))

(define-public %virtualization-features
  (list
   (feature-docker)
   (feature-qemu)))

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
                     (clock)))))

(define-public %emacs-features
  (list
   (feature-emacs
    #:default-application-launcher? #t)

   (feature-emacs-appearance)
   (feature-emacs-faces)
   (feature-emacs-modus-themes)

   (feature-emacs-completion
    #:mini-frame? #f
    #:marginalia-align 'right)
   (feature-emacs-corfu
    #:corfu-doc-auto #f)
   (feature-emacs-vertico)

   (feature-emacs-tramp)
   (feature-emacs-project)
   (feature-compile)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)

   (feature-emacs-message)
   (feature-emacs-erc
    #:erc-log? #t
    #:erc-autojoin-channels-alist '((Libera.Chat "#rde")))
   (feature-emacs-telega)
   (feature-emacs-elpher)

   (feature-emacs-pdf-tools)
   (feature-emacs-nov-el)
   (feature-emacs-org-protocol)
   ;; TODO: Remove auctex dependency, which interjects in texinfo-mode.
   ;; (feature-emacs-citar)

   (feature-emacs-smartparens
    #:show-smartparens? #t)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-eglot)))

(define-public %general-features
  (append
   %base-features
   %dev-features
   %cli-features
   %ui-features
   %emacs-features))

(define-public %all-features
  (append
   %base-features
   %dev-features
   %virtualization-features
   %mail-features
   %cli-features
   %ui-features
   %emacs-features))
