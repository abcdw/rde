(define-module (rde features presets)
  #:use-module (rde features base)
  #:use-module (rde features bittorrent)
  #:use-module (rde features docker)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features finance)
  #:use-module (rde features fontutils)
  #:use-module (rde features image-viewers)
  #:use-module (rde features irc)
  #:use-module (rde features linux)
  #:use-module (rde features mail)
  #:use-module (rde features markup)
  #:use-module (rde features networking)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features version-control)
  #:use-module (rde features video)
  #:use-module (rde features virtualization)
  #:use-module (rde features web-browsers)
  #:use-module (rde features wm))

;;;
;;; Various lists of features with predefined values.
;;;

(define-public rde-base
  (list
   ;; TODO: merge them into feature-base (maybe using super-feature)
   (feature-base-services)
   (feature-base-packages)
   (feature-desktop-services)

   (feature-pipewire)
   (feature-backlight #:step 10)
   (feature-networking)))

(define-public rde-desktop
  (list
   (feature-fonts)

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
   (feature-swaynotificationcenter)
   (feature-waybar)
   (feature-swayidle)
   (feature-swaylock
    ;; #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; ;; The blur on lock screen is not privacy-friendly.
    ;; #:extra-config '((screenshots)
    ;;                  (effect-blur . 7x5)
    ;;                  (clock))
    )
   (feature-imv)
   (feature-mpv)
   (feature-ungoogled-chromium
    #:default-browser? #t)
  (feature-transmission #:auto-start? #f)
  (feature-ledger)))

(define-public rde-mail
  (list
   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)))

(define-public rde-cli
  (list
   (feature-vterm)
   (feature-tmux)
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-bash)
   (feature-direnv)
   (feature-git)
   (feature-ssh)))

(define-public rde-emacs
  (list
   (feature-emacs
    #:default-application-launcher? #t)

   (feature-emacs-appearance)
   (feature-emacs-modus-themes)
   ;; Adds 0.15s load time and blinking
   ;; (feature-emacs-dashboard)

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
