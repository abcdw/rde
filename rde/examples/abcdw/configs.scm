(define-module (rde examples abcdw configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features xdisorg)
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
  #:use-module (rde features video)
  #:use-module (rde features markup)
  ;; #:use-module (gnu services)
  #:use-module (rde features networking)
  #:use-module (gnu services)
  #:use-module (rde home services i2p)

  ;; #:use-module (gnu services nix)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages)
  #:use-module (rde packages)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
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
    #:user-initial-password-hash
    "$6$abc$3SAZZQGdvQgAscM2gupP1tC.SqnsaLSPoAnEOb2k6jXMhzQqS1kCSplAJ/vUy2rrnpHtt6frW2Ap5l/tIvDsz."
    ;; (crypt "bob" "$6$abc")

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
    #:mail-accounts (list (mail-acc 'work       "andrew@trop.in" 'gandi)
                          (mail-acc 'personal   "andrewtropin@gmail.com"))
    #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))
                          (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                    '("https://yhetil.org/guix-bugs/0"))
                          (mail-lst 'guix-patches "guix-patches@gnu.org"
                                    '("https://yhetil.org/guix-patches/1"))))

   (feature-keyboard
    ;; To get all available options, layouts and variants run:
    ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
    #:keyboard-layout
    (keyboard-layout
     "us,ru" "dvorak,"
     #:options '("grp:shifts_toggle" "ctrl:nocaps")))))

;;; TODO: feature-wallpapers https://wallhaven.cc/
;;; TODO: feature-icecat
;; PipeWire/iwd:
;; https://github.com/J-Lentz/iwgtk
;; https://github.com/krevedkokun/guix-config/blob/master/system/yggdrasil.scm


;;; Generic features should be applicable for various hosts/users/etc

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define* (pkgs-vanilla #:rest lst)
  "Packages from guix channel."
  (define channel-guix
    (list (channel
           (name 'guix)
           (url "https://git.savannah.gnu.org/git/guix.git")
           (commit
            "2b6af630d61dd5b16424be55088de2b079e9fbaf"))))

  (define inferior (inferior-for-channels channel-guix))
  (define (get-inferior-pkg pkg-name)
    (car (lookup-inferior-packages inferior pkg-name)))

   (map get-inferior-pkg lst))


;;; WARNING: The order can be important for features extending
;;; services of other features.  Be careful changing it.
(define %main-features
  (list
   (feature-ssh-socks-proxy
    #:host "204:cbf:3e07:e67a:424f:93bc:fc5c:b3dc")
   (feature-i2pd
    #:outproxy 'http://acetone.i2p:8888
    ;; 'purokishi.i2p
    #:less-anonymous? #t)
   (feature-custom-services
    #:feature-name-prefix 'ixy
    #:system-services
    (list
     ;; (service nix-service-type)
     )
    #:home-services
    ;; TODO: move to feature-irc-settings
    (list
     (simple-service
      'i2pd-add-ilita-irc
      home-i2pd-service-type
      (home-i2pd-extension
       (tunnels-conf
        `((IRC-ILITA ((type . client)
                      (address . 127.0.0.1)
                      (port . 6669)
                      (destination . irc.ilita.i2p)
                      (destinationport . 6667)
                      (keys . ilita-keys.dat)))))))

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

   (feature-pipewire)
   (feature-backlight #:step 5)

   (feature-fonts
    #:font-monospace (font "Iosevka" #:size 11 #:weight 'regular)
    ;; #:font-monospace (font "Fira Mono" #:size 14 #:weight 'semi-light)
    #:font-packages (list font-iosevka font-fira-mono))

   (feature-alacritty
    #:config-file (local-file "./config/alacritty/alacritty.yml")
    #:default-terminal? #f
    #:backup-terminal? #t
    #:software-rendering? #f)
   (feature-vterm)
   (feature-tmux
    #:config-file (local-file "./config/tmux/tmux.conf"))
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-bash)
   (feature-direnv)
   (feature-git)
   (feature-ssh
    #:ssh-configuration
    (home-ssh-configuration
     (toplevel-options
      '((host-key-algorithms . "+ssh-rsa")
        (pubkey-accepted-key-types . "+ssh-rsa")))))

   (feature-sway
    #:xwayland? #f
    #:extra-config
    `((output DP-2 scale 2)
      (workspace 9 output DP-2)
      (workspace 10 output DP-2)

      (bindsym
       --locked $mod+Shift+p exec
       ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
       play-pause)
      ;; (input type:touchpad
      ;;            ((tap enabled)
      ;;             (natural_scroll enabled)))
      (bindsym $mod+Shift+Return exec emacs)))
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   ;; (feature-sway-statusbar
   ;;  #:use-global-fonts? #f)
   (feature-waybar
    #:waybar-modules
    (list
     (waybar-sway-workspaces)
     ;; (waybar-sway-window)
     (waybar-tray)
     (waybar-idle-inhibitor)
     ;; (waybar-temperature)
     (waybar-sway-language)
     (waybar-battery #:intense? #f)
     (waybar-clock)))
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config '(;; (screenshots)
                     ;; (effect-blur . 7x5)
                     (clock)))
   (feature-rofi)

   (feature-emacs
    #:extra-init-el `()
    #:additional-elisp-packages
    (append
     (list emacs-consult-dir)
     (pkgs "emacs-elfeed" "emacs-hl-todo"
           "emacs-ytdl"
           "emacs-ement"
           "emacs-restart-emacs"
           "emacs-org-present")))
   (feature-emacs-appearance)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #f)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-keycast #:turn-on? #f)

   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)
   (feature-emacs-message)
   (feature-emacs-erc
    #:erc-kill-buffers-on-quit #t
    #:erc-nick "abcdw"
    #:align-nicknames? #f
    #:erc-autojoin-channels-alist
    '((Libera.Chat "#guix" "#emacs" "#tropin" "#rde" "#sway")
      (OFTC        "#pipewire" "#wayland"))
    #:log? #t

    ;; #:erc-server "chat.sr.ht"
    #:extra-config
    `((setq rde-bouncer-network-alist
            `((irc.libera.chat . "abcdw")
              (irc.oftc.net . "abcdw")))
      (setq rde-bouncer-nick "abcdw")

      ;; Rename server buffers to reflect the current network name instead
      ;; of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
      ;; This is useful when using a bouncer like ZNC where you have multiple
      ;; connections to the same server.
      (setq erc-rename-buffers t)

      (defun rde-erc-connect-bouncer-oftc ()
        (interactive)
        (setq erc-email-userid "abcdw/irc.oftc.net")
        (erc-tls :server "chat.sr.ht" :nick rde-bouncer-nick))
      (defun rde-erc-connect-bouncer-libera ()
        (interactive)
        (setq erc-email-userid "abcdw/irc.libera.chat")
        (erc-tls :server "chat.sr.ht" :nick rde-bouncer-nick))))
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
   (feature-emacs-org-agenda
    #:org-agenda-files '("~/work/abcdw/agenda/todo.org"))
   (feature-markdown)

   (feature-mpv)
   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)
   (feature-notmuch
    #:extra-tag-updates-post
    '("notmuch tag +guix-home -- 'thread:\"\
{((subject:guix and subject:home) or subject:/home:/) and tag:new}\"'")
    #:notmuch-saved-searches
    (cons*
     '(:name "Work Inbox" :query "tag:work and tag:inbox" :key "W")
     '(:name "Personal Inbox" :query "tag:personal and tag:inbox" :key "P")
     '(:name "Guix Home Inbox" :key "H" :query "tag:guix-home and tag:unread")
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
      "figlet" ;; TODO: Move to emacs-artist-mode
      "calibre"
      "icecat" "nyxt"
      "ungoogled-chromium-wayland" "ublock-origin-chromium"

      "utox" "qtox" "jami"

      "alsa-utils" "youtube-dl" "imv" "cozy"
      "pavucontrol" "wev"
      "obs" "obs-wlrobs"
      "recutils" "binutils"
      "fheroes2"
      ;; TODO: Enable pipewire support to chromium by default
      ;; chrome://flags/#enable-webrtc-pipewire-capturer
      "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-standard"
      "papirus-icon-theme" "arc-theme"
      "thunar"
      ;; "glib:bin"

      ;; TODO: Fix telega package!
      "ffmpeg"
      "ripgrep" "curl")))))

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
(define-public ixy-os
  (rde-config-operating-system ixy-config))

(define ixy-he
  (rde-config-home-environment ixy-config))



(use-modules (gnu system file-systems))
(define live-file-systems
  (list (file-system
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
         ;; (list %pseudo-terminal-file-system
         ;;       %shared-memory-file-system
         ;;       %efivars-file-system
         ;;       %immutable-store)
         ))

(use-modules (gnu services))
(define-public live-config
  (rde-config
   (features
    (append
     %abcdw-features
     %main-features
     (list
      (feature-host-info
       #:host-name "gnu"
       #:timezone  "Europe/Moscow")

      (feature-file-systems
       #:file-systems live-file-systems)
      (feature-hidpi)
      (feature-custom-services
       #:feature-name-prefix 'live
       #:system-services
       (list
        (simple-service
         'channels-and-sources
         etc-service-type
         `(("channels.scm" ,(local-file "live-channels"))
           ("guix-sources" ,(local-file "/home/bob/work/gnu/guix"
                                        #:recursive? #t))
           ("rde-sources" ,(local-file "/home/bob/work/abcdw/rde"
                                       #:recursive? #t))))
        ;; (service
        ;;  guix-home-service-type
        ;;  `(("bob" . ,ixy-he)))
        (service
         gc-root-service-type
         (list ixy-he))
        )))))))

(define-public live-os
  (rde-config-operating-system live-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      ("live-system" live-os)
      (_ ixy-he))))

;; (pretty-print-rde-config ixy-config)
;; (use-modules (gnu services)
;; 	     (gnu services base))
;; (display
;;  (filter (lambda (x)
;; 	   (eq? (service-kind x) console-font-service-type))
;; 	 (rde-config-system-services ixy-config)))

;; (use-modules (rde features))
;; ((@ (ice-9 pretty-print) pretty-print)
;;  (map feature-name (rde-config-features ixy-config)))

(dispatcher)
