(define-module (rde examples abcdw configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features bluetooth)
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
  ;; #:use-module (gnu services nix)
  #:use-module (flat packages emacs)
  #:use-module (nongnu packages nvidia)
  #:use-module (rde examples abcdw emacs)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu packages)
  #:use-module (rde packages)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1))


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

(define my-org-directory "~/life")
(define my-notes-directory
  (string-append my-org-directory "/roam"))

(pretty-print "pre-%abcdw-features")
(define %abcdw-features
  (list
   (feature-user-info
    ;;#:emacs-advanced-user? #t
    #:user-name "samuel"
    #:full-name "Samuel Culpepper"
    #:email "samuel@samuelculpepper.com"
    #:user-groups '("lp")) ;; TODO confluence of features -> groups

   (feature-gnupg
     #:gpg-primary-key "EE20E25391AAB9BB"
     #:gpg-smart-card? #f)
   (feature-password-store)
   (feature-mail-settings
    #:mail-accounts
    (list (mail-account
           (id   'personal)
           (fqda "samuel@samuelculpepper.com")
           (type 'bravehost))
          (mail-account
           (id   'work)
           (fqda "sculpepper@newstore.com")
           (type 'gmail)))
    #:mailing-lists
    (list
     (mail-lst 'python-speed "speed@python.org"
               '("https://mail.python.org/mailman/listinfo/speed"
                 "https://mail.python.org/archives/list/speed@python.org/"))

     (mail-lst 'rde-announce "~acbdw/rde-announce@lists.sr.ht"
               '("https://lists.sr.ht/~abcdw/rde-announce/export"))
     (mail-lst 'rde-discuss "~acbdw/rde-discuss@lists.sr.ht"
               '("https://lists.sr.ht/~abcdw/rde-discuss"))
     (mail-lst 'rde-devel "~acbdw/rde-devel@lists.sr.ht"
               '("https://lists.sr.ht/~abcdw/rde-devel"))

     (mail-lst 'guix-bugs "guix-bugs@gnu.org"
               '("https://yhetil.org/guix-bugs/0"))
     (mail-lst 'guix-devel "guix-devel@gnu.org"
               '("https://yhetil.org/guix-devel/0"))
     (mail-lst 'guix-patches "guix-patches@gnu.org"
               '("https://yhetil.org/guix-patches/1"))))

   (feature-keyboard
    #:keyboard-layout %thinkpad-layout)))

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

(use-modules (gnu services)
             (gnu services databases)
             (gnu services desktop))

(pretty-print "pre-%main-features")
;;; WARNING: The order can be important for features extending
;;; services of other features.  Be careful changing it.
(define %main-features
  (list
   (feature-custom-services
    #:feature-name-prefix 'ixy
    #:system-services
    (list
     ;; (service nix-service-type)
     )
    #:home-services
    (list
     ;; TODO: Remove it once upstreamed.
     ((@ (gnu services) simple-service)
      'make-guix-aware-of-guix-home-subcomand
      (@ (gnu home services) home-environment-variables-service-type)
      '(
        ;;; GRAPHICS
        ("GBM_BACKEND" . "nvidia-drm")
        ("GBM_BACKENDS_PATH" . "/gnu/store/k52jklliyks6sjhp8w44by7qph73y2rw-nvidia-driver-495.46/lib/gbm")
        ("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
        ("WLR_NO_HARDWARE_CURSORS" . "1")
        ;;("WLR_DRM_DEVICES" . "/dev/dri/card1")   ;; gpu only
        ;;("WLR_DRM_DEVICES" . "/dev/dri/card1") ;; cpu only
        ;;("WLR_DRM_DEVICES" . "/dev/dri/card0:/dev/dri/card1") ;; gpu:cpu

        ;;; GUILE
        ("GUILE_LOAD_PATH" .
         "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0\
:$GUILE_LOAD_PATH")
        ("GUILE_LOAD_COMPILED_PATH" .
         "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache\
:$GUILE_LOAD_COMPILED_PATH")

        ;;; JS/BABEL
        ;; javascript sucks, npm sucks
        ;; https://github.com/npm/npm/issues/6675#issuecomment-250318382
        ;; https://github.com/npm/cli/issues/1451
        ;; https://github.com/pnpm/pnpm/issues/2574
        ;; https://github.com/rxrc/zshrc/blob/3ca83703da5bd93b015747835a8a0164160c9b83/env.zsh#L33-L928
        ("NPM_CONFIG_USERCONFIG" . "${XDG_CONFIG_HOME}/npm/config")
        ("NPM_CONFIG_CACHE" . "${XDG_CACHE_HOME}/npm")
        ("NPM_CONFIG_TMP" . "${XDG_RUNTIME_DIR}/npm")
        ("YARN_CACHE_FOLDER" . "${YARN_CACHE_FOLDER:-$XDG_CACHE_HOME/yarn}")
        ("NODE_REPL_HISTORY" . "${NODE_REPL_HISTORY:-$XDG_CACHE_HOME/node/repl_history}")
        ("NVM_DIR" . "${NVM_DIR:-$XDG_DATA_HOME/nvm}")
        ("BABEL_CACHE_PATH" . "${BABEL_CACHE_PATH:-$XDG_CACHE_HOME/babel/cache.json}")

        ;;; DEVELOPMENT
        ("GUIX_CHECKOUT" . "$HOME/git/sys/guix")
        ("GUIX_EXTRA_PROFILES" . "$HOME/.guix-extra-profiles")

        ;;; ETC
        ("GDK_BACKEND" . "wayland") ;; ... for clipboarding emasc
        ("PATH" . (string-join (list "$PATH"
                                     "$HOME/.local/bin"
                                     "$HOME/.krew/bin"
                                     "${XDG_CACHE_HOME}/npm/bin")
                               ":"))))
     ;; ((@ (gnu services) simple-service)
     ;;  'extend-shell-profile
     ;;  (@ (gnu home-services shells) home-shell-profile-service-type)
     ;;  (list
     ;;   #~(string-append
     ;;      "alias superls="
     ;;      #$(file-append (@ (gnu packages base) coreutils) "/bin/ls"))))

     ;; see logs at ~/.local/var/log/mcron.log
     ;;   tail --follow ~/.local/var/log/mcron.log
     ;; ((@ (gnu services) simple-service)
     ;;  'notes-commit-job
     ;;  (@ (gnu home services mcron) home-mcron-service-type)
     ;;  (list #~(job '(next-minute)
     ;;               (lambda ()
     ;;                 (system* "echo \"$(date -u) attempting to commit\" >> /tmp/commit-log")
     ;;                 (system* "cd $HOME/life")
     ;;                 (system* "git add .")
     ;;                 (system* "git commit -m \"auto-commit | $(date -u)\""))
     ;;               "notes-commit")))

     )
    #:system-services
    (list (service postgresql-service-type)
          (service postgresql-role-service-type
                   (postgresql-role-configuration
                    (roles (list (postgresql-role
                                  (name "postgres")
                                  (create-database? #t))
                                 (postgresql-role
                                  (name "samuel")
                                  (permissions '(superuser))
                                  (create-database? #t))
                                 (postgresql-role
                                  (name "newstore")
                                  (create-database? #t))))))
          ))

   (feature-base-services)
   (feature-desktop-services)
   (feature-docker)

   (feature-pipewire)
   (feature-backlight)

   (feature-fonts
    #:font-monospace (font "Iosevka" #:size 14 #:weight 'regular)
    ;; #:font-monospace (font "Fira Mono" #:size 14 #:weight 'semi-light)
    #:font-packages (list font-iosevka font-fira-mono))

   (feature-alacritty
     #:config-file (local-file "./config/alacritty/alacritty.yml"))
   (feature-zsh
    #:extra-zshrc
    (list ;; XXX higher level category
     ;; something which evals equiv to following for each promptline "PS1=\"[$(date -u '+%Y-%m-%d | %H:%M')] $PS1\""
     "alias ns='cd $HOME/git/ns'"
     "alias om='ns && cd om'"
     "alias omom='om && cd om'"
     "alias rt='ns && cd routing'"
     "alias sys='cd $HOME/git/sys'"

     ;; TIL https://unix.stackexchange.com/questions/225943/except-the-1st-argument
     "rgw() { d=$1; p=$2; argv[1,2]=(); rg $p $d $@; }"
     "alias rgg='rgw $HOME/git/'"
     "alias rgr='rgw $HOME/git/sys/rde'"
     "alias rgns='rgw $HOME/git/ns'"
     "alias rgom='rgw $HOME/git/ns/om'"
     "alias rgrt='rgw $HOME/git/ns/routing'"
     "alias rgsys='rgw $HOME/git/sys'"

     "alias gp='ls $GUIX_EXTRA_PROFILES'"
     "_gP() { export GUIX_PROFILE=$1 ; }"
     "alias gP='_gP'"
     "_gsP() { . $GUIX_EXTRA_PROFILES/$1/$1 ; }"
     "gsP=_gsP"
     ;; (string-append
     ;;  "export PATH=\""
     ;;  (string-join (list "$PATH"
     ;;                     "$HOME/.local/bin"
     ;;                     "$HOME/.krew/bin"
     ;;                     "${XDG_CACHE_HOME}/npm/bin") ":")
     ;;  "\"")
     ))
   (feature-ssh
    #:ssh-configuration
    (home-ssh-configuration
     (default-options
      '((hostkey-algorithms . "+ssh-rsa")
        (pubkey-accepted-algorithms "+ssh-rsa")))
     (extra-config
      (list (ssh-host
             (host "qz")
             (options '((user . "samuel")
                        (hostname . "192.168.0.249")
                        (port . 22)
                        ;;(identity-file . "~/.ssh/newstore-luminate.pem")
                        )))
            (ssh-host
             (host "ko")
             (options '((user . "root")
                        (hostname . "192.168.0.240")
                        (port . 2222)
                        (identity-file . "~/.ssh/ko.pub"))))
            (ssh-host
             (host "bastion-sandbox")
             (options '((user . "ubuntu@bastion-sandbox")
                        (hostname . "bastion-sandbox.ssh.newstore.luminatesec.com")
                        (port . 22)
                        (identity-file . "~/.ssh/newstore-luminate.pem"))))
            (ssh-host
             (host "bastion-staging")
             (options '((user . "ubuntu@bastion-staging")
                        (hostname . "bastion-staging.ssh.newstore.luminatesec.com")
                        (port . 22)
                        (identity-file . "~/.ssh/newstore-luminate.pem"))))
            (ssh-host
             (host "bastion-production")
             (options '((user . "ubuntu@bastion-production")
                        (hostname . "bastion-production.ssh.newstore.luminatesec.com")
                        (port . 22)
                        (identity-file . "~/.ssh/newstore-luminate.pem"))))))))
   (feature-git)
   ;; #:package sway-latest
   (feature-sway
    #:xwayland? #f
    #:opacity 0.9
    #:wallpaper "$HOME/.cache/wallpaper.png"
    #:extra-config
    `(;;(include ,(local-file "./config/sway/config"))
      ;; TODO sway: toggle opacity for WINDOW
      (,#~"output DP-1 res 5120x1440 bg ~/.cache/wallpaper.png fill")
      ;; TODO sway: wacom input rotation matrix
      (,#~"input \"*\" tool_mode \"*\" relative calibration_matrix 0.0 -1.0 1.0 1.0 0.0 0.0")
      ;; danke demis https://github.com/minikN/guix/blob/ca15b5a5954d50fe75e2b03f21afc019e002022b/config.scm#L173
      (for_window "[app_id=\"pavucontrol\"]" floating enable, border pixel)
      (for_window "[app_id=\"pinentry-qt\"]" floating enable, border pixel)
      )
    )
   (feature-sway-run-on-tty
    #:sway-tty-number 2
    ;;#:launch-args "--unsupported-gpu" ;; 1.7-rc1+ https://github.com/swaywm/sway/releases/tag/1.7-rc1
    #:launch-args "--my-next-gpu-wont-be-nvidia --debug &>/tmp/sway")
   (feature-sway-screenshot)
   (feature-sway-statusbar
    #:use-global-fonts? #f)
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    #:extra-config '((screenshots)
                     (effect-blur . 7x5)
                     (clock)))
   (feature-rofi)
   (feature-direnv)
   (feature-emacs
    #:extra-init-el
    (append
      (list #~"(define-key key-translation-map [?\\C-x] [?\\C-u])\n"
            #~"(define-key key-translation-map [?\\C-u] [?\\C-x])\n")
      init-el)
    #:additional-elisp-packages
    ;; TODO if feature-emacs-PACKAGE exists, advise its use
    (append
     (list emacs-consult-dir
           emacs-consult-eglot
           emacs-consult-recoll)
     (pkgs "emacs-elfeed"
           "emacs-hl-todo"
           "emacs-ytdl"
           "emacs-dimmer"
           "emacs-hyperbole"
           "emacs-ement"
           "emacs-restart-emacs"
           "emacs-org-fragtog"
           "emacs-yaml-mode"
           "emacs-org-download"
           "emacs-org-edit-latex"
           "emacs-guix"
           "emacs-forge"
           "emacs-debbugs"
           "emacs-ob-async"
           "emacs-plantuml-mode"
           "emacs-org-fragtog"

           "emacs-explain-pause-mode"
           ;; TODO feature-emacs-lsp
           "emacs-eglot"
           "emacs-lsp-ui"
           "emacs-lsp-mode"
           "emacs-python-black"
           "emacs-py-isort"
           "emacs-gnuplot"
           "emacs-protobuf-mode"
           "emacs-go-mode"
           "emacs-eros"
           "emacs-string-inflection"
           "emacs-htmlize" ;; ement: -> ox-export html: org src blocks
           ;; emacs-impostman
           ;; "emacs-org-autotangle"
           )))

   (feature-emacs-appearance #:light? #f)
   (feature-emacs-faces)
   (feature-emacs-completion
    #:mini-frame? #f)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-keycast)
   (feature-emacs-perfect-margin
    #:visible-width 150)

   (feature-emacs-dired)
   (feature-emacs-vterm)
   (feature-emacs-monocle)
   (feature-emacs-message)
   (feature-emacs-erc
    #:erc-nick "qzdl"
    #:erc-autojoin-channels-alist
    '(("irc.libera.chat" "#guix" "#emacs" "#tropin" "#systemcrafters")
      ("irc.oftc.net"    "#pipewire" "#wayland")))
   (feature-emacs-elpher)
   (feature-emacs-telega)
   (feature-emacs-pdf-tools)

   ;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
   (feature-emacs-git)
   ;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>

   (feature-emacs-org
    #:org-directory my-org-directory
    #:org-agenda-directory my-notes-directory)

   (feature-emacs-org-roam
    ;; TODO: Rewrite to states
   ;; (feature-emacs-org-agenda
   ;; #:org-agenda-files '("~/work/abcdw/agenda/todo.org"))
    #:org-roam-directory my-notes-directory
    #:org-roam-dailies-directory (string-append my-notes-directory "/daily"))
   ;; (feature-emacs-ref
   ;;  ;; why error with nil for reftex-default-bibliography
   ;;  ;; TODO: Rewrite to states
   ;;  #:bibliography-paths
   ;;  (list (string-append my-org-directory "/tex.bib"))
   ;;  #:bibliography-notes
   ;;  (list(string-append my-org-directory "/bib.org")
   ;;  #:bibliography-directory my-notes-directory)

   (feature-emacs-es-mode
    #:package emacs-es-mode)
   (feature-emacs-restclient
    #:package-ob emacs-ob-restclient)
   (feature-mpv)
   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp
    #:msmtp msmtp-latest)
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
   (feature-bluetooth #:auto-enable? #t)
   (feature-base-packages
    #:home-packages
    (append
     ;; (pkgs-vanilla
     ;;  "icecat" "nyxt"
     ;;  "ungoogled-chromium-wayland" "ublock-origin-chromium")
     (pkgs
      "alsa-utils" "mpv" "youtube-dl" "imv" "vim"
      "cozy" "pavucontrol"
      "wev"
      "obs" "obs-wlrobs"
      "recutils"
      "fheroes2"
      ;; TODO: Enable pipewire support to chromium by default
      ;; chrome://flags/#enable-webrtc-pipewire-capturer
      "ungoogled-chromium-wayland" "ublock-origin-chromium"
      "nyxt"
      ;;
      "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-standard"
      "ripgrep" "curl" "make"
      ;;; nonguix
      ;;"firefox"
      )))))
(pretty-print "post-%main-features")



;;; Hardware/host specific features

;; TODO: Switch from UUIDs to partition labels For better
;; reproducibilty and easier setup.  Grub doesn't support luks2 yet.

(define ixy-mapped-devices
  (list (mapped-device
         (source (uuid "cb453366-cc17-4742-ada1-91f7f569103f"))
         (target "sys-root")
         (type luks-device-mapping))))

(define ixy-file-systems
  (list (file-system
           (device (file-system-label "sys-root"))
           (mount-point "/")
           (type "ext4")
           (dependencies ixy-mapped-devices))
         (file-system
           (device "/dev/nvme0n1p1")
           (mount-point "/boot/efi")
           (type "vfat"))
         ))


;; (define ixy-file-systems
;;   (append
;;    (map (match-lambda
;; 	  ((subvol . mount-point)
;; 	   (file-system
;; 	     (type "btrfs")
;; 	     (device "/dev/mapper/enc")
;; 	     (mount-point mount-point)
;; 	     (options (format #f "subvol=~a" subvol))
;; 	     (dependencies ixy-mapped-devices))))
;; 	'((root . "/")
;; 	  (boot . "/boot")
;; 	  (gnu  . "/gnu")
;; 	  (home . "/home")
;; 	  (data . "/data")
;; 	  (log  . "/var/log")))
;;    (list
;;     (file-system
;;       (mount-point "/boot/efi")
;;       (type "vfat")
;;       (device (uuid "8C99-0704" 'fat32))))))

(use-modules
 (gnu packages linux)
 ((nongnu packages linux) #:prefix nongnu:)
 ((nongnu system linux-initrd) #:prefix nongnu-sys:))

(define %ixy-features
  (list
   (feature-host-info
    #:host-name "ixy"
    #:timezone  "Europe/Berlin")
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   ;; (feature-bootloader)
   ;; os
   (feature-kernel
    #:kernel nongnu:linux-lts
    #:kernel-arguments
    '("quiet" "ipv6.disable=1" "net.ifnames=0"
      ;; https://forums.developer.nvidia.com/t/nvidia-495-on-sway-tutorial-questions-arch-based-distros/192212
       "nvidia-drm.modeset=1" "nouveau.blacklist=1" "modprobe.blacklist=nouveau"
      )
    ;; removed "modprobe.blacklist=snd_hda_intel,snd_soc_skl"
    #:firmware (list nongnu:linux-firmware
                     nongnu:sof-firmware
                     nvidia-driver)
    #:initrd nongnu-sys:microcode-initrd
    #:kernel-loadable-modules (list v4l2loopback-linux-module
                                    nvidia-driver))
   (feature-file-systems
    #:mapped-devices ixy-mapped-devices
    #:file-systems   ixy-file-systems)
   ;(feature-hidpi)
   ))


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
;;  ((@@ (ice-9 pretty-print) pretty-print)
;;   (map feature-name (rde-config-features ixy-config)))

(pretty-print "pre-dispatch")
(dispatcher)


