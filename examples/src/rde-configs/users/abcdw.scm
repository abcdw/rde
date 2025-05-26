(define-module (rde-configs users abcdw)
  #:use-module (contrib features javascript)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (rde features android)
  #:use-module (rde features base)
  #:use-module (rde features clojure)
  #:use-module (rde features containers)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features gnupg)
  #:use-module (rde features gtk)
  #:use-module (rde features irc)
  #:use-module (rde features keyboard)
  #:use-module (rde features libreoffice)
  #:use-module (rde features llm)
  #:use-module (rde features mail)
  #:use-module (rde features markup)
  #:use-module (rde features networking)
  #:use-module (rde features ocaml)
  #:use-module (rde features password-utils)
  #:use-module (rde features presets)
  #:use-module (rde features security-token)
  #:use-module (rde features sourcehut)
  #:use-module (rde features system)
  #:use-module (rde features terminals)
  #:use-module (rde features uml)
  #:use-module (rde features version-control)
  #:use-module (rde features video)
  #:use-module (rde features virtualization)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features)
  #:use-module (rde home services emacs)
  #:use-module (rde home services i2p)
  #:use-module (rde home services shells)
  #:use-module (rde home services video)
  #:use-module (rde home services wm)
  #:use-module (rde packages aspell)
  #:use-module (rde packages)
  #:use-module (srfi srfi-1))


;;; Helpers

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


;;; Service extensions

(define emacs-extra-packages-service
  (simple-service
   'emacs-extra-packages
   home-emacs-service-type
   (home-emacs-extension
    (init-el
     `((with-eval-after-load 'org
         (setq org-use-speed-commands t)
         (setq org-enforce-todo-dependencies t)
         ;; (setq org-enforce-todo-checkbox-dependencies t)
         (setq org-log-reschedule 'time)
         (defun rde-org-goto-end-of-heading ()
           (interactive)
           (org-end-of-meta-data t)
           (end-of-line)
           (unless (bolp)
             (newline)))
         (define-key org-mode-map (kbd "M-o") 'rde-org-goto-end-of-heading))

       (with-eval-after-load 'geiser-mode
         (setq geiser-mode-auto-p nil)
         (defun abcdw-geiser-connect ()
           (interactive)
           (geiser-connect 'guile "localhost" "37146"))

         (define-key geiser-mode-map (kbd "C-c M-j") 'abcdw-geiser-connect))

       (with-eval-after-load 'simple
         (global-page-break-lines-mode 1)
         (setq-default display-fill-column-indicator-column 80)
         (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

       (setq copyright-names-regexp
             (format "%s <%s>" user-full-name user-mail-address))
       (add-hook 'after-save-hook (lambda () (copyright-update nil nil)))))
    (elisp-packages
     (append
      (list
       ;; (@ (rde packages emacs-xyz) emacs-corfu-candidate-overlay)
       )
      (strings->packages
       ;; "emacs-dirvish"
       "emacs-elixir-mode"
       "emacs-company-posframe"
       "emacs-wgrep"
       "emacs-ox-haunt"
       "emacs-haskell-mode"
       "emacs-rainbow-mode"
       "emacs-hl-todo"
       "emacs-yasnippet"
       ;; "emacs-xkb-mode"
       ;; "emacs-consult-dir"
       "emacs-kind-icon"
       "emacs-nginx-mode" "emacs-yaml-mode"
       "emacs-multitran"
       "emacs-minimap"
       "emacs-ement"
       "emacs-restart-emacs"
       "emacs-org-present"))))))

(define home-extra-packages-service
  (simple-service
   'home-profile-extra-packages
   home-profile-service-type
   (append
    (list
     (@ (gnu packages tree-sitter) tree-sitter-clojure)
     (@ (gnu packages tree-sitter) tree-sitter-html)
     (@ (gnu packages guile) guile-next))
    (strings->packages
     "figlet" ;; TODO: Move to emacs-artist-mode
     ;; "calibre"
     ;; "nyxt"

     "dino"
     "libnotify"
     "utox" "qtox"
     ;; "jami"

     "flatpak"

     "python-yubikey-manager"

     "alsa-utils"
     ;; "cozy"
     "pavucontrol" "wev"
     "imagemagick"
     "obs" "obs-wlrobs"
     "recutils" "binutils" "make" "gdb"
     "fheroes2" "sdlpop"

     "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
     ;; "papirus-icon-theme" ; 85k files
     "arc-theme"
     "fd"
     "nautilus"
     "qbittorrent"
     "kdenlive"
     ;; "glib:bin"

     ;; "ffmpeg"
     "ripgrep" "curl"))))

(define (wallpaper url hash)
  (origin
    (method url-fetch)
    (uri url)
    (file-name "wallpaper.png")
    (sha256 (base32 hash))))

(define wallpaper-ai-art
  (wallpaper "https://w.wallhaven.cc/full/j3/wallhaven-j3m8y5.png"
             "0qqx6cfx0krlp0pxrrw0kvwg6x40qq9jic90ln8k4yvwk8fl1nyw"))

(define wallpaper-dark-rider
  (wallpaper "https://w.wallhaven.cc/full/lm/wallhaven-lmlzwl.jpg"
             "01j5z3al8zvzqpig8ygvf7pxihsj2grsazg9yjiqyjgsmp00hpaf"))



(define sway-extra-config-service
  (simple-service
   'sway-extra-config
   home-sway-service-type
   `((output HDMI-A-1 scale 2)
     (output DP-2 scale 2)
     ;; (gaps bottom 90)
     ;; (output * bg ,wallpaper-ai-art center)
     ;; (output eDP-1 disable)
     ,@(map (lambda (x) `(workspace ,x output DP-2)) (iota 8 1))

     ,@(append-map
        (lambda (x)
          `(;; (bindsym --to-code ,(format #f "$mod+~a" (modulo x 10))
            ;;          workspace number ,x)
            (bindsym --to-code ,(format #f "$mod+Control+~a" (modulo x 10))
                     move container to workspace number ,x)))
        (iota 10 1))
     ;; (workspace 9 output DP-2)
     ;; (workspace 10 output DP-2)

     ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)

     (bindsym
      --locked Pause exec
      ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
      play-pause)

     (bindsym
      --locked $mod+Alt+n exec
      ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
      next)

     (bindsym $mod+Shift+o move workspace to output left)
     (bindsym $mod+Ctrl+o focus output left)
     (input type:touchpad
            ;; TODO: Move it to feature-sway or feature-mouse?
            ((natural_scroll enabled)
             (tap enabled)))

     ;; (xwayland disable)
     (bindsym $mod+Shift+Return exec emacs))))

(define mpv-add-user-settings-service
  (simple-service
   'mpv-add-user-settings
   home-mpv-service-type
   (home-mpv-extension
    (mpv-conf
     `((global
        ((keep-open . yes)
         (ytdl-format . "bestvideo[height<=?720][fps<=?30][vcodec!=?vp9]+bestaudio/best
")
         (save-position-on-quit . yes)
         (speed . 1.61))))))))

(define i2pd-add-ilita-irc-service
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
                   (keys . ilita-keys.dat))))))))

(define ssh-extra-config-service
  (simple-service
   'ssh-extra-config
   home-ssh-service-type
   (home-ssh-extension
    (extra-config
     (append
      ;; TODO: Move it feature-qemu?
      (map (lambda (id)
             (ssh-host
              (host (format #f "qemu~a" id))
              (options
               `((host-name . "localhost")
                 (port . ,(+ 10020 id))))))
           (iota 4))
      (list
       (ssh-host
        (host "*.cons.town")
        (options
         '((user . "root")
           (port . 22)
           (compression . #t))))
       (ssh-host
        (host "pinky-ygg")
        (options
         '((host-name . "200:554d:3eb1:5bc5:6d7b:42f4:8792:efb8")
           (port . 50621)
           ;; (control-master . "auto")
           ;; (control-path . "~/.ssh/master-%r@%h:%p")
           (compression . #t))))
       (ssh-host
        (host "pinky-wg")
        (options
         '((host-name . "10.0.0.1")
           (port . 50621)
           (compression . #t))))
       (ssh-host
        (host "pinky")
        (options
         '((host-name . "23.137.249.202")
           (port . 50621)
           (compression . #t)))))))
    (toplevel-options
     '((host-key-algorithms . "+ssh-rsa")
       (pubkey-accepted-key-types . "+ssh-rsa"))))))

(define rde-guix-add-to-shell-profile-service
  (simple-service
   'rde-guix-add-to-shell-profile
    home-shell-profile-service-type
   (list "
GUIX_PROFILE=/data/abcdw/work/abcdw/rde/examples/target/profiles/guix
if [ -f $GUIX_PROFILE/etc/profile ]; then source $GUIX_PROFILE/etc/profile; fi
")))

(define (feature-additional-services)
  (feature-custom-services
   #:feature-name-prefix 'abcdw
   #:home-services
   (list
    emacs-extra-packages-service
    home-extra-packages-service
    sway-extra-config-service
    ssh-extra-config-service
    i2pd-add-ilita-irc-service
    mpv-add-user-settings-service
    rde-guix-add-to-shell-profile-service)))

;;; User-specific features with personal preferences

;; Initial user's password hash will be available in store, so use this
;; feature with care (display (crypt "hi" "$6$abc"))

(define dev-features
  (list
   (feature-markdown)))

(define virtualization-features
  (list
   (feature-podman)
   (feature-distrobox)
   (feature-qemu)))

(define general-features
  (append
   rde-base
   rde-desktop
   rde-mail
   rde-cli
   rde-emacs))

(define keyboard-features
  (list
   (feature-swaykbdd)))

(define %all-features
  (append
   keyboard-features
   virtualization-features
   dev-features
   general-features))

;; In fact, kernel customization fits hosts/ submodule better, but it's ok to
;; have it here for demonstration purpose.
(define example-firmware (@ (gnu packages firmware) ath9k-htc-ar7010-firmware))

;; To override default features obtained from (rde presets) just remove them
;; from the list and add them back with customizations needed.
(define all-features-with-custom-kernel-and-substitutes
  (append
   ;; "C-h S" (info-lookup-symbol), "C-c C-d C-i" (geiser-doc-look-up-manual)
   ;; to see the info manual for a particular function.

   ;; Here we basically remove all the features which has feature name equal
   ;; to either 'base-services or 'kernel.
   (remove (lambda (f)
             (member
              (feature-name f)
              '(base-services
                kernel
                swaylock
                xdg
                git)))
           %all-features)
   (list
    (feature-git)
    (feature-kernel
     #:kernel-arguments '("snd_hda_intel.dmic_detect=0")
     #:firmware (list example-firmware))
    (feature-swaylock
     ;; swaylock-effects often crashes with red screen
     ;; #:swaylock (@ (gnu packages wm) swaylock-effects)
     ;; The blur on lock screen is not privacy-friendly.
     ;; #:extra-config '((screenshots)
     ;;                  (effect-blur . 7x5)
     ;;                  (clock))
     )
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


    (feature-base-services
     #:default-substitute-urls (list "https://bordeaux.guix.gnu.org"
                                     "https://ci.guix.gnu.org")))))

(define-public %abcdw-features
  (append
   all-features-with-custom-kernel-and-substitutes
   (list
    (feature-additional-services)
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
     #:ssh-keys '(("58AAE5966479124A357F7D6B9D710EBA1C24E10E")))
    (feature-security-token)
    (feature-password-store
     #:password-store-directory "/data/abcdw/password-store"
     #:remote-password-store-url "ssh://abcdw@olorin.lan/~/state/password-store")

    (feature-mail-settings
     #:mail-directory-fn (const "/data/abcdw/mail")
     #:mail-accounts (list
                      (mail-account
                       (id 'work)
                       (type 'migadu)
                       (fqda "andrew@trop.in")
                       (aliases '("admin@trop.in" "postmaster@trop.in"))
                       (pass-cmd "pass show mail/work"))
                      (mail-account
                       (id 'personal)
                       (type 'migadu)
                       (fqda "mail@trop.in")
                       (pass-cmd "pass show mail/personal")))
     #:mailing-lists (list (mail-lst 'guile-devel "guile-devel@gnu.org"
                                     '("https://yhetil.org/guile-devel/0"))
                           (mail-lst 'guix-devel "guix-devel@gnu.org"
                                     '("https://yhetil.org/guix-devel/0"))
                           (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                     '("https://yhetil.org/guix-bugs/0"))
                           (mail-lst 'guix-patches "guix-patches@gnu.org"
                                     '("https://yhetil.org/guix-patches/1"))))

    (feature-irc-settings
     #:irc-accounts (list
                     (irc-account
                      (id 'srht)
                      (network "chat.sr.ht")
                      (bouncer? #t)
                      (nick "abcdw"))
                     (irc-account
                      (id 'libera)
                      (network "irc.libera.chat")
                      (nick "abcdw"))
                     (irc-account
                      (id 'oftc)
                      (network "irc.oftc.net")
                      (nick "abcdw"))))

    (feature-ssh-proxy  #:host "pinky-ygg" #:auto-start? #f)
    (feature-ssh-tunnel #:host "pinky-ygg" #:name "pinky-web-server"
                        #:auto-start? #t)

    (feature-foot)
    (feature-yggdrasil)
    (feature-i2pd
     #:outproxy 'http://acetone.i2p:3128
     ;; 'purokishi.i2p
     #:less-anonymous? #t)

    (feature-emacs-keycast #:turn-on? #t)

    (feature-emacs-tempel
     #:default-templates? #t
     #:templates
     `(fundamental-mode
       ,#~""
       (t (format-time-string "%Y-%m-%d"))
       (todo
        (if (derived-mode-p 'lisp-data-mode 'clojure-mode 'scheme-mode)
            ";;"
            comment-start)
        (if (string-suffix-p " " comment-start) "" " ")
        "TODO"  ": [" user-full-name ", "
        (format-time-string "%Y-%m-%d") "] ")
       ;; TODO: Move to feature-guix
       ;; ,((@ (rde gexp) slurp-file-like)
       ;;   (file-append ((@ (guix packages) package-source)
       ;;                 (@ (gnu packages package-management) guix))
       ;;                "/etc/snippets/tempel/text-mode"))
       ))
    (feature-emacs-time)
    (feature-emacs-spelling
     #:spelling-program (@ (gnu packages hunspell) hunspell)
     #:spelling-dictionaries
     (list
      (@ (gnu packages hunspell) hunspell-dict-en)
      (@ (rde packages aspell) hunspell-dict-ru)))
    (feature-emacs-git
     #:project-directory "/data/abcdw/work")
    ;; https://plaindrops.de/blog/2020/GTDorgmode/
    ;; https://www.labri.fr/perso/nrougier/GTD/index.html#org2d62325
    (feature-emacs-org
     #:org-directory "/data/abcdw/work/abcdw/private"
     #:org-indent? #f
     #:org-capture-templates
     ;; https://libreddit.tiekoetter.com/r/orgmode/comments/gc76l3/org_capture_inside_notmuch/
     `(("r" "Reply" entry (file+headline "" "Tasks")
        "* TODO %:subject %?\nSCHEDULED: %t\n%U\n%a\n"
        :immediate-finish t)
       ("t" "Todo" entry (file+headline "" "Tasks") ;; org-default-notes-file
        "* TODO %?\nSCHEDULED: %t\n%a\n")
       ("p" "PhD Todo" entry
        (file+headline "/data/abcdw/work/abcdw/private/phd.org" "Tasks")
        "* TODO %?\nSCHEDULED: %t\n%a\n")))
    (feature-emacs-org-roam
     ;; TODO: Rewrite to states
     #:org-roam-directory "/data/abcdw/work/abcdw/notes/notes")
    (feature-emacs-org-dailies
     #:encrypted? #t)

    (feature-emacs-org-agenda
     ;; Add efforts to agenda, it's a bit space hungry, so not included in
     ;; feature by default yet.
     #:org-agenda-prefix-format
     '((agenda . " %i %-12:c%-6e%?-12t% s")
       (todo . " %i %-12:c %-6e")
       (tags . " %i %-12:c")
       (search . " %i %-12:c"))
     #:org-agenda-files '("/data/abcdw/work/abcdw/private/todo.org"
                          "/data/abcdw/work/abcdw/private/phd.org"))
    (feature-emacs-elfeed
     #:elfeed-org-files '("/data/abcdw/work/abcdw/private/rss.org"))

    (feature-android)
    (feature-javascript)
    (feature-ocaml #:opam? #t)

    (feature-emacs-piem
     #:piem-inboxes '(("rde-devel"
                       :url "https://lists.sr.ht/~abcdw/rde-devel"
                       :address "~abcdw/rde-devel@lists.sr.ht"
                       :coderepo ("~/work/abcdw/rde/"
                                  "~/work/abcdw/emacs-arei/"
                                  "~/work/abcdw/guile-ares-rs/"))
                      ("guix-devel"
                       :url "https://yhetil.org/guile-devel/"
                       :address "guile-devel@gnu.org"
                       :coderepo "~/work/gnu/guile/")
                      ("guix-patches"
                       :url "https://yhetil.org/guix-patches/"
                       :address "guix-patches@gnu.org"
                       :coderepo "~/work/gnu/guix/")))
    ;; TODO: move feature to general, move extra configuration to service.
    (feature-notmuch
     #:notmuch-queries
     '((rde-all . "to:\"rde-devel\" or to:\"rde-discuss\" or tag:rde")
       (rde-inbox . "query:rde-all and tag:inbox"))
     #:extra-tag-updates-post
     '("notmuch tag +guix-home +inbox -- 'thread:\"\
{((subject:guix and subject:home) or (subject:service and subject:home) or \
subject:/home:/) and tag:new}\"'"
       "notmuch tag +rde +list -- 'to:.*rde.*@lists.sr.ht and tag:new}'")
     #:notmuch-saved-searches
     (append
      ;; TODO: [Andrew Tropin, 2024-01-07] Archive replied emails automatically
      '((:name "To Process"
         :query "tag:todo or (tag:inbox and not tag:unread and not tag:replied)"
         :key "t")
        (:name "Drafts" :query "tag:draft" :key "d")
        (:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "w")
        (:name "RDE Inbox"
         :query "query:rde-inbox" :key "ir")
        (:name "RDE All"
         :query "query:rde-all" :key "pr")
        (:name "Project Debugger: RDE Internship 2025"
         :query "rde internship or tag:guile-debugger" :key "pd")
        (:name "Project Suitbl"
         :query "to: 2024-10-272@NLnet.nl or tag:suitbl" :key "ps")
        (:name "Work Inbox (Unsorted)"
         :query "(tag:work and tag:inbox) and not query:rde-all"
         :key "iu")
        (:name "Work Inbox"
         :query "tag:work and tag:inbox"
         :key "iw")
        (:name "Personal Inbox"
         :query "tag:personal and tag:inbox"
         :key "ip")
        (:name "Guix Home Inbox" :key "H" :query "tag:guix-home and tag:unread"))
      ;; %rde-notmuch-saved-searches
      '()))

    (feature-sourcehut
     #:user-name-fn (const "abcdw"))
    (feature-yt-dlp)

    (feature-plantuml)
    (feature-clojure)
    (feature-libreoffice)

    ;; TODO: Remove auctex dependency, which interjects in texinfo-mode.
    (feature-emacs-citation
     #:global-bibliography
     (list "/data/abcdw/work/abcdw/notes/bibliography.bib"))

    (feature-ellama)
    (feature-emacs-cua)
    (feature-keyboard
     ;; To get all available options, layouts and variants run:
     ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
     ;; To get a list of symbols and actions:
     ;; cat `guix build xorgproto`/include/X11/keysymdef.h
     #:keyboard-layout
     (keyboard-layout
      "us,ru" "dvorak,"
      #:options '("grp:shifts_toggle" "ctrl:nocaps"))))))
