(define-module (abcdw configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features security-token)
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
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features linux)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (rde features irc)
  #:use-module (rde features docker)
  #:use-module (rde features virtualization)
  #:use-module (rde features video)
  #:use-module (rde features finance)
  #:use-module (rde features markup)
  #:use-module (rde features networking)
  #:use-module (rde features clojure)
  #:use-module (rde features javascript)

  #:use-module (gnu services)
  #:use-module (rde home services i2p)
  #:use-module (rde home services emacs)
  #:use-module (rde home services wm)
  #:use-module (gnu home services)

  #:use-module (gnu home-services ssh)

  ;; #:use-module (gnu services nix)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages)
  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (ice-9 match))


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
   'additional-emacs-packages
   home-emacs-service-type
   (home-emacs-extension
    (elisp-packages
     (append
      (strings->packages
       ;; "emacs-dirvish"
       "emacs-hl-todo"
       "emacs-yasnippet"
       ;; "emacs-company"
       "emacs-consult-dir"
       ;; "emacs-all-the-icons-completion" "emacs-all-the-icons-dired"
       "emacs-kind-icon"
       "emacs-nginx-mode" "emacs-yaml-mode"
       ;; "emacs-lispy"
       "emacs-ytdl"
       "emacs-multitran"
       "emacs-minimap"
       "emacs-ement"
       "emacs-restart-emacs"
       "emacs-org-present"))))))

(define home-extra-packages-service
  (simple-service
   'additional-packages
   home-profile-service-type
   (append
    (strings->packages
     "figlet" ;; TODO: Move to emacs-artist-mode
     "calibre"
     "icecat" "nyxt"
     "ungoogled-chromium-wayland" "ublock-origin-chromium"

     "utox" "qtox" "jami"

     "alsa-utils" "youtube-dl" "imv" "cozy"
     "pavucontrol" "wev"
     "imagemagick"
     "obs" "obs-wlrobs"
     "recutils" "binutils" "make"
     "fheroes2"

     "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
     "papirus-icon-theme" "arc-theme"
     "thunar" "fd"
     ;; "glib:bin"

     "libreoffice"
     "ffmpeg"
     "ripgrep" "curl"))))

(define sway-extra-config-service
  (simple-service
   'sway-extra-config
   home-sway-service-type
   `((output DP-2 scale 2)
     ;; (output eDP-1 disable)
     ,@(map (lambda (x) `(workspace ,x output DP-2)) (iota 8 1))

     ;; (workspace 9 output DP-2)
     ;; (workspace 10 output DP-2)

     ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)

     (bindsym
      --locked $mod+Shift+t exec
      ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
      play-pause)

     (bindsym
      --locked $mod+Shift+n exec
      ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
      next)

     (bindsym $mod+Shift+o move workspace to output left)
     (bindsym $mod+Ctrl+o focus output left)
     (input type:touchpad
            ;; TODO: Move it to feature-sway or feature-mouse?
            (;; (natural_scroll enabled)
             (tap enabled)))
     (bindsym $mod+Shift+Return exec emacs)
     ;; (xwayland disable)
     )))

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
   (home-ssh-configuration
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
        (host "pinky-ygg")
        (options
         '((host-name . "200:554d:3eb1:5bc5:6d7b:42f4:8792:efb8")
           (port . 50621)
           (control-master . "auto")
           (control-path . "~/.ssh/master-%r@%h:%p")
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


;;; User-specific features with personal preferences

;; Initial user's password hash will be available in store, so use this
;; feature with care (display (crypt "hi" "$6$abc"))

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
    #:gpg-primary-key "74830A276C328EC2")
   (feature-security-token)
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

   (feature-custom-services
    #:feature-name-prefix 'abcdw
    #:home-services
    (list
     emacs-extra-packages-service
     home-extra-packages-service
     sway-extra-config-service
     i2pd-add-ilita-irc-service))

   (feature-ssh-proxy #:host "pinky-ygg" #:auto-start? #f)
   (feature-ssh-proxy #:host "pinky-ygg" #:name "hundredrps"
                      #:proxy-string "50080:localhost:8080"
                      #:reverse? #t
                      #:auto-start? #f)

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

   (feature-keyboard
    ;; To get all available options, layouts and variants run:
    ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
    #:keyboard-layout
    (keyboard-layout
     "us,ru" "dvorak,"
     #:options '("grp:shifts_toggle" "ctrl:nocaps")))))


;;; Generic features should be applicable for various hosts/users/etc

;;; WARNING: The order can be important for features extending
;;; services of other features.  Be careful changing it.
(define %main-features
  (list
   (feature-yggdrasil)
   (feature-i2pd
    #:outproxy 'http://acetone.i2p:8888
    ;; 'purokishi.i2p
    #:less-anonymous? #t)

   ;; (feature-rofi)

   ;; TODO: Add an app for saving and reading articles and web pages
   ;; https://github.com/wallabag/wallabag
   ;; https://github.com/chenyanming/wallabag.el

   ;; TODO: feature-wallpapers https://wallhaven.cc/
   ;; TODO: feature-icecat

   (feature-emacs-keycast #:turn-on? #t)

   (feature-emacs-tempel
    #:default-templates? #t
    #:templates
    `(fundamental-mode
      ,#~""
      (t (format-time-string "%Y-%m-%d"))
      ;; TODO: Move to feature-guix
      ,((@ (rde gexp) slurp-file-like)
        (file-append ((@ (guix packages) package-source)
                      (@ (gnu packages package-management) guix))
                     "/etc/snippets/tempel/text-mode"))))
   (feature-emacs-spelling
    #:spelling-program (@ (gnu packages libreoffice) hunspell)
    #:spelling-dictionaries (strings->packages
                             "hunspell-dict-en"
                             "hunspell-dict-ru"))
   ;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
   ;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>
   (feature-emacs-git
    #:project-directory "~/work")
   (feature-emacs-org
    #:org-directory "~/work/abcdw/private"
    #:org-indent? #f
    #:org-capture-templates
    `(("t" "Todo" entry (file+headline "" "Tasks") ;; org-default-notes-file
       "* TODO %?\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)))
   (feature-emacs-org-roam
    ;; TODO: Rewrite to states
    #:org-roam-directory "~/work/abcdw/notes/notes")
   (feature-emacs-org-agenda
    #:org-agenda-files '("~/work/abcdw/private/todo.org"
                         "~/work/abcdw/rde/TODO"))
   (feature-emacs-elfeed
    #:elfeed-org-files '("~/work/abcdw/private/rss.org"))

   (feature-javascript)

   (feature-notmuch
    ;; TODO: Add integration with mail-lists
    ;; `notmuch-show-stash-mlarchive-link-alist'
    #:extra-tag-updates-post
    '("notmuch tag +guix-home -- 'thread:\"\
{((subject:guix and subject:home) or (subject:service and subject:home) or \
subject:/home:/) and tag:new}\"'")
    #:notmuch-saved-searches
    (cons*
     ;; TODO: Add tag:unread to all inboxes.  Revisit archive workflow.
     '(:name "Work Inbox" :query "tag:work and tag:inbox and tag:unread" :key "W")
     '(:name "Personal Inbox" :query "tag:personal and tag:inbox" :key "P")
     '(:name "Guix Home Inbox" :key "H" :query "tag:guix-home and tag:unread")
     '(:name "RDE Inbox"       :key "R"
             :query "(to:/rde/ or cc:/rde/) and tag:unread")

     ;; '(:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "tw")
     %rde-notmuch-saved-searches))))


;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(use-modules (abcdw hosts ixy)
             (abcdw emacs)
             (abcdw general))

(define-public ixy-config
  (rde-config
   (features
    (append
     %abcdw-features
     %main-features
     %emacs-features
     %general-features
     %ixy-features))))

(define-public ixy-os
  (rde-config-operating-system ixy-config))

(define-public ixy-he
  (rde-config-home-environment ixy-config))


;;; Dispatcher, which helps to return various values based on environment
;;; variable value.

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      (_ ixy-he))))

(dispatcher)
