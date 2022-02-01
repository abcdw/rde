(define-module (rde features wm)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features fontutils)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (rde packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages image)
  #:use-module (gnu packages web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (rde home services wm)
  #:use-module (gnu home-services shells)
  #:use-module (guix gexp)
  #:export (feature-sway
	    feature-sway-run-on-tty
            feature-sway-screenshot
            feature-sway-statusbar

            feature-swayidle))

;; https://github.com/jjquin/dotfiles/tree/master/sway/.config/sway/config.d
;; https://nixos.wiki/wiki/Sway
;; https://github.com/swaywm/sway/wiki/Useful-add-ons-for-sway

(define (keyboard-layout-to-sway-config keyboard-layout)
  (let ((kb-options (string-join
		     (keyboard-layout-options keyboard-layout) ",")))
    `((input *
	     ((xkb_layout  ,(keyboard-layout-name keyboard-layout))
	      (xkb_variant ,(keyboard-layout-variant keyboard-layout))
	      (xkb_options ,kb-options))))))

(define* (feature-sway
	  #:key
	  (extra-config '())
	  (sway sway)
          (swaylock swaylock)
          (foot foot)
          (xdg-desktop-portal xdg-desktop-portal)
          (xdg-desktop-portal-wlr xdg-desktop-portal-wlr)
          ;; Logo key. Use Mod1 for Alt.
          (sway-mod 'Mod4)
	  (add-keyboard-layout-to-config? #t)
          (xwayland? #f))
  "Setup and configure sway."
  (ensure-pred sway-config? extra-config)
  (ensure-pred boolean? add-keyboard-layout-to-config?)
  (ensure-pred any-package? sway)
  (ensure-pred any-package? swaylock)
  (ensure-pred any-package? foot)
  (ensure-pred any-package? xdg-desktop-portal)
  (ensure-pred any-package? xdg-desktop-portal-wlr)

  (define (sway-home-services config)
    "Returns home services related to sway."
    ;; (require-value 'elogind config)
    (let* ((kb-layout      (get-value 'keyboard-layout config))
	   (layout-config  (if (and add-keyboard-layout-to-config? kb-layout)
			       (keyboard-layout-to-sway-config kb-layout)
			       '()))


           (lock-cmd
            ;; ,(file-append swaylock "/bin/swaylock -c 3e3e3e")
            (get-value 'lock-cmd config
                       "swaylock -f -c 3e3e3e"))

           (default-terminal
             (get-value 'default-terminal config
                        (file-append foot "/bin/foot")))
           (default-app-launcher
             (get-value 'default-app-launcher config
                        (file-append wofi "/bin/wofi --show=drun"))))
      (list
       (service
	home-sway-service-type
	(home-sway-configuration
	 (package sway)
	 (config
	  `((xwayland ,(if xwayland? 'enable 'disable))
            (,#~"")
            ,@layout-config

            (,#~"")
            (set $mod ,sway-mod)
            (set $term ,default-terminal)
            (set $menu ,default-app-launcher)
            (set $lock ,lock-cmd)

            (,#~"")
            (bindsym $mod+Control+Shift+Return exec $term)
            (bindsym --to-code $mod+Shift+d exec $menu)
            (bindsym $mod+Shift+l exec $lock)

	    (,#~"")
            (default_border pixel)
            (default_floating_border pixel)
            (gaps inner ,(get-value 'emacs-margin config 8))))))

       (when (get-value 'swayidle-cmd config)
         (simple-service
	  'sway-enable-swayidle
	  home-sway-service-type
          `((,#~"")
	    (exec ,(get-value 'swayidle-cmd config)))))

       (when (get-value 'swayidle config)
         (let* ((swaymsg (file-append sway "/bin/swaymsg"))
                (swaymsg-cmd (lambda (cmd)
                               #~(format #f "'~a \"~a\"'" #$swaymsg #$cmd))))
           (simple-service
            'sway-add-dpms-to-swayidle
            home-swayidle-service-type
            `((timeout 600    ,(swaymsg-cmd "output * dpms off")
                       resume ,(swaymsg-cmd "output * dpms on\"'"))))))

       (simple-service
	'sway-configuration
	home-sway-service-type
        `(,@extra-config
	  (,#~"")))

       (simple-service
        'sway-reload-config-on-change
        (@@ (gnu home services) home-run-on-change-service-type)
        `(("files/config/sway/config"
           ,#~(system* #$(file-append sway "/bin/swaymsg") "reload"))))
       ;; TODO: Move wofi to feature-app-launcher or something like that
       (simple-service
        'packages-for-sway
	home-profile-service-type
        (append
         (if (get-value 'default-terminal config) '() (list foot))
	 (list wofi qtwayland
               ;; swaylock
               swayhide
               xdg-desktop-portal xdg-desktop-portal-wlr)))
       (simple-service 'set-wayland-specific-env-vars
		       home-environment-variables-service-type
		       ;; export NO_AT_BRIDGE=1
		       '(("XDG_CURRENT_DESKTOP" . "sway")
                         ("XDG_SESSION_TYPE" . "wayland")
                         ;; FIXME: Should be in feature-pipewire
                         ("RTC_USE_PIPEWIRE" . "true")
                         ("SDL_VIDEODRIVER" . "wayland")
                         ("MOZ_ENABLE_WAYLAND" . "1")
                         ("CLUTTER_BACKEND" . "wayland")
                         ("ELM_ENGINE" . "wayland_egl")
                         ("ECORE_EVAS_ENGINE" . "wayland-egl")
                         ("QT_QPA_PLATFORM" . "wayland-egl")
			 ("_JAVA_AWT_WM_NONREPARENTING" . "1"))))))

  (define (sway-system-services _)
    "Returns system services related to sway."
    (list
     ;; TODO: Find a better solution for foreign distros?
     ;; TODO: Move it to a separate feature?
     (screen-locker-service swaylock "swaylock")))

  (feature
   (name 'sway)
   (values `((sway . ,sway)
             (wl-clipboard . ,wl-clipboard)
	     (wayland . #t)
             (xwayland? . ,xwayland?)))
   (home-services-getter sway-home-services)
   (system-services-getter sway-system-services)))

(define* (feature-sway-run-on-tty
	  #:key (sway-tty-number 2))
  "Launch Sway on specified tty upon user login.  Also,
automatically switch to SWAY-TTY-NUMBER on boot."
  (ensure-pred tty-number? sway-tty-number)

  (define (sway-run-on-tty-home-services config)
    (list
     (simple-service
      'run-sway-on-login-to-sway-tty
      home-shell-profile-service-type
      (list
       (format #f "[ $(tty) = /dev/tty~a ] && exec sway"
	       sway-tty-number)))))

  (define (sway-run-on-tty-system-services _)
    (list
     (simple-service
      'switch-to-sway-tty-after-boot shepherd-root-service-type
      (list (shepherd-service
             (provision '(switch-to-sway-tty))
             (requirement '(virtual-terminal))
             (start #~(lambda ()
			(invoke #$(file-append kbd "/bin/chvt")
				#$(format #f "~a" sway-tty-number))))
             (one-shot? #t))))))

  (feature
   (name 'sway-run-on-tty)
   (values (make-feature-values sway-tty-number))
   (home-services-getter sway-run-on-tty-home-services)
   (system-services-getter sway-run-on-tty-system-services)))

(define* (feature-sway-screenshot)
  "Configure slurp, grim and other tools for screenshot capabilities."

  (define sway-f-name 'screenshot)
  (define f-name (symbol-append 'sway- sway-f-name))

  (define (get-home-services config)
    (require-value 'sway config)
    (define subject-output
      #~(format #f "~a -t get_outputs | ~a -r '.[] | select(.focused) | .name'"
                #$(file-append (get-value 'sway config) "/bin/swaymsg")
                #$(file-append jq "/bin/jq")))
    (define subject-window-or-selection
      #~(format #f "~a -t get_tree | ~a -r '.. | select(.pid? and .visible?) \
| .rect | \"\\(.x),\\(.y) \\(.width)x\\(.height)\"' | ~a -b ~a -B ~a"
                #$(file-append (get-value 'sway config) "/bin/swaymsg")
                #$(file-append jq "/bin/jq")
                ;; TODO: Move to slurp-cmd
                #$(file-append slurp "/bin/slurp")
                "303030AA"
                "303030AA"))

    (define* (shot-script subject #:key output geom (file "-"))
      (program-file
       (string-append "sway-shot-" subject)
       #~(system
          (format #f "~a ~a~a~a | ~a"
                  #$(file-append grim "/bin/grim")
                  #$(if output #~(string-append "-o \"$(" #$output ")\" ") "")
                  #$(if geom #~(string-append "-g \"$(" #$geom ")\" ") "")
                  #$file
                  #$(file-append (get-value 'wl-clipboard config)
                                 "/bin/wl-copy")))))

    (define shot-output
      (shot-script "output" #:output subject-output))
    (define shot-window-or-selection
      (shot-script "window-or-selection" #:geom subject-window-or-selection))
    (list
     ;; (simple-service
     ;;  'sway-screenshot-packages
     ;;  home-profile-service-type
     ;;  (list slurp grim wl-clipboard jq))

     (simple-service
      'sway-screenshot
      home-sway-service-type
      `((bindsym $mod+Print exec ,shot-output)
        (bindsym $mod+Shift+Print exec ,shot-window-or-selection)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; <https://www.reddit.com/r/unixporn/comments/a2c9kl/sway_in_the_wild/>
(define* (feature-sway-statusbar
          #:key
          ;; (package waybar)
          (battery "BAT0")
          (use-global-fonts? #f))
  "Configure statusbar."

  (define sway-f-name 'waybar)
  (define f-name (symbol-append 'sway- sway-f-name))

  (define (get-home-services config)
    (require-value 'sway config)
    (when use-global-fonts?
      (require-value 'font-monospace config))
    (define font-mono (get-value 'font-monospace config))
    (define (get-status-command)
      (format #f
              "while echo $(cat /sys/class/power_supply/~a/capacity)% \
$(date +'%Y-%m-%d %l:%M:%S %p'); do sleep 5; done" battery))
    (list
     (simple-service
      'sway-waybar
      home-sway-service-type
      `((bar ((position top)
              ,@(if use-global-fonts?
                    `((font ,(font-name font-mono)))
                    '())
              (colors ((statusline "#ffffff")
                       (background "#323232")))
              (status_command ,(get-status-command))))
        ;; (bar swaybar_command ,(file-append package "/bin/waybar"))
        ))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


(define* (feature-swayidle
          #:key
          (swayidle swayidle))
  "Configure swayidle."
  (ensure-pred any-package? swayidle)

  (define swayidle-cmd (file-append swayidle "/bin/swayidle -w"))

  (define (get-home-services config)
    (define lock-cmd "swaylock -f -c 3e3e3e")
    (define lock-cmd-quoted (format #f "'~a'" lock-cmd))
    (list
     (service
      home-swayidle-service-type
      (home-swayidle-configuration
       (swayidle swayidle)
       (config
        `((lock ,lock-cmd-quoted)
          (before-sleep ,lock-cmd-quoted)
          (timeout 300 ,lock-cmd-quoted)))))))

  (feature
   (name 'swayidle)
   (values `((swayidle . ,swayidle)
             (swayidle-cmd . ,swayidle-cmd)))
   (home-services-getter get-home-services)))
;; [X] feature-sway-run-on-tty
;; [X] feature-sway-screenshot
;; [ ] feature-sway-lock-idle-sleep
;; [ ] feature-sway-input
;; [ ] feature-sway-keybindings
;; [ ] feature-sway-media-keys
;; [ ] feature-sway-outputs (kanshi, workspaces, displays)

;; [ ] feature-wayland-appearance (sway, gtk, qt themes)
;; [ ] feature-wayland-statusbar
;; [ ] feature-wayland-notifications
;; [ ] feature-wayland-clipboard


;; window rules will configured on app's feature basis
