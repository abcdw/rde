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
  #:use-module (guix packages)

  #:use-module (srfi srfi-1)

  #:export (feature-sway
	    feature-sway-run-on-tty
            feature-sway-screenshot
            feature-sway-statusbar

            feature-swayidle
            feature-swaylock))

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
          (foot foot)
          (bemenu bemenu)
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
  (ensure-pred any-package? foot)
  (ensure-pred any-package? bemenu)
  (ensure-pred any-package? xdg-desktop-portal)
  (ensure-pred any-package? xdg-desktop-portal-wlr)

  (define (sway-home-services config)
    "Returns home services related to sway."
    (let* ((kb-layout      (get-value 'keyboard-layout config))
	   (layout-config  (if (and add-keyboard-layout-to-config? kb-layout)
			       (keyboard-layout-to-sway-config kb-layout)
			       '()))

           (lock-cmd
            (get-value 'default-screen-locker config "loginctl lock-session"))

           (default-terminal
             (get-value-eval 'default-terminal config
                             (file-append foot "/bin/foot")))
           (backup-terminal
             (get-value 'backup-terminal config
                        (file-append foot "/bin/foot")))
           (default-application-launcher
             (get-value 'default-application-launcher config
                        (file-append bemenu "/bin/bemenu-run -l 20 -p run:"))))
      (list
       (service
	home-sway-service-type
	(home-sway-configuration
	 (package sway)
	 (config
	  `((xwayland ,(if xwayland? 'enable 'disable))
            (,#~"")
            ,@layout-config

            (,#~"\n\n# General settings:")
            (set $mod ,sway-mod)
            (set $term ,default-terminal)
            (set $backup-term ,backup-terminal)
            (set $menu ,default-application-launcher)
            (set $lock ,lock-cmd)

            (floating_modifier $mod normal)

            (bindsym $mod+Shift+r reload)

            (,#~"\n\n# Launching external applications:")
            (bindsym $mod+Control+Shift+Return exec $backup-term)
            (bindsym $mod+Return exec $term)

            (bindsym $mod+Shift+d exec $menu)
            (bindsym $mod+Shift+l exec $lock)

            (,#~"\n\n# Manipulating windows:")
            (bindsym $mod+Shift+c kill)
            (bindsym $mod+Shift+f fullscreen)
            (bindsym $mod+Shift+space floating toggle)
            (bindsym $mod+Ctrl+space focus mode_toggle)

            (bindsym $mod+Left focus left)
            (bindsym $mod+Down focus down)
            (bindsym $mod+Up focus up)
            (bindsym $mod+Right focus right)

            (bindsym $mod+Shift+Left move left)
            (bindsym $mod+Shift+Down move down)
            (bindsym $mod+Shift+Up move up)
            (bindsym $mod+Shift+Right move right)

            (,#~"\n\n# Moving around workspaces:")
            (bindsym $mod+tab workspace back_and_forth)
            ,@(append-map
               (lambda (x)
                 `((bindsym ,(format #f "$mod+~a" (modulo x 10))
                            workspace number ,x)
                   (bindsym ,(format #f "$mod+Shift+~a" (modulo x 10))
                            move container to workspace number ,x)))
               (iota 10 1))

            (,#~"\n\n# Scratchpad settings:")
            (bindsym $mod+Shift+minus move scratchpad)
            (bindsym $mod+minus scratchpad show)

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
                               #~(format #f "'~a \"~a\"'" #$swaymsg #$cmd)))
                (idle-timeout (+ 30 (get-value 'lock-timeout config 120))))
           (simple-service
            'sway-add-dpms-to-swayidle
            home-swayidle-service-type
            `((timeout ,idle-timeout ,(swaymsg-cmd "output * dpms off")
               resume                ,(swaymsg-cmd "output * dpms on"))))))

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

       (simple-service
        'packages-for-sway
	home-profile-service-type
        (append
         (if (and (get-value 'default-terminal config)
                  (get-value 'backup-terminal config))
             '() (list foot))
         (if (get-value 'default-application-launcher config) '() (list bemenu))
	 (list qtwayland swayhide
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

  (feature
   (name 'sway)
   (values `((sway . ,sway)
             (wl-clipboard . ,wl-clipboard)
	     (wayland . #t)
             (xwayland? . ,xwayland?)))
   (home-services-getter sway-home-services)))


;;;
;;; sway-run-on-tty.
;;;

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


;;;
;;; sway-screenshot.
;;;

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


;;;
;;; sway-statusbar.
;;;

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


;;;
;;; swayidle.
;;;

(define* (feature-swayidle
          #:key
          (swayidle swayidle)
          (lock-timeout 120)
          (extra-config '()))
  "Configure swayidle."
  (ensure-pred any-package? swayidle)

  (define swayidle-cmd (file-append swayidle "/bin/swayidle -w"))

  (define (get-home-services config)
    (define lock-cmd (get-value 'default-screen-locker config))

    (list
     (service
      home-swayidle-service-type
      (home-swayidle-configuration
       (swayidle swayidle)
       (config
        `(,@(if lock-cmd
                (let ((lock-cmd-quoted (format #f "'~a'" lock-cmd)))
                  `((lock ,lock-cmd-quoted)
                    (before-sleep ,lock-cmd-quoted)
                    (timeout ,lock-timeout ,lock-cmd-quoted)))
                '())
          ,@extra-config))))))

  (feature
   (name 'swayidle)
   (values `((swayidle . ,swayidle)
             (swayidle-cmd . ,swayidle-cmd)
             (lock-timeout . ,lock-timeout)))
   (home-services-getter get-home-services)))


;;;
;;; swaylock.
;;;

(define* (feature-swaylock
          #:key
          (swaylock swaylock)
          (show-failed-attempts? #t)
          (show-keyboard-layout? #f)
          (daemonize? #t)
          (extra-config '())
          (default-screen-locker? #t))
  "Configure swaylock."
  (ensure-pred any-package? swaylock)

  (define (get-home-services config)
    (list
     (service
      home-swaylock-service-type
      (home-swaylock-configuration
       (swaylock swaylock)
       (config
        `((show-failed-attempts . ,show-failed-attempts?)
          (daemonize . ,daemonize?)
          (show-keyboard-layout . ,show-keyboard-layout?)
          ;; TODO: Source color from colorscheme
          (color . 3e3e3e)
          (indicator-caps-lock)
          ,@extra-config))))))

  (define (get-system-services _)
    (list
     (screen-locker-service swaylock "swaylock")
     ;; (simple-service
     ;;  'setuid-chkpwd
     ;;  setuid-program-service-type
     ;;  (list (file-like->setuid-program
     ;;         (file-append linux-pam "/sbin/unix_chkpwd"))))

     ;; (simple-service
     ;;  'sway-add-swaylock-pam
     ;;  pam-root-service-type
     ;;  (list
     ;;   (unix-pam-service "swaylock")))
     ))

  (feature
   (name 'swaylock)
   (values `((swaylock . ,swaylock)
             ,@(if default-screen-locker?
                   ;; TODO: Change it to path in the store, once
                   ;; https://issues.guix.gnu.org/53468 is resolved
                   `((default-screen-locker . "/run/setuid-programs/swaylock"))
                   '())))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))

;; [X] feature-sway-run-on-tty
;; [X] feature-sway-screenshot
;; [X] feature-sway-lock-idle-sleep
;; [ ] feature-sway-input
;; [ ] feature-sway-keybindings
;; [ ] feature-sway-media-keys
;; [ ] feature-sway-outputs (kanshi, workspaces, displays)

;; [ ] feature-wayland-appearance (sway, gtk, qt themes)
;; [ ] feature-wayland-statusbar
;; [ ] feature-wayland-notifications
;; [ ] feature-wayland-clipboard


;; window rules will configured on app's feature basis
