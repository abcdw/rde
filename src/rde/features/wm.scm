;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024, 2025 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2022, 2024 Nicolas Graves <ngraves@ngraves.fr>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde features wm)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (rde features emacs)
  #:use-module (rde features fontutils)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (rde packages)
  #:use-module (rde packages wm)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services xorg)
  #:use-module (gnu home services)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services shepherd)
  #:use-module (rde home services wm)
  #:use-module (rde home services shells)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (srfi srfi-1)

  #:export (feature-sway
            feature-sway-run-on-tty
            feature-sway-screenshot

            feature-sway-statusbar
            feature-waybar
            waybar-sway-language
            waybar-sway-window
            waybar-sway-workspaces
            waybar-tray
            waybar-temperature
            waybar-microphone
            waybar-volume
            waybar-cpu
            waybar-memory
            waybar-disk
            waybar-idle-inhibitor
            waybar-clock
            waybar-battery
            waybar-custom

            feature-swaynotificationcenter
            feature-swayidle
            feature-swaylock
            feature-swaykbdd
            feature-kanshi
            feature-batsignal))


;;;
;;; Sway.
;;;

;; https://github.com/jjquin/dotfiles/tree/master/sway/.config/sway/config.d
;; https://nixos.wiki/wiki/Sway
;; https://github.com/swaywm/sway/wiki/Useful-add-ons-for-sway

(define (keyboard-layout-to-sway-config keyboard-layout)
  (let* ((kb-options (keyboard-layout-options keyboard-layout))
         (xkb-options (and (not (null? kb-options))
                           (string-join kb-options ",")))
         (xkb-variant (keyboard-layout-variant keyboard-layout)))
    `((input *
             ((xkb_layout  ,(keyboard-layout-name keyboard-layout))
              ,@(if xkb-variant `((xkb_variant ,xkb-variant)) '())
              ,@(if xkb-options `((xkb_options ,xkb-options)) '()))))))

(define* (feature-sway
          #:key
          (extra-config '())
          (sway sway)
          (foot foot)
          (dconf dconf)
          (bemenu bemenu)
          (qtwayland qtwayland)
          (shepherd #f)
          (xdg-desktop-portal xdg-desktop-portal)
          (xdg-desktop-portal-gtk xdg-desktop-portal-gtk)
          (xdg-desktop-portal-wlr xdg-desktop-portal-wlr)
          ;; Logo key. Use Mod1 for Alt.
          (sway-mod 'Mod4)
          (add-keyboard-layout-to-config? #t)
          (xwayland? #f))
  "Setup and configure sway."
  (ensure-pred sway-config? extra-config)
  (ensure-pred boolean? add-keyboard-layout-to-config?)
  (ensure-pred boolean? xwayland?)
  (ensure-pred file-like? sway)
  (ensure-pred file-like? foot)
  (ensure-pred file-like? bemenu)
  (ensure-pred file-like? qtwayland)
  (ensure-pred file-like? xdg-desktop-portal)
  (ensure-pred file-like? xdg-desktop-portal-gtk)
  (ensure-pred file-like? xdg-desktop-portal-wlr)

  (when shepherd
    (warning
     (G_ "'~a' in feature-wm is deprecated and ignored, use '~a' in feature-shepherd instead~%")
     'shepherd 'shepherd))

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
             (get-value-eval 'default-application-launcher-fn config
                             (file-append bemenu "/bin/bemenu-run -l 20 -p run:"))))
      (list
       (simple-service
        'sway-launch-shepherd
        home-sway-service-type
        `((,#~"\n\n# Launch shepherd:")
          (exec ,(get-value 'shepherd-launch config))))

       (service
        home-sway-service-type
        (home-sway-configuration
         (package sway)
         (config
          `(,@(if xwayland? `((xwayland enable)) '())
            (,#~"\n\n# Layout configuration")
            ,@layout-config

            (,#~"\n\n# General settings:")
            (set $mod ,sway-mod)

            (floating_modifier $mod normal)

            (bindsym --to-code $mod+Shift+r reload)

            (,#~"\n\n# Launching external applications:")
            (set $term ,default-terminal)
            (set $backup-term ,backup-terminal)
            (set $menu ,default-application-launcher)
            (set $lock ,lock-cmd)

            ,@(if (get-value 'default-pass-prompt-fn config #f)
                  `((set $pass ,(get-value-eval 'default-pass-prompt-fn config))
                    (bindsym --to-code $mod+Shift+p exec $pass)
                    (bindsym --to-code $mod+Control+p exec $pass))
                  '())

            ,@(if (get-value 'default-power-menu-fn config #f)
                  `((set $power-menu ,(get-value-eval 'default-power-menu-fn config))
                    (bindsym --to-code $mod+Shift+q exec $power-menu)
                    (bindsym --to-code $mod+Control+q exec $power-menu))
                  '())

            (bindsym $mod+Control+Shift+Return exec $backup-term)
            (bindsym $mod+Return exec $term)

            (bindsym --to-code $mod+Shift+d exec $menu)
            (bindsym --to-code $mod+Shift+l exec $lock)
            (bindsym --to-code $mod+Control+d exec $menu)
            (bindsym --to-code $mod+Control+l exec $lock)

            (,#~"\n\n# Manipulating windows:")
            (bindsym --to-code $mod+Shift+c kill)
            (bindsym --to-code $mod+Shift+f fullscreen)
            (bindsym --to-code $mod+Control+c kill)
            (bindsym --to-code $mod+Control+f fullscreen)
            (bindsym $mod+Shift+space floating toggle)
            (bindsym $mod+Ctrl+space focus mode_toggle)

            ;; (bindsym --to-code $mod+Shift+o
            ;;          #{[workspace=__focused__]}# focus next)

            ;; TODO: Add keybindings for controlling swaynag
            ;; <https://wiki.archlinux.org/title/Sway#Control_swaynag_with_the_keyboard>

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
            (bindsym --to-code $mod+Shift+minus move scratchpad)
            (bindsym --to-code $mod+minus scratchpad show)

            (,#~"")
            (default_border pixel)
            (default_floating_border pixel)
            (gaps inner ,(get-value 'emacs-margin config 8))))))

       (when (get-value 'swayidle-cmd config #f)
         (simple-service
          'sway-enable-swayidle
          home-sway-service-type
          `((,#~"")
            (exec ,(get-value 'swayidle-cmd config)))))

       (when (get-value 'swayidle config #f)
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
        home-run-on-change-service-type
        `(("files/.config/sway/config"
           ,#~(system* #$(file-append sway "/bin/swaymsg") "reload"))))

       (simple-service
        'xdg-desktop-portal-wlr-configuration
        home-xdg-configuration-files-service-type
        `(("xdg-desktop-portal-wlr/config"
           ,(mixed-text-file
             "xdg-desktop-portal-wlr-config"
             #~(format #f "[screencast]
output_name=
max_fps=30
chooser_cmd=~a -f %o -or -c ff0000
chooser_type=simple"
                       #$(file-append (get-value 'slurp config slurp)
                                      "/bin/slurp"))))))

       (simple-service
        'packages-for-sway
        home-profile-service-type
        (append
         (list dconf)
         (if (and (get-value 'default-terminal config #f)
                  (get-value 'backup-terminal config #f))
             '() (list foot))
         (if (get-value 'default-application-launcher-fn config #f)
             '() (list bemenu))
         (list qtwayland swayhide
               xdg-desktop-portal
               xdg-desktop-portal-gtk
               xdg-desktop-portal-wlr)))

       (when (get-value 'emacs config #f)
         (rde-elisp-configuration-service
          'sway
          config
          `((eval-when-compile (require 'sway))
            (autoload 'sway--x-focus-frame "sway")
            (defalias 'x-focus-frame 'sway--x-focus-frame)
            (setq frame-title-format
                  '(multiple-frames ("" "%b — GNU Emacs at " system-name
                                     " [" (:eval (frame-parameter (selected-frame) 'window-id)) "]")
                                    "%b")))
          #:summary "\
Emacs configuration to play nice with sway."
          #:commentary "\
Currently this is a workaround because pgtk doesn't support the function
x-focus-frame (which is used in emacs-mini-frame for instance). Using sway.el
properly with pgtk also requires a unique name for each frame, hence the
frame-title-format."
          #:keywords '(convenience)
          #:elisp-packages
          (list emacs-sway))))))

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
          #:key
          (sway-tty-number 2)
          (logfile "${XDG_STATE_HOME}/log/sway.log")
          (launch-arguments ""))
  "Launch Sway on specified tty upon user login.  Also, automatically switch
to SWAY-TTY-NUMBER on boot.  Log errors into LOGFILE. Sway is launched with
additional LAUNCH-ARGUMENTS."
  (ensure-pred tty-number? sway-tty-number)
  (ensure-pred string? logfile)
  (ensure-pred string? launch-arguments)

  (define (sway-run-on-tty-home-services config)
    (require-value 'sway config)

    (define env-vars
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
        ("_JAVA_AWT_WM_NONREPARENTING" . "1")))

    (define sway-with-env-vars
      (program-file
       "sway-with-env-vars"
       #~(begin
           (for-each (lambda (x)
                       (setenv (car x) (cdr x)))
                     '#$env-vars)
           (apply system* #$(file-append (get-value 'sway config) "/bin/sway")
                  (cdr (command-line))))))
    (list
     (simple-service
      'sway-run-sway-on-login-to-sway-tty
      home-shell-profile-service-type
      (list
       #~(format
          #f
          "[ $(tty) = /dev/tty~a ] && \\
~a -p \"$(~a ~a)\" && \\
exec ~a ~a"
          #$sway-tty-number
          #$(file-append (get-value 'coreutils config coreutils) "/bin/mkdir")
          #$(file-append (get-value 'coreutils config coreutils) "/bin/dirname")
          #$logfile
          #$sway-with-env-vars
          #$(string-join (list launch-arguments "2>>" logfile) " "))))))

  (define (sway-run-on-tty-system-services _)
    (list
     (simple-service
      'sway-switch-to-sway-tty-after-boot
      shepherd-root-service-type
      (list (shepherd-service
             (provision '(switch-to-sway-tty))
             (requirement '(term-tty1))
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

(define* (feature-sway-screenshot
          #:key (screenshot-key 'Print))
  "Configure slurp, grim and other tools for screenshot capabilities.  Feature
is sway dependent, because it relies on swaymsg.

If @code{xdg-user-directories-configuration} value provided, feature uses
@code{$XDG_PICTURES_DIR/screenshots} directory for saving swappy results (and
changes filename format by removing swappy- prefix and using ISO date),
otherwise $HOME directory is used."

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

    (define xdg-user-dirs
      (get-value 'xdg-user-directories-configuration config #f))

    (define ensure-screenshots-dir
      (if xdg-user-dirs
          "source ${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs;
export XDG_PICTURES_DIR; mkdir -p $XDG_PICTURES_DIR/screenshots;"
          ""))

    (define swappy-clipboard
      (program-file
       "sway-swappy-clipboard"
       #~(system
          (format #f "~a ~a | ~a -f -"
                  #$ensure-screenshots-dir
                  #$(file-append (get-value 'wl-clipboard config wl-clipboard)
                                 "/bin/wl-paste")
                  #$(file-append (get-value 'swappy config swappy)
                                 "/bin/swappy")))))

    (list
     (simple-service
      'sway-screenshot-swappy-config
      home-xdg-configuration-files-service-type
      `(("swappy/config"
         ,(mixed-text-file
           "swappy.conf"
           "[Default]\n"
           (if xdg-user-dirs "save_dir=$XDG_PICTURES_DIR/screenshots\n" "")
           (if xdg-user-dirs "save_filename_format=%Y-%m-%d-%H%M%S.png" "")))))
     (simple-service
      'sway-screenshot
      home-sway-service-type
      `((bindsym ,(symbol-append '$mod+ screenshot-key) exec ,shot-output)
        (bindsym ,(symbol-append '$mod+Alt+ screenshot-key)
                 exec ,swappy-clipboard)
        ;; Usually Fn+PrtSc
        (bindsym ,(symbol-append 'XF86SelectiveScreenshot)
                 exec ,shot-window-or-selection)
        (bindsym ,(symbol-append '$mod+Shift+ screenshot-key)
                 exec ,shot-window-or-selection)))))

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
          (battery "BAT0")
          (use-global-fonts? #f))
  "Configure statusbar."

  (define sway-f-name 'statusbar)
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
      'sway-statusbar
      home-sway-service-type
      `((bar ((position top)
              ,@(if use-global-fonts?
                    `((font ,(font-name font-mono)))
                    '())
              (colors ((statusline "#ffffff")
                       (background "#323232")))
              (status_command ,(get-status-command))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; waybar.
;;;

;; TODO: Move to home services?
(define* (waybar-module
          name
          #:optional
          (config '())
          (style '())
          #:key
          (placement 'modules-right)
          (bar-id 'main))
  "Returns a service, which extends home-waybar-service-type in the way the
module will be added to the BAR-ID."
  (simple-service
   (symbol-append 'waybar-module- name)
   home-waybar-service-type
   (home-waybar-extension
    (config `#(((name . ,bar-id)
                (,placement . #(,name))
                (,name . ,config))))
    (style-css style))))

(define* (waybar-sway-language #:key (bar-id 'main))
  (waybar-module 'sway/language #:bar-id bar-id))

(define* (waybar-sway-window #:key (bar-id 'main))
  (waybar-module
   'sway/window
   `()
   `((#{#window}#
      ((margin-left . 1em)
       (margin-right . 1em))))
   #:placement 'modules-center
   #:bar-id bar-id))

(define* (waybar-sway-workspaces
          #:key
          (bar-id 'main)
          (persistent-workspaces '())
          (all-outputs? #f)
          (format-icons '(("1" . )
                          ("2" . )
                          ("3" . )
                          ("4" . )
                          ("5" . )
                          ("6" . )  ; 
                          ("7" . )  ; 
                          ("8" . )
                          ("9" . )
                          ("10" . )

                          ("urgent" . )
                          ;; ("focused" . )
                          ("default" . ))))
  "PERSISTENT-WORKSPACES is a list of pairs workspace and vector of outputs."
  (waybar-module
   'sway/workspaces
   `((disable-scroll . #t)
     (format . {icon})
     ;; FIXME: Height becomes higher when icons are not used.
     (format-icons . ,format-icons)
     (all-outputs . ,all-outputs?)
     (persistent_workspaces . ,persistent-workspaces))
   `(((#{#workspaces}# button)
      ((background . none)
       (border-radius . 0.2em)
       (margin . (0.4em 0.2em))
       (padding . (0.2em 0.2em))
       (color . @base05)))

     ((#{#workspaces}# button:hover)
      ((background . none)
       (border-color . @base07)))

     ((#{#workspaces}# button.focused)
      ((background . @base02)
       (color . @base07)))

     ((#{#workspaces}# button.urgent)
      ((color . @base08))))
   #:placement 'modules-left
   #:bar-id bar-id))

(define* (waybar-tray #:key (bar-id 'main))
  (waybar-module
   'tray
   `()
   `(((#{#tray}# menu)
      ((color . @base05)
       (background . @base01)
       (border . (solid 1px))
       (border-color . @base02)))

     ((#{#tray}# menu menuitem)
      ((padding-top . 0px)
       (padding-bottom . 0px)
       (margin-top . 0.1em)
       (margin-bottom . 0em)))

     ((#{#tray}# menu menuitem:hover)
      ((background . none)))

     ((#{#tray}# menu separator)
      ((background . @base03)
       (padding-top . 1px)
       (margin-top . 0.2em)
       (margin-bottom . 0.2em))))
   #:bar-id bar-id))

(define* (waybar-temperature #:key (bar-id 'main))
  (waybar-module 'temperature #:bar-id bar-id))

(define* (waybar-idle-inhibitor #:key (bar-id 'main))
  (waybar-module
   'idle_inhibitor
   '((format . {icon})
     (format-icons . ((activated . )
                      (deactivated . )))
     (tooltip-format-activated
      . "Idle inhibitor {status}.  It prevents automatic screen locking and \
hibernation.")
     (tooltip-format-deactivated . "Idle inhibitor {status}."))
   #:bar-id bar-id))

(define* (waybar-clock
          #:key
          (format "{:%Y-%m-%d %H:%M}")
          (interval 60)
          (timezone #f)
          (bar-id 'main))
  "Returns a function, which accepts rde config and returns waybar clock module.
Left click on the module open world-clock in emacs client, right click opens
calendar."
  (lambda (config)
    (define ec (get-value 'emacs-client config))
    (define (ec-command command)
      #~(format #f "\"~a --eval \\\"(~a)\\\"\"" #$ec #$command))
    (waybar-module
     'clock
     `((tooltip-format
        . "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>")

       ,@(if timezone `((timezone . ,timezone)) '())
       (format . ,format)
       ,@(if ec
             `((on-click . ,(ec-command "world-clock"))
               (on-click-right . ,(ec-command "calendar")))
             '())

       (interval . ,interval))
     #:bar-id bar-id)))

(define* (waybar-battery
          #:key
          (intense? #f)
          (show-percentage? #f)
          (charging-icon "⚡")
          (bar-id 'main))
  "When INTENSE? is #t changes background color instead of text color when the
battery is low or nearly empty.  SHOW-PERCENTAGE? controls if current capacity
is displayed in label or not, in most cases it's not needed and disctracting,
also this information can be obtained from tooltip if really needed.
CHARGING-ICON is shown next to battery icon, when battery on AC and not full."
  (let ((base-format (if show-percentage? "{capacity}" ""))
        (percent-sign (if show-percentage? "% " "")))
    (waybar-module
     'battery
     `((format . ,(format #f "~a~a{icon}" base-format percent-sign))
       (format-charging . ,(format #f "~a~a {icon}" base-format charging-icon))
       (tooltip-format . "Current capacity: {capacity}%\n\n{timeTo}")
       ;; | icon |  range |
       ;; |------+--------|
       ;; |    0 | 0-10   |
       ;; |   25 | 10-40  |
       ;; |   50 | 40-60  |
       ;; |   75 | 60-90  |
       ;; |  100 | 90-100 |
       (states . ((empty . 10)
                  (low . 20)
                  (half-low . 40)
                  (half . 60)
                  (high . 90)
                  (full . 100)))
       (format-icons . ((empty . )
                        (low . )
                        (half-low . )
                        (half . )
                        (high . )
                        (full . ))))
     `((#{#battery.discharging.empty}#
        ,(if intense?
             `((color . @base02)
               (background . @base08))
             `((color . @base08))))
       (#{#battery.discharging.low}#
        ,(if intense?
             `((color . @base02)
               (background . @base09))
             `((color . @base09)))))
     #:bar-id bar-id)))

;; https://www.reddit.com/r/swaywm/comments/sks343/pwvolume_pipewire_volume_control_and_waybar_module/
;; https://github.com/Alexays/Waybar/wiki/Module:-PulseAudio
(define* (waybar-microphone #:key (bar-id 'main))
  "It's more minimalistic counterpart of waybar-volume, but for input audio
device.  It has no configuration options yet."
  (lambda (config)
    (waybar-module
     'pulseaudio#source
     `((format . "{format_source}")
       (format-source . "")    ;; active mic
       (format-source-muted . "") ;; muted mic ;
       (tooltip-format . "{source_desc} is on {source_volume}%")
       (on-click . ,#~(format #f "~s"
                              #$(file-append
                                 (get-value 'pavucontrol config pavucontrol)
                                 "/bin/pavucontrol --tab=4")))
       (scroll-step . 0))
     #:bar-id bar-id)))

(define* (waybar-volume
          #:key
          (show-percentage? #f)
          (scroll-step 0)
          (bar-id 'main))
  "Left click opens pavucontrol, scroll changes volume, but it seems buggy and
SCROLL-STEP is 0 by default.  People rarely care about precise percentage,
only icons for every 25% is provided by default, controlled by
SHOW-PERCENTAGE?."
  (lambda (config)
    (let ((base-format (string-append
                        (if show-percentage? "{volume}% " "")
                        "{icon}")))
      (waybar-module
       'pulseaudio#sink
       `((format . ,base-format)
         (format-muted . "🔇")
         (format-bluetooth . ,(string-append base-format " " ""))
         (tooltip-format . "{desc} is on {volume}%")
         (format-icons . ((default .  #(🔈 🔉 🔊 📢))))
         (on-click . ,#~(format #f "~s"
                                #$(file-append
                                   (get-value 'pavucontrol config pavucontrol)
                                   "/bin/pavucontrol")))
         (on-click-middle . ,#~(format #f "~s"
                                #$(file-append
                                   (get-value 'pulseaudio config pulseaudio)
                                   "/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle")))
         (scroll-step . ,scroll-step))
       #:bar-id bar-id))))

(define* (waybar-memory #:key (bar-id 'main))
  "Displays information about the memory."
  (lambda (config)
    (waybar-module
     'memory
     `((interval . 30)
       (format . " {percentage}%"))
     #:bar-id bar-id)))

(define* (waybar-cpu #:key (bar-id 'main))
  "Displays information about the current CPU load."
  (lambda (config)
    (waybar-module
     'cpu
     `((interval . 2)
       (format . " {usage}%"))
     #:bar-id bar-id)))

(define* (waybar-disk
          #:key
          (name 'root)
          (path "/")
          (disk-icon "")
          (bar-id 'main))
  "Displays information about the specified disk.
By default, NAME is root, PATH is /, and DISK-ICON is ."
  (lambda (config)
    (waybar-module
     (symbol-append 'disk# name)
     `((interval . 30)
       (format . ,(string-append disk-icon " {percentage_used}%"))
       (path . ,path))
     #:bar-id bar-id)))

(define* (waybar-custom
          #:key
          (bar-id 'main)
          (name 'main)
          (exec #f)
          (return-type "json")
          (icon #f)
          (interval 60)
          (placement 'modules-right))
  "Executes a custom script EXEC. The script is expected
to return a valid json object."
  (lambda (config)
    (waybar-module
     (symbol-append 'custom/ name)
     `((interval . ,interval)
       (exec . ,exec)
       (return-type . ,return-type)
       (tooltip . "{tooltip}")
       (format . ,(if icon
                       (string-append icon " {}")
                       "{}")))
     #:bar-id bar-id
     #:placement placement)))

(define* (feature-waybar
          #:key
          (waybar (@ (rde packages wm) waybar-stable))
          (waybar-modules
           (list
            (waybar-sway-workspaces)
            (waybar-tray)
            (waybar-idle-inhibitor)
            (waybar-sway-language)
            (waybar-microphone)
            (waybar-volume)
            (waybar-battery #:intense? #f)
            (waybar-clock)))
          (base16-css (local-file "./wm/waybar/base16-default-dark.css"))
          (extra-config '())
          (transitions? #f)
          (output #f)
          (height #f))
  "Configure waybar.  Each element of WAYBAR-MODULES is a home service or a
function accepting an rde config and returning a home-service, which extends
home-waybar-service-type.  Set TRANSITIONS? to #t if you prefer a smooth
animation. Define a list of additional bars using EXTRA-CONFIG, you can use
waybar modules with #:bar-id equal to the name of the bar. By default, the
main bar will be shown on every output. Use OUTPUT to set a specific output
for the main bar."

  (ensure-pred list? extra-config)
  (ensure-pred maybe-symbol? output)
  (ensure-pred maybe-integer? height)
  (define f-name 'waybar)

  (define (get-home-services config)
    ;; (when use-global-fonts?
    ;;   (require-value 'font-monospace config))
    (define font-mono
      (and=> (get-value 'font-monospace config #f)
             (compose string->symbol font-name)))
    (append
     (list
      (simple-service
       'waybar-add-font-awesome
       home-profile-service-type
       (list font-awesome))
      (service
        home-waybar-service-type
        (home-waybar-configuration
         (waybar waybar)
         (config `#(((position . top)
                     (name . main)
                     ,@(if height `((height . ,height)) '())
                     ,@(if output `((output . ,output)) '()))
                    ,@extra-config))

         ;; TODO: fix tray menu styles.
         (style-css
          `(,#~(format #f "@import \"~a\";\n" #$base16-css)
            (*
             ((font-family . #(,@(if font-mono (list font-mono) '())
                               ;; TODO: Add icon-font argument
                               FontAwesome))
              ,@(if transitions? '() '((transition . none)))
              (box-shadow . none)
              (text-shadow . none)
              (min-height . 0)))

            (tooltip
             ((border . (solid @base02))
              (background . @base01)
              (opacity . 0.9)))

            ((tooltip label)
             ((color . @base05)
              (padding . 0)))

            (#{#waybar}#
             ((color . @base04)
              (background . @base01)))

            (#((.modules-right label)
               (.modules-right image))
             ((margin . (0.4em 0.2em))
              (padding . (0 0.4em))
              (background . @base02)
              (border-radius . 0.2em)))

            (.modules-left
             ((margin-left . 0.2em)))

            (.modules-right
             ((margin-right . 0.2em))))))))

      (map (lambda (x) (if (procedure? x) (x config) x)) waybar-modules)

      (list
       (simple-service
        'waybar-reload-config-on-change
        home-run-on-change-service-type
        `(("files/.config/waybar/style.css"
           ,#~(system* #$(file-append psmisc "/bin/killall") "-SIGUSR2" "waybar"))
          ("files/.config/waybar/config"
           ,#~(system* #$(file-append psmisc "/bin/killall") "-SIGUSR2" "waybar"))))

       (when (get-value 'sway config #f)
         (simple-service
          'sway-waybar
          home-sway-service-type
          `((bar swaybar_command ,(file-append waybar "/bin/waybar"))))))))

  (feature
   (name 'waybar)
   (values `((waybar . ,waybar)
             (sway-statusbar . #t)))
   (home-services-getter get-home-services)))


;;;
;;; swaync.
;;;

(define* (feature-swaynotificationcenter
          #:key
          (libnotify libnotify)
          (swaynotificationcenter swaynotificationcenter))
  "Configure Sway Notification Center."

  (ensure-pred file-like? swaynotificationcenter)
  (define f-name 'swaynotificationcenter)

  (define (get-home-services config)
    (define (swaync-client args)
      #~(format #f "\"~a/bin/swaync-client ~a\""
                ;; "hi"
                #$(get-value 'swaynotificationcenter config)
                #$args))
    (append
     (list
      (simple-service
       'swaynotificationcenter-add-package
       home-profile-service-type
       (list swaynotificationcenter))
      (waybar-module
       'custom/notification
       `((tooltip . #f)
         (format . "{icon}")
         (format-icons
          .
          ((none . "🔔")
           (notification . "🔔")
           (inhibited-none . "🔔")
           (inhibited-notification . "🔔")
           (dnd-none . "🔕")
           (dnd-notification . "🔕")
           (dnd-inhibited-none . "🔕")
           (dnd-inhibited-notification . "🔕")))
         (return-type . "json")
         (exec . ,(swaync-client "-swb"))
         (on-click . ,(swaync-client "-t -sw"))
         (on-click-right . ,(swaync-client "-d -sw"))
         (on-click-middle . ,(swaync-client "-C -sw"))
         (escape . #t))
       `((#((#{#custom-notification.notification}#)
            (#{#custom-notification.dnd-notification}#)
            (#{#custom-notification.dnd-inhibited-notification}#))
          ((color . @base07)
           (background . @base02))))))

      (list
       (when (get-value 'sway config #f)
         (simple-service
          'sway-swaync
          home-sway-service-type
          `((bindsym --to-code $mod+Shift+n exec ,(swaync-client "-t -sw"))
            (bindsym --to-code $mod+Control+n exec ,(swaync-client "-t -sw"))
            (bindsym --to-code XF86NotificationCenter
                     exec ,(swaync-client "-t -sw"))
            (exec ,(file-append swaynotificationcenter "/bin/swaync"))))))))

  (feature
   (name f-name)
   (values `((desktop-notifications . #t)
             (libnotify . ,libnotify)
             (swaynotificationcenter . ,swaynotificationcenter)))
   (home-services-getter get-home-services)))


;;;
;;; swayidle.
;;;

(define* (feature-swayidle
          #:key
          (swayidle swayidle)
          (lock-timeout 240)
          (extra-config '()))
  "Configure swayidle."
  (ensure-pred file-like? swayidle)

  (define swayidle-cmd (file-append swayidle "/bin/swayidle -w"))

  (define (get-home-services config)
    (define lock-cmd (get-value 'default-screen-locker config #f))

    (list
     (service
      home-swayidle-service-type
      (home-swayidle-configuration
       (swayidle swayidle)
       (config
        `(,@(if lock-cmd
                (let ((lock-cmd-quoted #~(format #f "'~a'" #$lock-cmd)))
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
  (ensure-pred file-like? swaylock)

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
     (service
      screen-locker-service-type
      (screen-locker-configuration
       (name "swaylock")
       (program (file-append swaylock "/bin/swaylock"))
       (using-setuid? #f)
       (using-pam? #t)))))

  (feature
   (name 'swaylock)
   (values
    `((swaylock . ,swaylock)
      ,@(if default-screen-locker?
            `((default-screen-locker . ,(file-append swaylock "/bin/swaylock")))
            '())))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))


;;;
;;; swaykbdd.
;;;

(define* (feature-swaykbdd
          #:key
          (swaykbdd swaykbdd)
          (tab-applications '()))
  "Sets per-application or per-tab layout.  List app_id in
@code{tab-applications}, can be obtained with @code{swaymsg -t get_tree}.

We don't set @code{tab-applications}, because it uses window title to
determine the layout for a tab, which works horrible for sites with dynamic
titles (for example translators or search engines, which change the title
depending on the content you input) and for sites with the same title, but
having different session.  So for consistent experience it's recommended not
to use this functionality."
  (ensure-pred file-like? swaykbdd)
  (ensure-pred list-of-strings? tab-applications)

  (define f-name 'swaykbdd)

  (define (get-home-services config)
    (require-value 'sway config)
    (list
     (simple-service
      'swaykbdd-package
      home-profile-service-type
      (list swaykbdd))
     (simple-service
      'swaykbdd-shepherd-service
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision '(swaykbdd))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append swaykbdd "/bin/swaykbdd")
                        "-a" ;; --tabapps=
                        #$(string-join tab-applications ","))
                  #:log-file (string-append
                              (getenv "XDG_STATE_HOME") "/log"
                              "/swaykbdd.log"))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; kanshi.
;;;

(define* (feature-kanshi
          #:key
          (kanshi kanshi)
          (extra-config '()))
  "Configure kanshi."
  (ensure-pred file-like? kanshi)

  (define (get-home-services config)
    (list
     (service
      home-kanshi-service-type
      (home-kanshi-configuration
       (kanshi kanshi)
       (config
        `(,@extra-config))))))

  (feature
   (name 'kanshi)
   (values `((kanshi . ,kanshi)))
   (home-services-getter get-home-services)))


;;;
;;; batsignal.
;;;

(define* (feature-batsignal
          #:key
          (warning-level 15)
          (critical-level 10)
          (danger-level 5))
  "Configure desktop notifications for battery status levels."
  (ensure-pred integer? warning-level)
  (ensure-pred integer? critical-level)
  (ensure-pred integer? danger-level)

  (define (get-home-services config)
    (list
     (service
      home-batsignal-service-type
      (home-batsignal-configuration
       (warning-level warning-level)
       (critical-level critical-level)
       (danger-level danger-level)))))

  (feature
   (name 'batsignal)
   (values `((batsignal . #t)))
   (home-services-getter get-home-services)))

;; [X] feature-sway-run-on-tty
;; [X] feature-sway-screenshot
;; [X] feature-sway-lock-idle-sleep
;; [ ] feature-sway-input
;; [ ] feature-sway-keybindings
;; [ ] feature-sway-media-keys
;; [X] feature-sway-outputs (kanshi, workspaces, displays)

;; [ ] feature-wayland-appearance (sway, gtk, qt themes)
;; [X] feature-wayland-statusbar
;; [X] feature-wayland-notifications
;; [ ] feature-wayland-clipboard


;; window rules will configured on app's feature basis
