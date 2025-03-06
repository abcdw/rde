;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
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

(define-module (contrib features wm)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)
  #:use-module (rde home services shells)
  #:use-module (rde serializers elisp)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system setuid)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (feature-emacs-exwm
            feature-emacs-exwm-run-on-tty))

(define %default-xorg-modules (list xf86-input-libinput))
(define %default-xorg-server-arguments '("-nolisten" "tcp"))
(define %default-xorg-fonts
  (list (file-append font-alias "/share/fonts/X11/75dpi")
        (file-append font-alias "/share/fonts/X11/100dpi")
        (file-append font-alias "/share/fonts/X11/misc")
        (file-append font-alias "/share/fonts/X11/cyrillic")
        (file-append font-misc-misc "/share/fonts/X11/misc")
        (file-append font-adobe75dpi "/share/fonts/X11/75dpi")))
(define %default-xorg-libinput-configuration
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"
  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethdod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection")

(define-record-type* <xorg-configuration>
  xorg-configuration make-xorg-configuration
  xorg-configuration?
  (modules
   xorg-configuration-modules
   (thunked)
   (default (filter
             (lambda (p)
               (member (%current-system)
                       (package-supported-systems p)))
             %default-xorg-modules)))
  (fonts
   xorg-configuration-fonts
   (default %default-xorg-fonts))
  (drivers
   xorg-configuration-drivers
   (default '()))
  (resolutions
   xorg-configuration-resolutions
   (default '()))
  (keyboard-layout
   xorg-configuration-keyboard-layout
   (default #f))
  (extra-config
   xorg-configuration-extra-config
   (default '()))
  (server
   xorg-configuration-server
   (default xorg-server))
  (server-arguments
   xorg-configuration-server-arguments
   (default %default-xorg-server-arguments)))

(define (xorg-configuration->file config)
  "Compute an Xorg configuration file corresponding to CONFIG, an
<xorg-configuration> record."
  (let ((xorg-server (xorg-configuration-server config)))
    (define all-modules
      (append (xorg-configuration-modules config)
              (list xorg-server)))

    (define build
      #~(begin
          (use-modules (ice-9 match)
                       (srfi srfi-1)
                       (srfi srfi-26))

          (call-with-output-file #$output
            (lambda (port)
              (define drivers
                '#$(xorg-configuration-drivers config))

              (define (device-section driver)
                (string-append "
Section \"Device\"
  Identifier \"device-" driver "\"
  Driver \"" driver "\"
EndSection"))

              (define (screen-section driver resolutions)
                (string-append "
Section \"Screen\"
  Identifier \"screen-" driver "\"
  Device \"device-" driver "\"
  SubSection \"Display\"
    Modes "
  (string-join (map (match-lambda
                      ((x y)
                       (string-append "\"" (number->string x)
                                      "x" (number->string y) "\"")))
                    resolutions)) "
  EndSubSection
EndSection"))

              (define (input-class-section layout variant model options)
                (string-append "
Section \"InputClass\"
  Identifier \"evdev keyboard catchall\"
  MatchIsKeyboard \"on\"
  Option \"XkbLayout\" " (object->string layout)
  (if variant
      (string-append "  Option \"XkbVariant\" \"" variant "\"")
      "")
  (if model
      (string-append "  Option \"XkbModel\" \"" model "\"")
      "")
  (match options
    (() "")
    (_ (string-append "  Option \"XkbOptions\" \""
                      (string-join options ",") "\""))) "

  MatchDevicePath \"/dev/input/event*\"
  Driver \"evdev\"
EndSection\n"))

              (define (expand modules)
                (append-map (lambda (module)
                              (filter-map (lambda (directory)
                                            (let ((full (string-append module
                                                                       directory)))
                                              (and (file-exists? full)
                                                   full)))
                                          '("/lib/xorg/modules/drivers"
                                            "/lib/xorg/modules/input"
                                            "/lib/xorg/modules/multimedia"
                                            "/lib/xorg/modules/extensions")))
                            modules))

              (display "Section \"Files\"\n" port)
              (for-each (lambda (font)
                          (format port "  FontPath \"~a\"~%" font))
                        '#$(xorg-configuration-fonts config))
              (for-each (lambda (module)
                          (format port "  ModulePath \"~a\"~%" module))
                        (append (expand '#$all-modules)
                                (list #$(file-append xorg-server
                                                     "/lib/xorg/modules"))))
              (display "EndSection\n" port)
              (display "
Section \"ServerFlags\"
  Option \"AllowMouseOpenFail\" \"on\"
EndSection\n" port)
              (display (string-join (map device-section drivers) "\n")
                       port)
              (newline port)
              (display (string-join
                        (map (cut screen-section <>
                                  '#$(xorg-configuration-resolutions config))
                             drivers)
                        "\n")
                       port)
              (newline port)
              (let ((layout #$(and=> (xorg-configuration-keyboard-layout config)
                                     keyboard-layout-name))
                    (variant #$(and=> (xorg-configuration-keyboard-layout config)
                                      keyboard-layout-variant))
                    (model #$(and=> (xorg-configuration-keyboard-layout config)
                                    keyboard-layout-model))
                    (options '#$(and=> (xorg-configuration-keyboard-layout config)
                                       keyboard-layout-options)))
                (when layout
                  (display (input-class-section layout variant model options)
                           port)
                  (newline port)))

              (for-each (lambda (config)
                          (display config port))
                        '#$(xorg-configuration-extra-config config))))))

    (computed-file "xserver.conf" build)))

(define (xorg-configuration-directory modules)
  "Return a directory that contains the @code{.conf} files for X.org that
includes the @code{share/X11/xorg.conf.d} directories of each package listed
in @var{modules}."
  (with-imported-modules '((guix build utils))
    (computed-file "xorg.conf.d"
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-1))

                       (define files
                         (append-map (lambda (module)
                                       (find-files (string-append
                                                    module
                                                    "/share/X11/xorg.conf.d")
                                                   "\\.conf$"))
                                     (list #$@modules)))

                       (mkdir #$output)
                       (for-each (lambda (file)
                                   (symlink file
                                            (string-append #$output "/"
                                                           (basename file))))
                                 files)
                       #t))))

(define* (xorg-wrapper xinitrc #:optional (config (xorg-configuration)))
  "Return a derivation that builds a script to start the X server with the
given @var{config}.  The resulting script should be used in place of
@code{/usr/bin/X}."
  (define exp
    #~(begin
        (setenv "XORG_DRI_DRIVER_PATH" (string-append #$mesa "/lib/dri"))
        (setenv "XKB_BINDIR" (string-append #$xkbcomp "/bin"))
        (let ((xinit #$(file-append xinit "/bin/xinit"))
              (X (string-append #$(xorg-configuration-server config) "/bin/X")))
          (apply execl xinit xinit
                 #$xinitrc
                 "--"
                 X
                 ":0"
                 "vt1"
                 "-keeptty"
                 "-xkbdir" (string-append #$xkeyboard-config "/share/X11/xkb")
                 "-config" #$(xorg-configuration->file config)
                 "-configdir" #$(xorg-configuration-directory
                                 (xorg-configuration-modules config))
                 (cdr (command-line))))))

  (program-file "X-wrapper" exp))

(define* (xorg-start-command xinitrc #:optional (config (xorg-configuration)))
  "Return a @code{startx} script in which the modules, fonts, etc. specified
in @var{config}, are available.  The result should be used in place of
@code{startx}."
  (define X (xorg-wrapper xinitrc config))
  #~(apply execl #$X #$X
           "-logverbose" "-verbose" "-terminate"
           #$@(xorg-configuration-server-arguments config)
           (cdr (command-line))))

(define* (xinitrc #:key command args)
  "Return a xinitrc script that starts xorg with the specified COMMAND and ARGS."
  (program-file
   "xinitrc"
   #~(system* #$command #$@args)))


;;;
;;; EXWM.
;;;

(define* (feature-emacs-exwm
          #:key
          (emacs-exwm emacs-exwm)
          (workspace-number 3)
          (window-configurations '())
          (floating-window-border-width 0)
          (floating-window-border-color "#212121")
          (extra-exwm-bindings '())
          (extra-exwm-init '()))
  "Configure the Emacs X Window Manager."
  (ensure-pred file-like? emacs-exwm)
  (ensure-pred integer? workspace-number)
  (ensure-pred elisp-config? window-configurations)
  (ensure-pred integer? floating-window-border-width)
  (ensure-pred string? floating-window-border-color)
  (ensure-pred elisp-config? extra-exwm-bindings)
  (ensure-pred elisp-config? extra-exwm-init)

  (define emacs-f-name 'exwm)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EXWM."
    (require-value 'emacs config)
    (define xrandr-bin
      (file-append
       (get-value 'xrandr config (@ (gnu packages xorg) xrandr))
       "/bin/xrandr"))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
          (require 'cl-macs))
        (defgroup rde-exwm nil
          "Helpers for EXWM."
          :group 'rde)
        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              `((defvar rde-exwm-buffer-source
                  `(:name "EXWM"
                    :hidden t
                    :narrow ?x
                    :category buffer
                    :state ,'consult--buffer-state
                    :items ,(lambda () (mapcar 'buffer-name
                                               (rde-completion--mode-buffers
                                                'exwm-mode))))
                  "Source for EXWM buffers to be set in
`consult-buffer-sources'.")
                (with-eval-after-load 'consult
                  (add-to-list 'consult-buffer-sources rde-exwm-buffer-source
                               'append))
                (with-eval-after-load 'rde-completion
                  (add-to-list 'rde-completion-initial-narrow-alist
                               '(exwm-mode . ?x))))
              '())

        ,@(if (get-value 'emacs-tab-bar config)
              '((defun rde-exwm--disable-tab-bar (frame)
                  "Disable the tab-bar on new Emacs FRAME."
                  (set-frame-parameter frame 'tab-bar-lines 0)))
              '())

        (defun rde-exwm--get-outputs ()
          "Return the currently-connected RandR outputs."
          (let ((xrandr-output-regexp (rx "\n" bol (group (+ any))
                                          " connected"))
                outputs)
            (with-temp-buffer
              (call-process ,xrandr-bin nil t nil)
              (goto-char (point-min))
              (re-search-forward xrandr-output-regexp nil 'noerror)
              (setq outputs (match-string 1))
              (forward-line)
              (while (re-search-forward xrandr-output-regexp nil 'noerror)
                (setq outputs (append (list outputs) (list (match-string 1)))))
              outputs)))

        (defun rde-exwm-apply-display-settings ()
          "Apply the corresponding display settings after EXWM is enabled."
          (interactive)
          (cl-loop for i from 0 upto (- exwm-workspace-number 1)
                   do (progn
                        (exwm-workspace-switch-create i)
                        (set-frame-parameter
                         (selected-frame) 'internal-border-width
                         ,(get-value 'emacs-margin config 0)))
                   finally (progn
                            (exwm-workspace-switch-create 0)
                            ,@(if (get-value 'emacs-tab-bar config)
                                  '((add-hook 'after-make-frame-functions
                                              'rde-exwm--disable-tab-bar))
                                  '())
                            ,@extra-exwm-init)))

        (defun rde-exwm-shorten-buffer-name ()
          "Shorten EXWM buffer names to be more discernible."
          (interactive)
          (exwm-workspace-rename-buffer
           (concat exwm-class-name ": "
                   (if (<= (length exwm-title) 30)
                       exwm-title
                     (concat (substring exwm-title 0 29) "...")))))

        (defun rde-exwm--call-xrandr (&rest args)
          "Call `xrandr' with the supplied ARGS."
          (apply 'start-process "xrandr" nil ,xrandr-bin args))

        (defun rde-exwm--get-resolution ()
          "Prompt the user for a list of available resolutions."
          (with-temp-buffer
            (call-process ,xrandr-bin nil t nil)
            (goto-char (point-min))
            (unless (re-search-forward
                     (rx "\n" bol (+ any) " connected primary")
                     nil 'noerror)
              (goto-char (point-min))
              (re-search-forward (rx bol (+ any) "connected") nil 'noerror))
            (let ((resolutions
                   (cl-loop while (not (eobp))
                            do (forward-line 1)
                            when (re-search-forward
                                  (rx (+ blank) (group (+ num) "x" (+ num))
                                      (+ blank) (+ num))
                                  nil 'noerror)
                            collect (match-string 1))))
              (completing-read
               "Select resolution: "
               (lambda (string pred action)
                 (if (eq action 'metadata)
                     `(metadata
                       ,(cons 'display-sort-function 'identity))
                     (complete-with-action action resolutions string pred)))))))

        (defun rde-exwm-change-resolution ()
          "Change the resolution of the primary RandR output."
          (interactive)
          (rde-exwm-automatic-output-mode -1)
          (when-let ((resolution (rde-exwm--get-resolution))
                     (outputs (rde-exwm--get-outputs)))
            (set-process-sentinel
             (if (listp outputs)
                 (rde-exwm--call-xrandr "--output" (cadr outputs) "--primary"
                                        "--mode" resolution "--output"
                                        (car outputs) "--off")
               (rde-exwm--call-xrandr "--output" outputs "--mode" resolution))
             (lambda (_process event)
               (when (string= event "finished\n")
                 (rde-exwm-automatic-output-mode))))))

        (defun rde-exwm-update-output ()
          "Update RandR output configuration."
          (interactive)
          (when-let ((secondary-output-regexp
                      (rx "\n" blank (+ any) blank (group (+ nonl))))
                     (outputs (rde-exwm--get-outputs)))
            (if (listp outputs)
                (progn
                  (rde-exwm--call-xrandr
                   "--output" (cadr outputs) "--primary"
                   "--auto" "--output" (car outputs) "--off")
                  (setq exwm-randr-workspace-monitor-plist
                        (list 0 (cadr outputs))))
              (rde-exwm--call-xrandr "--output" outputs "--auto")
              (with-temp-buffer
                (call-process ,xrandr-bin nil t nil "--listactivemonitors")
                (goto-char (point-min))
                (while (not (eobp))
                  (when (and (re-search-forward
                              secondary-output-regexp nil 'noerror)
                             (not (string= (match-string 1) outputs)))
                    (rde-exwm--call-xrandr "--output" (match-string 1)
                                           "--auto")))))))

        (define-minor-mode rde-exwm-automatic-output-mode
          "Set up automatic handling of RandR outputs."
          :global t :group 'rde-exwm
          (if rde-exwm-automatic-output-mode
              (add-hook 'exwm-randr-screen-change-hook
                        'rde-exwm-update-output)
            (remove-hook 'exwm-randr-screen-change-hook
                         'rde-exwm-update-output)))

        (add-hook 'exwm-init-hook 'rde-exwm-apply-display-settings)
        (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
        (add-hook 'exwm-update-class-hook 'rde-exwm-shorten-buffer-name)
        (add-hook 'exwm-update-title-hook 'rde-exwm-shorten-buffer-name)
        (setq exwm-input-global-keys
              (append
               (list
                (cons (kbd "s-s")
                      '(lambda ()
                         (interactive)
                         (exwm-layout-shrink-window-horizontally 40)))
                (cons (kbd "s-e")
                      '(lambda ()
                         (interactive)
                         (exwm-layout-enlarge-window-horizontally 40)))
                (cons (kbd "s-C-s")
                      '(lambda ()
                         (interactive)
                         (exwm-layout-shrink-window 40)))
                (cons (kbd "s-C-e")
                      '(lambda ()
                         (interactive)
                         (exwm-layout-enlarge-window 40)))
                (cons (kbd "s-f") 'exwm-layout-toggle-fullscreen)
                (cons (kbd "s-r") 'exwm-reset)
                (cons (kbd "s-x") 'rde-exwm-change-resolution)
                (cons (kbd "s-t") 'exwm-floating-toggle-floating)
                (cons (kbd "s-i") 'exwm-input-toggle-keyboard)
                (cons (kbd "s-q") 'kill-this-buffer)
                (cons (kbd "s-<return>") 'split-window-horizontally)
                (cons (kbd "s-m") 'exwm-workspace-move-window)
                (cons (kbd "s-w") 'exwm-workspace-switch)
                (cons (kbd "s-`")
                      '(lambda ()
                         (interactive)
                         (exwm-workspace-switch 0)))
                ,@extra-exwm-bindings)
               (mapcar (lambda (i)
                         (cons (kbd (format "s-%d" i))
                               `(lambda ()
                                  (interactive)
                                  (exwm-workspace-switch ,i))))
                       (number-sequence 0 ,workspace-number))))
        (add-hook 'after-init-hook 'exwm-enable)
        (with-eval-after-load 'exwm
          (setq exwm-input-prefix-keys
                (append exwm-input-prefix-keys `(,(kbd "M-s") ,(kbd "s-e"))))
          (setq exwm-input-simulation-keys
                (list
                 (cons (kbd "C-b") (kbd "<left>"))
                 (cons (kbd "C-f") (kbd "<right>"))
                 (cons (kbd "C-p") (kbd "<up>"))
                 (cons (kbd "C-n") (kbd "<down>"))
                 (cons (kbd "C-a") (kbd "<home>"))
                 (cons (kbd "C-e") (kbd "<end>"))
                 (cons (kbd "M-v") (kbd "<prior>"))
                 (cons (kbd "C-v") (kbd "<next>"))
                 (cons (kbd "C-d") (kbd "<delete>"))
                 (cons (kbd "C-k") (kbd "<S-end> <delete>"))))
          (setq exwm-layout-show-all-buffers nil)
          (setq exwm-workspace-number ,workspace-number)
          (setq exwm-workspace-show-all-buffers nil)
          (setq exwm-workspace-minibuffer-position nil)
          (setq exwm-workspace-warp-cursor t)
          (setq exwm-workspace-switch-create-limit ,workspace-number)
          (setq exwm-floating-border-color ,floating-window-border-color)
          (setq exwm-floating-border-width ,floating-window-border-width)
          (setq exwm-manage-configurations ',window-configurations)
          (require 'exwm-randr)
          (exwm-randr-enable)
          (rde-exwm-update-output)
          (add-hook 'exwm-randr-screen-change-hook 'rde-exwm-update-output)))
      #:elisp-packages (list emacs-exwm)
      #:summary "Utilities for EXWM"
      #:commentary "Helpers for EXWM to switch outputs automatically, change
 resolution on-the-fly, and set up the initial workspace configuration.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-exwm)))
   (home-services-getter get-home-services)))


;;;
;;; emacs-exwm-run-on-tty.
;;;

(define* (feature-emacs-exwm-run-on-tty
          #:key
          (emacs-exwm-tty-number 2)
          (xorg-libinput-configuration
           %default-xorg-libinput-configuration)
          (extra-xorg-config '())
          (launch-arguments '()))
  "Launch EXWM on a specified TTY upon user login and
automatically switch to EXWM-TTY-NUMBER on boot."
  (ensure-pred tty-number? emacs-exwm-tty-number)
  (ensure-pred string? xorg-libinput-configuration)
  (ensure-pred list? extra-xorg-config)
  (ensure-pred list? launch-arguments)

  (define (get-home-services config)
    "Return home services related to EXWM run on TTY."
    (require-value 'emacs-exwm config)
    (require-value 'keyboard-layout config)

    (list
     (simple-service
      'run-exwm-on-login-tty
      home-shell-profile-service-type
      (list
       #~(format #f "[ $(tty) = /dev/tty~a ] && exec ~a"
                 #$emacs-exwm-tty-number
                 #$(program-file
                    "exwm-start"
                    (xorg-start-command
                     (xinitrc #:command (file-append (get-value 'emacs config)
                                                     "/bin/emacs")
                              #:args launch-arguments)
                     (xorg-configuration
                      (keyboard-layout (get-value 'keyboard-layout config))
                      (extra-config
                       (interpose
                        (append
                         (list xorg-libinput-configuration)
                         extra-xorg-config)))))))))))

  (define (get-system-services _)
    "Return system services related to EXWM run on TTY."
    (list
     (simple-service
      'switch-to-exwm-tty-after-boot
      shepherd-root-service-type
      (list
       (shepherd-service
        (provision '(switch-to-exwm-tty))
        (requirement '(virtual-terminal))
        (start #~(lambda ()
                   (invoke #$(file-append kbd "/bin/chvt")
                           #$(format #f "~a" emacs-exwm-tty-number))))
        (one-shot? #t))))))

  (feature
   (name 'emacs-exwm-run-on-tty)
   (values (make-feature-values emacs-exwm-tty-number))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
