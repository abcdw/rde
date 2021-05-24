(define-module (rde features wm)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home-services shells)
  #:use-module (guix gexp)
  #:export (feature-sway
	    feature-sway-run-on-tty))

;; https://github.com/jjquin/dotfiles/tree/master/sway/.config/sway/config.d

(define (keyboard-layout-to-sway-config keyboard-layout)
  (let ((kb-options (string-join
		     (keyboard-layout-options keyboard-layout) ",")))
    `((input *
	     ((xkb_layout  ,(keyboard-layout-name keyboard-layout))
	      (xkb_variant ,(keyboard-layout-variant keyboard-layout))
	      (xkb_options ,kb-options))))))

(define* (feature-sway
	  #:key
	  config-file
	  (add-keyboard-layout-to-config? #t))
  "Setup and configure sway."
  (ensure-pred maybe-file-like? config-file)
  (ensure-pred boolean? add-keyboard-layout-to-config?)

  (define (sway-home-services config)
    "Returns home services related to sway."
    (let* ((kb-layout      (get-value 'keyboard-layout config))
	   (layout-config  (if (and add-keyboard-layout-to-config? kb-layout)
			       (keyboard-layout-to-sway-config kb-layout)
			       '()))
	   (include-config (if config-file
			       `((include ,config-file))
			       '())))
      (list
       ;; TODO: Move to feature-app-launcher or something like that
       (simple-service 'packages-for-sway
		home-profile-service-type
		(list wofi))
       (simple-service 'set-wayland-specific-env-vars
		       home-environment-variables-service-type
		       '(("_JAVA_AWT_WM_NONREPARENTING" . "1")))
       (service
	home-sway-service-type
	(home-sway-configuration
	 (config
	  `(,@layout-config
	    ,@include-config)))))))

  (define (sway-system-services _)
    "Returns system services related to sway."
    (list
     ;; TODO: Find a better solution for foreign distros?
     ;; TODO: Move it to a separate feature?
     (screen-locker-service swaylock "swaylock")))

  (feature
   (name 'sway)
   (values '((sway . #t)
	     (wayland . #t)))
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
				(format #f "~a" sway-tty-number))))
             (respawn? #f))))))

  (feature
   (name 'sway-run-on-tty)
   (values (make-feature-values sway-tty-number))
   (home-services-getter sway-run-on-tty-home-services)
   (system-services-getter sway-run-on-tty-system-services)))


;; [X] feature-sway-run-on-tty
;; [ ] feature-sway-lock-idle-sleep
;; [ ] feature-sway-input
;; [ ] feature-sway-keybindings
;; [ ] feature-sway-media-keys
;; [ ] feature-sway-screenshot (+ color picker)
;; [ ] feature-sway-outputs (kanshi, workspaces, displays)

;; [ ] feature-wayland-appearance (sway, gtk, qt themes)
;; [ ] feature-wayland-statusbar
;; [ ] feature-wayland-notifications
;; [ ] feature-wayland-clipboard


;; window rules will configured on app's feature basis
