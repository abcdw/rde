(define-module (rde features keyboard)
  #:use-module (rde features)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (gnu home-services keyboard)
  #:export (feature-keyboard
	    %dvorak-layout
	    %dvorak-jcuken-layout
        %thinkpad-layout))

;; Example of multi-layer layout: https://neo-layout.org/index_en.html

(define %dvorak-layout
  (keyboard-layout "us" "dvorak" #:options '("ctrl:nocaps")))

(define %dvorak-jcuken-layout
  (keyboard-layout
   "us,ru" "dvorak,"
   #:options '("grp:win_space_toggle" "ctrl:nocaps")))

(define %thinkpad-layout
  (keyboard-layout
   "us" "altgr-intl"
   #:model "thinkpad"
   #:options '("ctrl:nocaps")))

;; TODO: Add ability to provide custom layout package or file

;; There is no default value to force user specify some keyboard
;; layout in case they use this feature
(define* (feature-keyboard #:key keyboard-layout)
  "Sets keyboard layout.  Affects bootloader, and XKB_* variables for
the user."
  (ensure-pred keyboard-layout? keyboard-layout)

  (define (keyboard-services values)
    "Returns home-keyboard service."
    (list
     (service home-keyboard-service-type keyboard-layout)))

  (feature
   (name 'keyboard)
   (values (make-feature-values keyboard-layout))
   (home-services-getter keyboard-services)))

