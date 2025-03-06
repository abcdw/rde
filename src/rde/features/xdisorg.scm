(define-module (rde features xdisorg)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (rde features fontutils)
  #:use-module (rde packages)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (rde home services xdisorg)
  #:use-module (rde system services accounts)
  #:use-module (guix gexp)
  #:export (feature-rofi
            feature-ydotool))


;;;
;;; rofi.
;;;

(define* (feature-rofi
          #:key
          (rofi rofi-wayland)
          (theme "Arc")
          (show-icons? #t)
          (show-actions? #t)
          (default-application-launcher? #t))
  "Configure rofi."
  (ensure-pred file-like? rofi)
  (ensure-pred file-like-or-path? theme)

  (define (get-home-services config)
    (define font
      (and=> (get-value 'font-monospace config #f)
             font-specification))

    (list
     (service
      home-rofi-service-type
      (home-rofi-configuration
       (rofi rofi)
       (config-rasi
        `((configuration
           ((modi . "run,ssh,drun")
            (drun-show-actions . ,show-actions?)
            (show-icons . ,show-icons?)
            ,@(if font `((font . ,font)) '())
            (kb-element-next . "")
            (kb-row-select . "Tab,Control+i")
            (kb-secondary-paste . "Control+y")
            (kb-remove-word-forward . "Alt+d")
            (kb-remove-word-back . "Control+w,Control+BackSpace")
            (kb-clear-line . "Control+slash")
            (kb-page-next . "Control+v")
            (kb-page-prev . "Alt+v")))
          ,#~(format #f "@theme \"~a\"" #$theme)))))))

  (feature
   (name 'rofi)
   (values `((rofi . ,rofi)
             ,@(if default-application-launcher?
                   `((default-application-launcher-fn .
                       ,(const (file-append rofi "/bin/rofi -show drun"))))
                   '())))
   (home-services-getter get-home-services)))


;;;
;;; ydotool.
;;;

(define* (feature-ydotool
          #:key
          (ydotool ydotool))
  "Configure Ydotool."
  (ensure-pred file-like? ydotool)

  (define (get-home-services config)
    (list
     (simple-service
      'ydotool-add-ydotool-package
      home-profile-service-type
      (list ydotool))
     (simple-service
      'start-ydotoold-at-startup
      home-shepherd-service-type
      (list (shepherd-service
             (documentation "Run the ydotool daemon (ydotoold).")
             (provision '(ydotool))
             (requirement '())
             (start #~(make-forkexec-constructor
                       (list (string-append
                              #$ydotool "/bin/ydotoold"))))
             (one-shot? #f)
             (stop #~(make-kill-destructor)))))))

  (define (get-system-services _)
    (list
     (simple-service
      'ydotool-add-input-group-to-user
      rde-account-service-type
      (list "input"))
     (udev-rules-service
      'ydotool
      (udev-rule
       "80-uinput.rules"
       ;; TODO: Take it from ydotool package
       (string-append
        "KERNEL==\"uinput\", MODE==\"0660\", "
        "GROUP=\"input\", OPTIONS+=\"static_node=uinput\"")))))

  (feature
   (name 'ydotool)
   (values `((ydotool . ,ydotool)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
