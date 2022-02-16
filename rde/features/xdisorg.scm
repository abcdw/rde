(define-module (rde features xdisorg)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde packages)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services xdisorg)
  #:use-module (guix gexp)
  #:export (feature-rofi))


;;;
;;; rofi.
;;;

(define* (feature-rofi
          #:key
          (rofi rofi-wayland)
          (theme #f)
          (default-application-launcher? #t))
  "Configure rofi."
  (ensure-pred any-package? rofi)

  (define (get-home-services config)
    (list
     (service
      home-rofi-service-type
      (home-rofi-configuration
       (rofi rofi)
       (config-rasi
        `((configuration
           ((modi . "run,ssh,drun")
            (drun-show-actions . #t)
            (show-icons)
            (kb-row-tab . "")
            (kb-row-select . "Tab")
            (kb-secondary-paste . "Control+y")
            (kb-remove-word-forward . "Alt+d")
            (kb-remove-word-back . "Control+w,Control+BackSpace")
            (kb-clear-line . "Control+slash")
            (kb-page-next . "Control+v")
            (kb-page-prev . "Alt+v")))
          ,#~(format
              #f "@theme \"~a\""
              (or #$theme
                  #$(file-append rofi "/share/rofi/themes/Arc.rasi")))))))))

  (feature
   (name 'rofi)
   (values `((rofi . ,rofi)
             ,@(if default-application-launcher?
                   `((default-application-launcher .
                       ,(file-append rofi "/bin/rofi -show drun")))
                   '())))
   (home-services-getter get-home-services)))

