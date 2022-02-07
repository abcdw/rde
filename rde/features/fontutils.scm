(define-module (rde features fontutils)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu services)
  #:use-module (gnu packages fonts)
  #:use-module (srfi srfi-9)

  #:export (feature-fonts

	    font
	    font-size
	    font-name
	    font-weight))

(define %rde-default-font-packages
  (list font-iosevka
	font-dejavu
	font-liberation
	font-gnu-unifont
	font-fira-go))

(define-record-type <font>
  (%font name size weight)
  font?
  (name font-name)
  (size font-size)
  (weight font-weight))

(define* (font name #:key size weight)
  (%font name size weight))

(define* (feature-fonts
	  #:key
          (default-font-size 11)
	  (font-monospace (font "Iosevka"
                                #:size default-font-size #:weight 'regular))
	  (font-sans      (font "Fira Go"))
	  (font-serif     (font "Liberation Serif"))
	  (font-unicode   (font "Unifont"))
	  (font-packages  '())
	  (base-font-packages  %rde-default-font-packages))
  "Configure fonts.  DEFAULT-FONT-SIZE will be used for making
font-monospace default value, and it will be ignored if
#:font-monospace argument is specified."

  (ensure-pred integer? default-font-size)
  (ensure-pred font? font-monospace)
  (ensure-pred list-of-packages? font-packages)
  (ensure-pred list-of-packages? base-font-packages)

  ;; TODO: Make it configure fonts via fontconfig home service
  ;; (requires adding extending capabilities to service)
  (define (fonts-home-services config)
    "Returns home services related to fonts."
    (list
     (simple-service
      'font-packages
      home-profile-service-type
      (append
       font-packages
       base-font-packages))))

  (feature
   (name 'fonts)
   (values (append
	    `((fonts . #t))
	    (make-feature-values font-monospace font-sans font-serif font-unicode)))
   (home-services-getter fonts-home-services)))
