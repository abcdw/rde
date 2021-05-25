(define-module (rde features base)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu home-services)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages fonts)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)

  #:export (feature-user-info
	    feature-base-packages
	    feature-base-services
	    feature-desktop-services
	    feature-hidpi
	    feature-generic))

(define* (feature-user-info
	  #:key user-name full-name email
	  (home-directory (format #f "/home/~a" user-name))
	  (user-initial-password-hash #f))
  "Provides basic information about user for all features."
  (ensure-pred string? user-name)
  (ensure-pred string? full-name)
  (ensure-pred string? email)
  (ensure-pred string? home-directory)
  (ensure-pred maybe-string? user-initial-password-hash)

  (feature
   (name 'user-info)
   (values (make-feature-values
	    user-name full-name email home-directory
	    user-initial-password-hash))))


;; TODO: Cleanup the list of base system packages, it contains some
;; unecessary for rde packages (some network, fs utils)
;; (display
;;  (map package-name %base-system-packages))
(define %rde-base-system-packages
  (append
   (list nss-certs)
   %base-packages-disk-utilities
   %base-packages))

(define* (feature-base-packages
	  #:key
	  (home-packages '())
	  (system-packages '())
	  (base-system-packages %rde-base-system-packages))
  "Provides base packages and allows to specify additional standalone
packages for home-environment, or operating-system, or both.
Standalone means that packages do not require configuration and not
installed by system or home services."
  (ensure-pred list-of-packages? home-packages)
  (ensure-pred list-of-packages? system-packages)
  (ensure-pred list-of-packages? base-system-packages)

  (define (get-home-packages values)
    (list
     (service home-profile-service-type home-packages)))

  (define (get-system-packages values)
    (list
     (simple-service
      'add-base-packages-to-system-profile
      profile-service-type
      (append system-packages
	      base-system-packages))))

  (feature
   (name 'base-packages)
   (home-services-getter get-home-packages)
   (system-services-getter get-system-packages)))


(define %rde-base-services
  %base-services)

(define %rde-desktop-services
  (remove (lambda (service)
	    (member (service-kind service)
		    (append
		     (map service-kind %rde-base-services)
		     (list gdm-service-type screen-locker-service-type))))
	  %desktop-services))

;; ((@@ (ice-9 pretty-print) pretty-print)
;;  (map service-kind  %base-services))

(define* (feature-base-services
	  #:key
	  (guix-substitute-urls '())
	  (guix-authorized-keys '())
	  (udev-rules '())
	  (base-services %rde-base-services))
  "Provides base system services."
  (define (get-base-system-services cfg)
    (modify-services base-services
      (console-font-service-type
       config =>
       (map (lambda (x)
	      (cons
	       (format #f "tty~a" x)
	       (get-value 'console-font cfg "LatGrkCyr-8x16")))
	    (iota (get-value 'number-of-ttys cfg 6) 1)))
      (guix-service-type
       config =>
       (guix-configuration
        (inherit config)
        (substitute-urls (append
			  guix-substitute-urls
                          %default-substitute-urls))
        (authorized-keys (append
                          guix-authorized-keys
                          %default-authorized-guix-keys))))
      (udev-service-type
       config =>
       (udev-configuration
        (inherit config)
        (rules (append
		udev-rules
		(udev-configuration-rules config)))))))

  (feature
   (name 'base-services)
   (values `((base-services . #t)
	     (number-of-ttys . ,%number-of-ttys)))
   (system-services-getter get-base-system-services)))

(define* (feature-desktop-services)
  "Provides desktop system services."
  (define (get-desktop-system-services _)
    %rde-desktop-services)

  (feature
   (name 'desktop-services)
   (values '((desktop-services . #t)
	     (elogind . #t)))
   (system-services-getter get-desktop-system-services)))


(define* (feature-hidpi
	  #:key
	  (scaling-factor 2)
	  (console-font (file-append
			 font-terminus
			 "/share/consolefonts/ter-132n")))
  "Provides values, which will affect other features, making them more
HiDPI friendly."
  (ensure-pred file-like-or-path? console-font)
  (ensure-pred integer? scaling-factor)

  (feature
   (name 'hidpi)
   (values (make-feature-values scaling-factor console-font))))


(define* (feature-generic
	  #:key
	  feature-name
	  (home-services '())
	  (system-services '()))
  "Allows to specify additional services for home-environment, or
operating-system, or both."
  (ensure-pred symbol? feature-name)
  (ensure-pred list-of-services? home-services)
  (ensure-pred list-of-services? system-services)

  (define (get-home-services values)
    home-services)

  (define (get-system-services values)
    system-services)

  (feature
   (name feature-name)
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
