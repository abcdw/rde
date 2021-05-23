(define-module (rde features base)
  #:use-module (rde features)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu packages certs)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)

  #:export (feature-user-info
	    feature-base-packages))

(define (maybe-string? x)
  (or (string? x) (not x)))

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


(define (list-of-packages? lst)
  (and (list? lst) (every package? lst)))

;; TODO: Cleanup the list of base system packages, it contains some
;; unecessary for rde packages (some network, fs utils)
;; (display
;;  (map package-name %base-system-packages))
(define %base-system-packages
  (append
   (list nss-certs)
   %base-packages-disk-utilities
   %base-packages))

(define* (feature-base-packages
	  #:key
	  (home-packages '())
	  (system-packages '())
	  (base-system-packages %base-system-packages))
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
      'add-packages-to-system-profile
      profile-service-type
      (append system-packages
	      base-system-packages))))

  (feature
   (name 'packages)
   (home-services-getter get-home-packages)
   (system-services-getter get-system-packages)))
