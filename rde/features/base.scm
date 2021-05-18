(define-module (rde features base)
  #:use-module (rde features)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu packages certs)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)

  #:export (feature-user-info))

(define (maybe-string? x)
  (or (string? x) (not x)))

(define* (feature-user-info
	  #:key user-name full-name email
	  (home-directory (format #f "/home/~a" user-name))
	  (user-password #f))
  "Provides basic information about user for all features."
  (ensure-pred string? user-name)
  (ensure-pred string? full-name)
  (ensure-pred string? email)
  (ensure-pred string? home-directory)
  (ensure-pred maybe-string? user-password)

  (feature
   (name 'user-info)
   (values (make-feature-values
	    user-name full-name email home-directory user-password))))


(define (list-of-packages? lst)
  (and (list? lst) (every package? lst)))

;; TODO: Cleanup the list of base system packages, it contains some
;; unecessary for rde packages
(define %base-system-packages
  (append
   (list nss-certs)
   %base-packages-disk-utilities
   %base-packages))

(define* (feature-packages
	  #:key
	  (home-packages '())
	  (system-packages '())
	  (base-system-packages %base-system-packages))
  "Allows to specify standalone packages for home-environment, or
operating-system, or both."
  (ensure-pred list-of-packages? home-packages)
  (ensure-pred list-of-packages? system-packages)

  (define (get-home-packages values)
    (list
     (service home-profile-service-type home-packages)))

  (define (get-system-packages values)
    (list
     (service profile-service-type
	      (append system-packages
		      base-system-packages))))

  (feature
   (name 'packages)
   (home-services-getter get-home-packages)
   (system-services-getter get-system-packages)))
