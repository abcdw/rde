(define-module (gnu services home)
  #:use-module (gnu services guix)

  #:use-module (guix deprecation)

  #:export (guix-home-service-type))

;; Use upstreamed version of the service:
;; https://yhetil.org/guix-patches/05fd930c91838ce9640720ce68e3379f10154590.1710355300.git.richard@freakingpenguin.com/
(define-deprecated/alias
  guix-home-service-type
  (@ (gnu services guix) guix-home-service-type))
