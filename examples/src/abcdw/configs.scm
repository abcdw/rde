(define-module (abcdw configs)
  #:use-module (abcdw hosts ixy)
  #:use-module (abcdw hosts live)
  #:use-module (abcdw users abcdw)
  #:use-module (abcdw users guest)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; TODO: Load user and hosts modules without explicit import
;; (canonicalize-path
;;  (search-path %load-path "abcdw/users/abcdw.scm"))

;; (getcwd)
;; (format #t "path: ~a\n" (canonicalize-path "./users/"))
;; (source-properties ixy-config)


;;; Some TODOs

;; TODO: Add an app for saving and reading articles and web pages
;; https://github.com/wallabag/wallabag
;; https://github.com/chenyanming/wallabag.el

;; TODO: feature-wallpapers https://wallhaven.cc/
;; TODO: feature-icecat
;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>


;;; ixy

(define-public ixy-config
  (rde-config
   (features
    (append
     %ixy-features
     %abcdw-features))))

(define-public ixy-os
  (rde-config-operating-system ixy-config))

(define-public ixy-he
  (rde-config-home-environment ixy-config))


;;; live

;; TODO: Pull channels from lock file in advance and link them to example-config
;; TODO: Add auto-login

(define-public live-config
  (rde-config
   (integrate-he-in-os? #t)
   (features
    (append
     %live-features
     %guest-features))))

(define-public live-os
  (rde-config-operating-system live-config))


;;; Dispatcher, which helps to return various values based on environment
;;; variable value.

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      ("live-system" live-os)
      (_ ixy-he))))

;; (pretty-print-rde-config ixy-config)
;; (use-modules (gnu services)
;;           (gnu services base))
;; (display
;;  (filter (lambda (x)
;;         (eq? (service-kind x) console-font-service-type))
;;       (rde-config-system-services ixy-config)))

;; (use-modules (rde features))
;; ((@ (ice-9 pretty-print) pretty-print)
;;  (map feature-name (rde-config-features ixy-config)))

;; ((@ (ice-9 pretty-print) pretty-print)
;;  (rde-config-home-services ixy-config))

;; (define br ((@ (rde tests store) build-with-store) ixy-he))
(dispatcher)


;;; TODO: Call reconfigure from scheme file.
;;; TODO: Rename configs.scm to main.scm?
;;; TODO: Move all files from abcdw/ to rde-config/ ?
