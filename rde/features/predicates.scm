(define-module (rde features predicates)
  #:use-module (rde features)
  #:use-module (rde serializers ini)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)

  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix inferior)
  #:use-module (guix gexp)

  #:re-export (package?
               ini-config?))

(define-public (maybe-string? x)
  (or (string? x) (not x)))

(define-public (maybe-url? x)
  (maybe-string? x))

(define-public (path? x)
  (string? x))

(define-public (maybe-path? x)
  (or (path? x) (not x)))

(define-public (maybe-file-like? x)
  (or (file-like? x) (not x)))

(define-public (file-like-or-path? x)
  (or (file-like? x) (path? x)))

(define-public %number-of-ttys 6)
(define-public (tty-number? x)
  (and (integer? x) (<= 1 x %number-of-ttys)))

(define-public (brightness? x)
  (and (integer? x) (<= 0 x 100)))

(define-public (maybe-list? lst)
  (or (list? lst) (not lst)))

(define-public (list-of-strings? lst)
  (and (list? lst) (every string? lst)))

(define-public (list-of-file-likes? lst)
  (and (list? lst) (every file-like? lst)))

(define-public (any-package? x)
  (or (package? x) (inferior-package? x)))

(define-public (list-of-packages? lst)
  (and (list? lst) (every any-package? lst)))

(define-public (list-of-elisp-packages? lst)
  (list-of-packages? lst))

(define-public (list-of-services? lst)
  (and (list? lst) (every service? lst)))

(define-public (string-or-gexp? x)
  (or (string? x) (gexp? x)))
(define-public (list-of-string-or-gexps? lst)
  (and (list? lst) (every string-or-gexp? lst)))


(define-public (list-of-file-systems? lst)
  (and (list? lst) (every file-system? lst)))
(define-public (list-of-mapped-devices? lst)
  (and (list? lst) (every mapped-device? lst)))
(define-public (list-of-swap-devices? lst)
  (and (list? lst) (every swap-space? lst)))
