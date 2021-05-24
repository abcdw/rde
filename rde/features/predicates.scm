(define-module (rde features predicates)
  #:use-module (rde features)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix gexp))

(define-public (maybe-string? x)
  (or (string? x) (not x)))

(define-public (path? x)
  (string? x))

(define-public (maybe-file-like? x)
  (or (file-like? x) (not x)))

(define-public (file-like-or-path? x)
  (or (file-like? x) (path? x)))

(define-public %number-of-ttys 6)
(define-public (tty-number? x)
  (and (integer? x) (<= 1 x %number-of-ttys)))

(define-public (list-of-packages? lst)
  (and (list? lst) (every package? lst)))

(define-public (list-of-services? lst)
  (and (list? lst) (every service? lst)))
