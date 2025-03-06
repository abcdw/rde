;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022, 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde predicates)
  #:use-module (rde features)
  #:use-module (rde serializers ini)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)

  #:use-module (srfi srfi-1)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix inferior)
  #:use-module (guix gexp)

  #:re-export (file-like?
               ini-config?
               list-of
               list-of-strings?
               list-of-symbols?
               package?))

;; Predicates
(define-public (keyboard-shortcut? x) (string? x))
(define-public (path? x) (string? x))
(define-public (url? x) (string? x))

(define-public (file-like-or-path? x)
  (or (file-like? x) (path? x)))

(define-public (brightness? x)
  (and (integer? x) (<= 0 x 100)))

(define-public %number-of-ttys 6)
(define-public (tty-number? x)
  (and (integer? x) (<= 1 x %number-of-ttys)))

(define-public (string-or-gexp? x)
  (or (string? x) (gexp? x)))

;; Maybes
(define-public (maybe pred?)
  (lambda (x)
    (or (pred? x) (not x))))

(define-public maybe-file-like? (maybe file-like?))
(define-public maybe-integer?   (maybe integer?))
(define-public maybe-list?      (maybe list?))
(define-public maybe-path?      (maybe path?))
(define-public maybe-procedure? (maybe procedure?))
(define-public maybe-string?    (maybe string?))
(define-public maybe-symbol?    (maybe symbol?))
(define-public maybe-url?       (maybe url?))

;; Lists
(define-public list-of-file-likes?      (list-of file-like?))
(define-public list-of-services?        (list-of service?))
(define-public list-of-string-or-gexps? (list-of string-or-gexp?))
(define-public list-of-file-systems?    (list-of file-system?))
(define-public list-of-mapped-devices?  (list-of mapped-device?))
(define-public list-of-swap-devices?    (list-of swap-space?))
(define-public list-of-elisp-packages?  list-of-file-likes?)

;; Aliases
(define-deprecated/public-alias any-package?      file-like?)
(define-deprecated/public-alias list-of-packages? list-of-file-likes?)

(define-public (rde-procedure? x)
  "Checks if procedure have exactly one required argument, no optional and no
rest arguments."
  (and (procedure? x)
       (equal?
        (procedure-minimum-arity x)
        '(1 0 #f))))
