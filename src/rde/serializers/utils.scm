;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (rde serializers utils)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)

  #:export (alist?

            path?
            serialize-path

            string-or-gexp?
            serialize-string-or-gexp

            gexp-text-config?
            serialize-gexp-text-config)
  #:re-export (interpose))

(define (alist? lst)
  (every pair? lst))


(define path? string?)
(define (serialize-path field-name val) val)

(define (string-or-gexp? sg) (or (string? sg) (gexp? sg)))
(define (serialize-string-or-gexp field-name val) "")

;; Guix proper has a different version of text-config.
(define (gexp-text-config? config)
  (and (list? config) (every string-or-gexp? config)))
(define (serialize-gexp-text-config field-name val)
  #~(string-append #$@(interpose val "\n" 'suffix)))
