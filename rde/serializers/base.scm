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

(define-module (rde serializers base)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)

  #:use-module (rde serializers utils)

  #:use-module (guix gexp)

  #:export (serialize-sexp-config
            sexp-serialize
            sexp-config?))

(define sexp-config? list?)
(define (sexp-serialize sexps)
  (define (serialize-list-element elem)
    (cond
     ((gexp? elem)
      elem)
     (else
      #~(string-trim-right
           (with-output-to-string
             (lambda ()
               ((@ (ice-9 pretty-print) pretty-print)
                '#$elem
                #:max-expr-width 79)))
           #\newline))))

  #~(string-append
     #$@(interpose
         (map serialize-list-element sexps)
         "\n" 'suffix)))

(define (serialize-sexp-config field-name sexps)
  (sexp-serialize sexps))
