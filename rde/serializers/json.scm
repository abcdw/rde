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

(define-module (rde serializers json)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (guix gexp)
  #:export (json-serialize
            json-print
            json-config?

            serialize-json-config))

(define (json-config? json)
  (or (vector? json) (list? json)))

(define (json-number? number)
  (and (number? number) (eqv? (imag-part number) 0) (finite? number)))

(define (json-key? scm)
  (or (symbol? scm) (string? scm)))

(define (num->string num)
  "Convert fraction to inexact representation if needed."
  (if (and (rational? num) (not (integer? num)))
      (number->string (exact->inexact num))
      (number->string num)))

(define (json-s-identation level pretty?)
  (if pretty? (list (format #f "~v_" (* 2 level))) '()))

(define (json-s-null)
  (list "null"))

(define (json-s-boolean v)
  (list (if v "true" "false")))

(define (json-s-number v)
  (list (num->string v)))

(define (json-s-string v)
  (list (format #f "~s" v)))

(define (json-s-file-like v)
  (list "\"" v "\""))

(define (json-s-symbol v)
  (json-s-string (symbol->string v)))

(define (json-s-key k)
  (cond
   ((symbol? k) (json-s-symbol k))
   ((string? k) (json-s-string k))
   (else (throw 'json-invalid-key k))))

(define (json-s-identity v)
  (list v))

(define (json-s-newline pretty?)
  (if pretty? (list "\n") '()))
(define (json-s-space pretty?)
  (if pretty? (list " ") '()))

(define (json-s-vector v level pretty?)
  (append
   (list "[")
   (json-s-newline pretty?)
   (vector-fold
    (lambda (i acc e)
      (append acc
              (if (> i 0)
                  (append (list ",") (json-s-newline pretty?))
                  '())
              (json-s-identation (1+ level) pretty?)
              (json-s-json e (1+ level) pretty?))) '() v)
   (json-s-newline pretty?)
   (json-s-identation level pretty?)
   (list "]")))

(define (json-s-pair v level pretty?)
  (append
   (json-s-identation level pretty?)
   (json-s-key (car v))
   (list ":")
   (json-s-space pretty?)
   (json-s-json (cdr v) level pretty?)))

(define (json-s-null-alist v)
  (list "{}"))

(define (json-s-alist v level pretty?)
  (append
   (list "{")
   (json-s-newline pretty?)
   (json-s-pair (car v) (1+ level) pretty?)
   (append-map
    (lambda (x)
      (append
       (append (list ",") (json-s-newline pretty?))
       (json-s-pair x (1+ level) pretty?)))
    (cdr v))
   (json-s-newline pretty?)
   (json-s-identation level pretty?)
   (list "}")))

(define (json-s-json json level pretty?)
  (define (pairs? lst)
    (and (list? lst) (every pair? lst)))
  (match json
    ('null (json-s-null))
    ((? boolean? v) (json-s-boolean v))
    ((? number? v) (json-s-number v))
    ((? string? v) (json-s-key v))
    ((? symbol? v) (json-s-key v))
    ((? gexp? v) (json-s-identity v))
    ((? file-like? v) (json-s-file-like v))
    ((? vector? v) (json-s-vector v level pretty?))
    ((? null? v) (json-s-null-alist v))
    ((? pairs? v) (json-s-alist v level pretty?))
    (e (throw 'json-invalid json))))

(define* (json-serialize json #:key (pretty? #t))
  "Works similar to guile-json, but returns a list of strings, which
have to be concatenated.  Additionally, supports gexps and file-likes.
vectors -> arrays, alists -> objects, etc."
  (json-s-json json 0 pretty?))

(define serialize-json-config json-serialize)

(define* (json-print json #:key (pretty? #t))
  "Prints generated json, useful for debugging."
  (display (apply string-append (json-s-json json 0 pretty?))))

;; (print-json #(some ((hi . h) ("a" . 4/7) (hi . 3))))

