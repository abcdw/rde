;;; rde --- Reproducible development environment.
;;;
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

(define-module (rde serializers yaml)
  #:use-module (rde serializers utils)
  #:use-module (gnu home services utils)
  #:use-module (gnu services configuration)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (yaml-print
            yaml-config?
            yaml-serialize

            serialize-yaml-term
            serialize-yaml-element
            serialize-yaml-config))

(define yaml-config? list?)

(define (yaml-term? t)
  (fold (lambda (x acc) (or acc (x t)))
        #f
        (list boolean? symbol? number? string? file-like? gexp?)))

(define (serialize-yaml-string v)
  (format #f "~s" v))

(define (serialize-yaml-symbol v)
  (format #f "~a" v))

(define (serialize-yaml-term term)
  (match term
    ((? boolean? v) (if v "true" "false"))
    ((? number? v) (number->string v))
    ((? string? v) (serialize-yaml-string v))
    ((? symbol? v) (serialize-yaml-symbol v))
    ((or (? gexp? v)
         (? file-like? v))
     v)
    (v (raise (formatted-message
               (G_ "\
YAML term should be boolean, number, string, symbol, or gexp. Provided term
is:\n ~a") v)))))

(define (serialize-yaml-key k)
  (list
   (cond
    ((symbol? k) (serialize-yaml-symbol k))
    ((string? k) (serialize-yaml-string k))
    (else (raise (formatted-message
                  (G_ "\
YAML key should be symbol or string. Provided key is:\n ~a")
                  k))))))

(define (serialize-yaml-newline pretty?)
  (if pretty? (list "\n") '()))

(define (serialize-yaml-space pretty?)
  (if pretty? (list " ") '()))

(define (serialize-yaml-indentation level pretty?)
  (if pretty?
      (list (format #f "~v_" (- (* 2 level) 2)))
      '()))

(define (serialize-yaml-vector v level pretty?)
  (append
   (serialize-yaml-newline pretty?)
   (vector-fold
    (lambda (i acc e)
      (append acc
              (if (> i 0)
                  (serialize-yaml-newline pretty?)
                  '())
              (serialize-yaml-indentation (1+ level) pretty?)
              (list "- ")
              (match e
                ((? alist? e)
                 (serialize-yaml-vector-alist e (+ 1 level) pretty?))
                (_ (serialize-yaml-element e (1+ level) pretty?)))))
    '() v)))

(define (serialize-yaml-list v pretty?)
  (append
   (list "[")
   (interpose
    (append-map
     (lambda (x)
       (serialize-yaml-element x 0 pretty?))
     v)
    ", ")
   (list "]")))

(define (serialize-yaml-pair v level pretty?)
  (append
   (serialize-yaml-indentation level pretty?)
   (serialize-yaml-key (car v))
   (list ":")
   (serialize-yaml-space pretty?)
   (if (alist? (cdr v))
       (serialize-yaml-newline pretty?)
       (list ""))
   (serialize-yaml-element (cdr v) level pretty?)))

(define (serialize-yaml-alist v level pretty?)
  (append
   (serialize-yaml-pair (car v) (1+ level) pretty?)
   (append-map
    (lambda (x)
      (append
       (serialize-yaml-newline pretty?)
       (serialize-yaml-pair x (1+ level) pretty?)))
    (cdr v))))

(define (serialize-yaml-vector-alist v level pretty?)
  (append
   (serialize-yaml-pair (car v) (- level (- level 1)) pretty?)
   (append-map
    (lambda (x)
      (append
       (serialize-yaml-newline pretty?)
       (serialize-yaml-pair x (1+ level) pretty?)))
    (cdr v))))

(define (serialize-yaml-element yaml level pretty?)
  (append
   (match yaml
     (() (list ""))
     ((? yaml-term? v) (list (serialize-yaml-term v)))
     ((? alist? v) (serialize-yaml-alist v level pretty?))
     ((? list? v) (serialize-yaml-list v pretty?))
     ((? vector? v) (serialize-yaml-vector v level pretty?))
     (e (throw 'yaml-invalid yaml)))))

(define (serialize-yaml-config f c)
  #~(apply string-append
           (list #$@(serialize-yaml-element c 0 #t))))

(define* (yaml-serialize config)
  "Returns a list of YAML strings which have to be concatenated.  It supports
 gexps, file-likes, vectors -> arrays, alists -> dictionaries, etc."
  (serialize-yaml-config #f config))

(define* (yaml-print yaml #:key (pretty? #t))
  "Prints the generated YAML, useful for debugging purposes."
  (display (apply string-append
                  (serialize-yaml-element yaml 0 pretty?))))
