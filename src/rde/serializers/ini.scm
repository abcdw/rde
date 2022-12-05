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

(define-module (rde serializers ini)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)

  #:use-module (rde serializers utils)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:export (ini-serialize
            ini-print
            ini-merge
            ini-append

            serialize-ini-config
            ini-config?))

(define ini-config? list?)

(define sample-ini
  `((global ((daemon)
             (log . file)))
    (http ((host . 127.0.0.1)
           (port . 1234)))))

(define (format-ini-section section)
  (match section
    ('global "")
    (name (format #f "[~a]\n" (symbol->string name)))))

(define* (ini-serialize
          config
          #:key
          (equal-string " = ")
          (format-ini-section format-ini-section))
  "For global properties use global section, the properties will be added to the
beginning of the list before any section, this behavior can be adjusted with
FORMAT-INI-SECTION argument.

@lisp
`((global ((daemon)
           (log . file)))
  (http ((host . 127.0.0.1)
         (port . 1234))))
@end lisp

would yeld

@example
@end example
"
  (define (serialize-ini-term term)
    (match term
      (#t "true")
      (#f "false")
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? string? e) (object->string e))
      ((lst ...)
       (raise (formatted-message
               (G_ "INI term should be a non-list value (string, \
boolean, number, symbol, or gexp). Provided term is:\n ~a") lst)))
      (e e)))

  (define (serialize-ini-properties properties)
     (unless (alist? properties)
       (raise (formatted-message
	       (G_ "INI properties should be an alist, \
but provided value is:\n ~a") properties)))
     (append-map
      (match-lambda
        ((? gexp? e)
         (list e "\n"))
        ((k)
         (list (serialize-ini-term k) equal-string
               (serialize-ini-term #t) "\n"))
        ((k . v)
         (list (serialize-ini-term k) equal-string
               (serialize-ini-term v) "\n")))
      properties))

  (define (serialize-ini-section section)
    (match section
      ((name properties)
       (append
        (list (format-ini-section name))
        (serialize-ini-properties properties)))
      (e
       (raise (formatted-message
               (G_ "INI section should be a list containing a section name as \
the first element and alist of properties as the second, but provided value \
is:\n~a") e)))))

  ;; TODO: serialize global section before all other sections.
  (append-map
   (lambda (expr)
     (append
      (match expr
        ((? gexp? e) (list e))
        (e (serialize-ini-section e)))
      (list "\n")))
   config))

(define (ini-merge ini1 ini2)
  "Combine to INIs. Naive quadratic implementation, which can be rediculously
slow."
  (define keys-to-merge
    (fold
     (match-lambda*
       (((k v) acc) (if (assoc-ref ini2 k) (cons k acc) acc))
       (((? gexp? e) acc) acc))
     '() ini1))

  (define enriched-ini1
    (fold-right
     (match-lambda*
       (((k v) acc)
        (cons
         (cons k (list (append v (car (or (assoc-ref ini2 k) '(()))))))
         acc))
       (((? gexp? e) acc) (cons e acc)))
     '()
     ini1))

  (define stripped-ini2
    (remove (match-lambda
              ((k . v) (memq k keys-to-merge))
              (e #f))
            ini2))
  (append enriched-ini1 stripped-ini2))

(define (ini-append x acc)
  (ini-merge acc x))

(define serialize-ini-config ini-serialize)
;; (display
;;  (merge-ini
;;   '((section1 ((k1 . v1)))
;;     (section2 ((k4 . v4))))
;;   '((section1 ((k2 . v2)
;;                (k3 . v3)))
;;     (section3 ((k5 . v5))))))

;; (cdr '(a ((k . v) (k2 . v2))))

(define (ini-print ini)
  "Prints generated INI, useful for debugging."
  (display (apply string-append (ini-serialize ini))))
