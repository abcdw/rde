;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (rde serializers nginx)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rde serializers utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (nginx-serialize
            nginx-merge
            nginx-config?

            serialize-nginx-term
            serialize-nginx-vector
            serialize-nginx-context))

(define (nginx-term? t)
  (fold (lambda (x acc) (or acc (x t)))
        #f
        (list symbol? number? string? file-like? gexp?)))

(define (aligner nestness)
  (apply string-append
         ;; 4 spaces is too much for highly nested contexts.
         (map (const "  ") (iota nestness))))

(define (serialize-nginx-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? string? e) (format #f "~s" e))
      ((or
        (? file-like? e)
        (? gexp? e))
       e)
      (e (raise (formatted-message
               (G_ "\
Nginx term should be string, number, symbol, or gexp. Provided term is:\n ~a")
               e)))))

(define (nginx-vector? v)
  (and (vector? v)
       (vector-fold
        (lambda (i acc x) (and acc (nginx-term? x)))
        #t v)))

(define (serialize-nginx-vector v)
  (append
   (list "(")
   (interpose
    (reverse
     (vector-fold (lambda (i acc x)
                    (cons (serialize-nginx-term x) acc))
                  '()
                  v))
    " ")
   (list ")")))

(define (serialize-nginx-element t)
  (if (nginx-term? t)
      (list (serialize-nginx-term t))
      (serialize-nginx-vector t)))

(define (context-item? x)
  (or (list? x)
      (gexp? x)))

(define (nginx-context? x)
  (and (list? x)
       (every context-item? x)))

(define* (serialize-nginx-expression
          expr #:optional (nestness 0))
  (match expr
    ;; context's top-level gexp
    ((? gexp? e)
     (list e "\n"))

    ;; (element context)
    ((element (? nginx-context? context))
     (append
      (serialize-nginx-element element)
      (list " {\n")
      (serialize-nginx-context context (1+ nestness))
      `(,(aligner nestness)
        "}\n")))

    ;; subexpression:

    ;; (element . rest)
    ((element rest ..1)
     (append
      (serialize-nginx-element element) (list " ")
      (serialize-nginx-expression rest nestness)))

    ;; last element of subexpression
    ((element)
     `(,@(serialize-nginx-element element) ";" "\n"))

    (e
     (raise (formatted-message
             (G_ "Nginx expression should be a list of terms \
optionally ending with context, but provided expression is:\n ~a")
             e)))))

(define* (serialize-nginx-context
          context #:optional (nestness 0))
  (match context
    ;; config:
    ;; ((expr1) (expr2) (expr3))
    ((? nginx-context? expressions)
     (append-map
      (lambda (e)
        (append (list (if (gexp? e)
                          (aligner 0)
                          (aligner nestness)))
                (serialize-nginx-expression e nestness)))
      expressions))
    (e
     (raise (formatted-message
             (G_ "Nginx context should be a list of expressions, \
where each expression is also a list or gexp, but provided value is:\n ~a")
             e)))))

(define (serialize-nginx-config f c)
  #~(apply string-append
           (list #$@(serialize-nginx-context c))))

(define (nginx-serialize config)
  (serialize-nginx-config #f config))

(define (nginx-merge config1 config2 . rest)
  "Naive implementation, without actual merging logic."
  (apply append config1 config2 rest))

(define (nginx-config? config)
  "Naive implementation, without traversing nested structures."
  (nginx-context? config))
