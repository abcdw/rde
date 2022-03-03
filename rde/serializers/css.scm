;;; rde --- Reproducible development environment.
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

(define-module (rde serializers css)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)

  #:use-module (gnu services configuration)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:export (serialize-css
            print-css

            serialize-css-config
            css-config?))

(define (css-config? config)
  (list? config))

(define (serialize-css-config config)
  "An implementation of css serialization, which produces a list of tokens, that
can be later concatenated to get a string represnting resulting css.

@lisp
`(((.inner > .backtotop)
   ((margin-right . 3%)))

  ((.backtotop a)
   ((padding . (.2em .6em .5em))
    (margin . (0 1em))))

  (#{@media (max-width: 30em)}#
   ((#(.note .summary)
     ((max-width . 92%)))
    ((#{#content}# .note)
     ((margin-left . auto)
      (margin-right . auto)))))

  ((#{#fssbox}#
    #{input[type=\"text\"]:focus}# ~ #{input[type=\"submit\"]}#)
   ((color . #{#005f5f}#)
    (background . #{#dff}#)))

  (#(#{#outdated}# (#{#outdated}# #{a[href]}#))
   ((font-family . #(\"Noto Sans Display\" \"Noto Sans\"
                     \"Liberation Sans\" sans-serif))))

  (#(.rounded-corners (.button a) (.backtotop a) (#{#fssbox}# input))
   ((border-radius . .4em)
    (-moz-border-radius . .4em)
    (-khtml-border-radius . .4em)
    (-webkit-border-radius . .4em)
    (-opera-border-radius . .4em))))
@end lisp

would yeild:

@example
.inner > .backtotop {
    margin-right: 3%;
}

.backtotop a {
    padding: .2em .6em .5em;
    margin: 0 1em;
}

@media (max-width: 30em) {
    .note,
    .summary {
        max-width: 92%;
    }
    #content .note {
        margin-left: auto;
        margin-right: auto;
    }
}

#fssbox input[type=\"text\"]:focus ~ input[type=\"submit\"] {
    color: #005f5f;
    background: #dff;
}

#outdated,
#outdated a[href] {
    font-family: \"Noto Sans Display\", \"Noto Sans\", \"Liberation Sans\", sans-serif;
}

.rounded-corners,
.button a,
.backtotop a,
#fssbox input {
    border-radius: .4em;
    -moz-border-radius: .4em;
    -khtml-border-radius: .4em;
    -webkit-border-radius: .4em;
    -opera-border-radius: .4em;
}

@end example
"
  (define (aligner nestness)
    (apply string-append
	   (map (const "    ") (iota nestness))))

  (define (serialize-css-term term)
    (match term
      (#t "true")  ; Probably not really needed for CSS,
      (#f "false") ; but useful for rofi's rasi dialect.
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? string? e) (format #f "~s" e))
      ((lst ...)
       (raise (formatted-message
               (G_ "CSS term should be a non-list value (string, \
boolean, number, symbol, or gexp). Provided term is:\n ~a") lst)))
      (e e)))

  (define* (serialize-css-pair
	    expr #:optional (nestness 0))
    (match expr
      ((k . #(e ...))
       (append
        (list (serialize-css-term k) ": ")
        (interpose (map serialize-css-term e) ", ")
        (list ";\n")))
      ((k)
       (list (serialize-css-term k) ": " (serialize-css-term #t) ";\n"))
      ((k . (e ...))
       (append
        (list (serialize-css-term k) ": ")
        (interpose (map serialize-css-term e) " ")
        (list ";\n")))
      ((k . v)
       (list (serialize-css-term k) ": " (serialize-css-term v) ";\n"))
      (e
       (raise (formatted-message
               (G_ "CSS pair should be a pair of terms, but provided \
expression is:\n ~a") e)))))

  (define* (serialize-css-properties
	    properties #:optional (nestness 0))
    (match properties
      ;; properties:
      ;; ((k1 . v1) (k2 . v2) (k3 . v3))
      (((k . v) ...)
       (append-map
	(lambda (e)
	  (append (list (aligner nestness))
	          (serialize-css-pair e nestness)))
	properties))
      (e
       (raise (formatted-message
	       (G_ "CSS properties should be a list of pairs, \
but provided value is:\n ~a") e)))))

  (define* (serialize-css-simple-selector p #:optional (nestness 0))
    (if (list? p)
        (interpose (map serialize-css-term p) " ")
        (list (serialize-css-term p))))

  (define* (serialize-css-path-element p #:optional (nestness 0))
    (cond
     ((vector? p)
      (append
       (vector-fold
        (lambda (i acc x)
          (append acc
                  (serialize-css-simple-selector x)
                  (if (= i (1- (vector-length p)))
                      '()
                      (list (string-append ",\n" (aligner nestness))))))
        '() p)
       (list " ")))
     (else (append (serialize-css-simple-selector p) (list " ")))))

  (define* (serialize-css-expression expr #:optional (nestness 0))
    (match expr
      ((p ((nested-p (e ...)) ...))
       (append
        (list (aligner nestness))
        (serialize-css-path-element p nestness)
        (list "{\n")
        (serialize-css-subconfig (cadr expr) (1+ nestness))
        (list (aligner nestness))
        (list "}\n")))
      ((p ((k . v) ...))
       (append
        (list (aligner nestness))
        (serialize-css-path-element p nestness)
        (list "{\n")
        (serialize-css-properties (cadr expr) (1+ nestness))
        (list (aligner nestness))
        (list "}\n")))
      (e
       (raise (formatted-message
               (G_ "CSS expression should be a list of containing \
path element as a first item and list of properties or subconfig as a \
second is:\n ~a")
               e)))))

  (define* (serialize-css-subconfig
	    subconfig #:optional (nestness 0))
     (unless (list? subconfig)
       (raise (formatted-message
	       (G_ "CSS (sub)config should be a gexp or a list of expressions, \
but provided value is:\n ~a") subconfig)))
     (append-map
      (lambda (expr)
        (append
         (match expr
           ((? gexp? e) (list e))
           (e (serialize-css-expression e nestness)))
         (if (= 0 nestness) (list "\n") '())))
      subconfig))

  (serialize-css-subconfig config))

(define sample-styles
  ;; Extracted from <https://www.gnu.org/layout.css>
  `(((.inner > .backtotop)
     ((margin-right . 3%)))

    ((.backtotop a)
     ((padding . (.2em .6em .5em))
      (margin . (0 1em))))

    (#{@media (max-width: 30em)}#
     ((#(.note .summary)
       ((max-width . 92%)))
      ((#{#content}# .note)
       ((margin-left . auto)
        (margin-right . auto)))))

    ((#{#fssbox}#
      #{input[type="text"]:focus}# ~ #{input[type="submit"]}#)
     ((color . #{#005f5f}#)
      (background . #{#dff}#)))

    (#(#{#outdated}# (#{#outdated}# #{a[href]}#))
     ((font-family . #("Noto Sans Display" "Noto Sans"
                       "Liberation Sans" sans-serif))))

    (#(.rounded-corners (.button a) (.backtotop a) (#{#fssbox}# input))
     ((border-radius . .4em)
      (-moz-border-radius . .4em)
      (-khtml-border-radius . .4em)
      (-webkit-border-radius . .4em)
      (-opera-border-radius . .4em)))))

;; (print-css sample-styles)

(define serialize-css serialize-css-config)

(define* (print-css css)
  "Prints generated json, useful for debugging."
  (display (apply string-append (serialize-css-config css))))
