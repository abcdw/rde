;;; rde --- Reproducible development environment
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

(define-module (rde home services xdisorg)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services configuration)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-rofi-service-type
	    home-rofi-configuration))

(define rasi-config? list?)

(define (serialize-rasi-config config)
    (define (aligner nestness)
    (apply string-append
	   (map (const "    ") (iota nestness))))

  (define (serialize-rasi-term term)
    (match term
      (#t "true")
      (#f "false")
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? string? e) (format #f "~s" e))
      ((lst ...)
       (raise (formatted-message
               (G_ "Rasi term should be a non-list value (string, \
boolean, number, symbol, or gexp). Provided term is:\n ~a") lst)))
      (e e)))

  (define* (serialize-rasi-pair
	    expr #:optional (nestness 0))
    (match expr
      ((k)
       (list (serialize-rasi-term k) ": " (serialize-rasi-term #t) ";\n"))
      ((k . v)
       (list (serialize-rasi-term k) ": " (serialize-rasi-term v) ";\n"))
      (e
       (raise (formatted-message
               (G_ "Rasi pair should be a pair of terms, but provided \
expression is:\n ~a") e)))))

  (define* (serialize-rasi-properties
	    properties #:optional (nestness 0))
    (match properties
      ;; properties:
      ;; ((k1 . v1) (k2 . v2) (k3 . v3))
      (((k . v) ...)
       (append-map
	(lambda (e)
	  (append (list (aligner nestness))
	          (serialize-rasi-pair e nestness)))
	properties))
      (e
       (raise (formatted-message
	       (G_ "Rasi properties should be a list of pairs, \
but provided value is:\n ~a") e)))))

  (define* (serialize-rasi-path-element p)
    (if (list? p)
        (append
         (interpose (map serialize-rasi-term p) ", ")
         (list " "))
        (list (serialize-rasi-term p) " ")))

  (define* (serialize-rasi-expression
	    expr #:optional (nestness 0))

    (match expr
      ((p ((nested-p (e ...)) ...))
       (append
        (list (aligner nestness))
        (serialize-rasi-path-element p)
        (list "{\n")
        (serialize-rasi-subconfig (cadr expr) (1+ nestness))
        (list (aligner nestness))
        (list "}\n")))
      ((p ((k . v) ...))
       (append
        (list (aligner nestness))
        (serialize-rasi-path-element p)
        (list "{\n")
        (serialize-rasi-properties (cadr expr) (1+ nestness))
        (list (aligner nestness))
        (list "}\n")))
      (e
       (raise (formatted-message
               (G_ "Rasi expression should be a list of containing \
path element as a first item and list of properties or subconfig as a \
second is:\n ~a")
               e)))))

  (define* (serialize-rasi-subconfig
	    subconfig #:optional (nestness 0))
     (unless (list? subconfig)
       (raise (formatted-message
	       (G_ "Rasi (sub)config should be a gexp or a list of expressions, \
but provided value is:\n ~a") subconfig)))
     (append-map
      (lambda (expr)
        (match expr
          ((? gexp? e) (list e))
          (e (serialize-rasi-expression e nestness))))
      subconfig))

  (serialize-rasi-subconfig config))

;; (display
;;  (apply string-append
;;         (serialize-rasi-config
;;          '((configuration
;;             ((timeout ((action . "kb-cancel")
;;                        (delay . 0)))
;;              ((a.g b) ((c . d)
;;                        (e . f)))))))))

(define (rasi-config-file name config)
  (apply
   mixed-text-file
   name
   (serialize-rasi-config config)))

(define-configuration home-rofi-configuration
  (rofi
    (package rofi)
    "rofi package to use.")
  (config-rasi
   (rasi-config
    '())
   "The deepest element should be a list of pairs, which represents
properties, intermediate items are path elements, it can be either a
section name or a list containing a few section names, to represented
nested sections use dot notation @code{section.subsection}, whitespace
notation isn't supported.

See @code{man 5 rofi-theme} for more infromation about format and
@code{man 1 rofi} for options.

@lisp
'((configuration
   ((timeout ((action . \"kb-cancel\")
              (delay . 0)))
    ((a.g b) ((c . d)
              (e . f))))))
@end lisp

would yeild:

@example
configuration {
    timeout {
        action: \"kb-cancel\";
        delay: 0;
    }
    a.g, b {
        c: d;
        e: f;
    }
}
@end example"))

(define (add-rofi-packages config)
  (list (home-rofi-configuration-rofi config)))

(define (add-rofi-configuration config)
  `(("config/rofi/config.rasi"
     ,(rasi-config-file "rofi-config"
                        (home-rofi-configuration-config-rasi config)))))

(define (home-rofi-extensions cfg extensions)
  (home-rofi-configuration
   (inherit cfg)
   (config-rasi
    (append (home-rofi-configuration-config-rasi cfg)
            (append-map identity (reverse extensions))))))

(define home-rofi-service-type
  (service-type (name 'home-rofi)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-rofi-packages)
		       (service-extension
                        home-files-service-type
                        add-rofi-configuration)))
		(compose identity)
		(extend home-rofi-extensions)
                (default-value (home-rofi-configuration))
                (description "\
Install and configure rofi, application launcher and window switcher.")))
