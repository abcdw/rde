;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde home wm)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu services configuration)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-sway-service-type
	    home-sway-configuration

            sway-config?))


;;;
;;; Sway.
;;;

(define sway-config? list?)
(define (serialize-sway-config field-name val)
  (define (aligner nestness)
    (apply string-append
	   (map (const "    ") (iota nestness))))

  (define (serialize-sway-term term)
    ;; (format #t "finval. ~a\n" term)
    (match term
      (#t "yes")
      (#f "no")
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? string? e) e)
      ((lst ...)
       (raise (formatted-message
               (G_ "Sway term should be a non-list value (string, \
boolean, number, symbol, or gexp). Provided term is:\n ~a") lst)))
      (e e)))

  (define* (serialize-sway-expression
	    expr #:optional (nestness 0))
    ;; (format #t "expres. ~a\n" expr)
    (match expr
      ;; subconfig has the same structure as config,
      ;; the only difference: it's not a top-level form
      ;; can be found at the end of expression.
      ;; (term subconfig)
      ((term ((expressions ...) ...))
       ;; (format #t "subtop. ~a . ~a\n" term expressions)
       (append
	(list (serialize-sway-term term) " {\n")
	(serialize-sway-subconfig expressions (1+ nestness))
	`(,(aligner nestness)
          "}\n")))

      ;; subexpression:
      ;; (term . rest)
      ((term rest ..1)
       ;; (format #t "inside. ~a . ~a\n" term rest)
       (cons* (serialize-sway-term term) " "
	      (serialize-sway-expression rest)))

      ;; last element of subexpression
      ((term)
       ;; (format #t "term.   ~a\n" term)
       (list (serialize-sway-term term) "\n"))

      (e
       (raise (formatted-message
               (G_ "Sway expression should be a list of terms \
optionally ending with subconfigs, but provided expression is:\n ~a")
               e)))))

  (define* (serialize-sway-subconfig
	    subconfig #:optional (nestness 0))
    (match subconfig
      ;; config:
      ;; ((expr1) (expr2) (expr3))
      (((expressions ...) ...)
       (append-map
	(lambda (e)
	  (append (list (aligner nestness))
	          (serialize-sway-expression e nestness)))
	expressions))
      (e
       (raise (formatted-message
	       (G_ "Sway (sub)config should be a list of expressions, \
where each expression is also a list, but provided value is:\n ~a") e))) ))

  #~(apply string-append
	   '#$(serialize-sway-subconfig val)))

(define-configuration home-sway-configuration
  (package
    (package sway)
    "Sway package to use.")
  (config
   ;;; TODO: make the default value thunked and use a sway package
   ;;; from package field. Requires patching define-configuration
   ;; `((include ,(file-append
   ;; 		 (home-sway-configuration-package this-home-sway-configuration)
   ;; 		 "/etc/sway/config")))
   (sway-config
    `((include ,(file-append sway "/etc/sway/config"))))
   "List of expressions.  Each @dfn{expression} is a list of terms,
optionally ending in a list of expressions.  A @dfn{term} is a non-list
value: string, boolean, number, symbol, or gexp.

There is no special syntax for CRITERIA (See @command{man 5 sway}),
because sway's doc states it's a string, so use a string for that.

For gradual migration, the file with old config can be included as
shown in the example below.  After that, you can start translate lines
to scheme one by one.

@example
`((include ,(local-file \"./sway/config\"))
  (bindsym $mod+Ctrl+Shift+a exec emacsclient -c --eval \"'(eshell)'\")
  (bindsym $mod+Ctrl+Shift+o \"[class=\\\"IceCat\\\"]\" kill)
  (input * ((xkb_layout us,ru)
            (xkb_variant dvorak,))))
@end example

would yield something like:

@example
include /gnu/store/408jwvh6wxxn1j85lj95fniih05gx5xj-config
bindsym $mod+Ctrl+Shift+a exec emacsclient -c --eval '(eshell)'
bindsym $mod+Ctrl+Shift+o [class=\"IceCat\"] kill
input * {
    xkb_layout us,ru
    xkb_variant dvorak,
}
@end example"))

(define (add-sway-packages config)
  (list (home-sway-configuration-package config)))

(define (add-sway-configuration config)
  `(("config/sway/config"
     ,(mixed-text-file
       "sway-config"
       (serialize-configuration config home-sway-configuration-fields)))))

(define (home-sway-extensions cfg extensions)
  (home-sway-configuration
   (inherit cfg)
   (config
    (append (home-sway-configuration-config cfg)
            (append-map identity (reverse extensions))))))

(define home-sway-service-type
  (service-type (name 'home-sway)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-sway-packages)
		       (service-extension
                        home-files-service-type
                        add-sway-configuration)))
		(compose identity)
		(extend home-sway-extensions)
                (default-value (home-sway-configuration))
                (description "\
Install and configure Sway, a Wayland compositor compatible with i3.")))
