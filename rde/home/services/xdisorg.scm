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

(define-module (rde home services xdisorg)
  #:use-module (rde serializers css)
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
  (serialize-css config))

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
   "The deepest element should be a list of pairs, which represents properties,
intermediate items are path elements, it can be either a section name or a
list containing a few section names, to represented nested sections use dot
notation @code{section.subsection}, or @code{(section subsection)}.

For more details on serialization see documentation for @code{serialize-css}
or @code{(rde serializers css)} module.

See @code{man 5 rofi-theme} for more infromation about format and
@code{man 1 rofi} for options.

@lisp
'((configuration
   ((timeout ((action . \"kb-cancel\")
              (delay . 0)))
    (#(a.g b) ((c . d)
               (e . f))))))
@end lisp

would yeild:

@example
configuration {
    timeout {
        action: \"kb-cancel\";
        delay: 0;
    }
    a.g,
    b {
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
