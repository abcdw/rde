;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 conses <contact@conses.eu>
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

(define-module (rde features gtk)
  #:use-module (rde features)
  #:use-module (rde features fontutils)
  #:use-module (rde features predicates)
  #:use-module (rde home services gtk)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-9)
  #:export (feature-gtk3
            theme
            make-theme
            theme?
            theme-name
            theme-package))

(define-record-type* <theme> theme
  make-theme theme?
  (name theme-name)
  (package theme-package))

(define (maybe-theme? x)
  (or (theme? x) (not x)))

(define* (feature-gtk3
          #:key
          (gtk-dark-theme? #f)
          (gtk-theme (make-theme
                      "Numix"
                      numix-gtk-theme))
          (icon-theme (make-theme
                       "Papirus-Dark"
                       papirus-icon-theme))
          (cursor-theme (make-theme
                         "Bibata-Modern-Classic"
                         bibata-cursor-theme))
          (extra-gtk-css #f)
          (extra-gtk-settings '()))
  "Configure the GTK3 toolkit.
You can tweak the provided GTK-THEME by overriding some of its styling via
EXTRA-GTK-CSS, a single argument procedure that returns a list of CSS rules
to be ingested by @code{serialize-css-config}."
  (ensure-pred boolean? gtk-dark-theme?)
  (ensure-pred maybe-theme? gtk-theme)
  (ensure-pred maybe-theme? icon-theme)
  (ensure-pred maybe-theme? cursor-theme)
  (ensure-pred maybe-procedure? extra-gtk-css)
  (ensure-pred list? extra-gtk-settings)

  (define (get-home-services config)
    "Return home services related to GTK."
    (require-value 'fonts config)

    (list
     (simple-service
      'add-gtk-packages
      home-profile-service-type
      (append
       (list gsettings-desktop-schemas)
       (if gtk-theme
           (list (theme-package gtk-theme))
           '())
       (if icon-theme
           (list (theme-package icon-theme))
           '())
       (if cursor-theme
           (list (theme-package cursor-theme))
           '())))
     (service
      home-gtk3-service-type
      (home-gtk3-configuration
       (default-cursor (and=> cursor-theme theme-name))
       (settings-ini
        `((Settings
           (,@(if gtk-theme
                  `(,(cons 'gtk-theme-name
                           #~(format #f "~a" #$(theme-name gtk-theme))))
                  '())
            ,@(if icon-theme
                  `(,(cons 'gtk-icon-theme-name
                           #~(format #f "~a" #$(theme-name icon-theme))))
                  '())
            ,@(if cursor-theme
                  `(,(cons 'gtk-cursor-theme-name
                           #~(format #f "~a" #$(theme-name cursor-theme))))
                  '())
            (gtk-font-name . ,#~(format #f "~a"
                                        #$(font-specification
                                           (get-value 'font-monospace config))))
            (gtk-application-prefer-dark-theme . ,gtk-dark-theme?)
            ,@extra-gtk-settings))))
       (gtk-css (if extra-gtk-css
                    (extra-gtk-css config)
                    '()))))))

  (feature
   (name 'gtk)
   (values `((gtk . #t)))
   (home-services-getter get-home-services)))
