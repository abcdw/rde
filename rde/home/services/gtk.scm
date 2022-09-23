;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 conses <contact@conses.eu>
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

(define-module (rde home services gtk)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (guix gexp)

  #:use-module (rde serializers css)
  #:use-module (rde serializers ini)

  #:export (home-gtk3-configuration
            home-gtk3-service-type))


;;;
;;; gtk3
;;;

(define-configuration/no-serialization home-gtk3-configuration
  (settings
   (ini-config '())
   "Alist of pairs that set GTK global settings in @file{settings.ini}.
See @uref{https://docs.gtk.org/gtk3/class.Settings.html} for the
available settings.")
  (theme
   (css-config '())
   "List of CSS rules which set the GTK theme.
For more information on the serialization, consult the documentation
for @code{serialize-css} or the @code{(rde serializers css)} module."))

(define (home-gtk3-files-service config)
  (list
   `("gtk-3.0/settings.ini"
     ,(apply mixed-text-file
             "settings.ini"
             (serialize-ini-config
              (home-gtk3-configuration-settings config))))
   `("gtk-3.0/gtk.css"
     ,(apply mixed-text-file
             "gtk.css"
             (serialize-css-config
              (home-gtk3-configuration-theme config))))))

(define home-gtk3-service-type
  (service-type
   (name 'home-gtk3)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      home-gtk3-files-service)))
   (description "Configure GTK3 global settings and theme.")
   (default-value (home-gtk3-configuration))))

(define (generate-home-gtk3-documentation)
  (generate-documentation
   `((home-gtk3-configuration
      ,home-gtk3-configuration-fields))
   'home-gtk3-configuration))
