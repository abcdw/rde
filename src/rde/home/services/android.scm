;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
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

(define-module (rde home services android)
  #:use-module (gnu home services)
  #:use-module (gnu packages android)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (rde serializers json)
  #:export (home-fdroidcl-configuration
            home-fdroidcl-service-type))

(define-configuration/no-serialization home-fdroidcl-configuration
  (fdroidcl
   (file-like fdroidcl)
   "The @code{fdroidcl} package to use.")
  (config
   (json-config '())
   "@code{fdroidcl} configuration.  You can provide an array of custom F-Droid
repositories like this:

@lisp
'((repos . #(((id . \"fdroid\")
              (url . \"https://f-droid.org/repo\"))
             ((id . \"fdroid-archive\")
              (url . \"https://f-droid.org/archive\")
              (enabled . #f)))))
@end lisp"))

(define (add-fdroidcl-configuration-file config)
  `(("fdroidcl/config.json"
     ,(apply mixed-text-file "config.json"
             (serialize-json-config
              (home-fdroidcl-configuration-config config))))))

(define (add-fdroidcl-package config)
  (list (home-fdroidcl-configuration-fdroidcl config)))

(define home-fdroidcl-service-type
  (service-type
   (name 'fdroidcl)
   (extensions
    (list (service-extension home-profile-service-type
                             add-fdroidcl-package)
          (service-extension home-xdg-configuration-files-service-type
                             add-fdroidcl-configuration-file)))
   (default-value (home-fdroidcl-configuration))
   (description "Configure the @code{fdroidcl} F-Droid desktop client to
invoke F-Droid operations on connected Android devices via @code{adb}.")))
