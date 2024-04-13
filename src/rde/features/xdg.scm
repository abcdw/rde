;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022, 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features xdg)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module ((gnu services configuration) #:select (maybe-value-set?))
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix ui)

  #:export (feature-xdg)

  #:re-export (home-xdg-base-directories-configuration
               home-xdg-user-directories-configuration))


(define* (feature-xdg
          #:key
          (xdg-base-directories-configuration
           (home-xdg-base-directories-configuration
            (state-home "$HOME/.local/var/lib")))
          (xdg-user-directories-configuration
           (home-xdg-user-directories-configuration)))
  "Set XDG base (with a few extensions) and user directories.

Set the value to \"$HOME\" for any user directory if you don't need
it.  No other environment variables allowed in user directories."
  (ensure-pred home-xdg-base-directories-configuration?
               xdg-base-directories-configuration)
  (ensure-pred home-xdg-user-directories-configuration?
               xdg-user-directories-configuration)
  (when (maybe-value-set?
         (home-xdg-base-directories-configuration-log-home
          xdg-base-directories-configuration))
    (raise
     (G_ "Use of home-xdg-base-directories-configuration-log-home is \
deprecated.  Please put you log files in the \"log\" subdirectory of \
home-xdg-base-directories-configuration-state-home.")))

  (define (xdg-home-services config)
    (list
     (simple-service
      'xdg-extra-packages
      home-profile-service-type
      (list xdg-utils xdg-user-dirs desktop-file-utils))
     ;; This service always present in essential services, that is why
     ;; we need to extend it to override configuration.
     (simple-service
      'xdg-base-directories-values
      home-xdg-base-directories-service-type
      xdg-base-directories-configuration)
     (service
      home-xdg-user-directories-service-type
      xdg-user-directories-configuration)))

  (feature
   (name 'xdg)
   (home-services-getter xdg-home-services)
   (values
    (make-feature-values xdg-base-directories-configuration
                         xdg-user-directories-configuration))))
