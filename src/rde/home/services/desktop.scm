;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
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

(define-module (rde home services desktop)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (rde serializers yaml)
  #:export (home-udiskie-configuration
            home-udiskie-service-type))

(define-configuration/no-serialization home-udiskie-configuration
  (udiskie
   (package udiskie)
   "The udiskie package to use.")
  (config
   (yaml-config '())
   "Udiskie configuration."))

(define (home-udiskie-profile-service config)
  (list (home-udiskie-configuration-udiskie config)))

(define (home-udiskie-shepherd-service config)
  (list
   (shepherd-service
    (provision '(udiskie))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list
               #$(file-append
                  (home-udiskie-configuration-udiskie config)
                  "/bin/udiskie")))))))

(define (add-udiskie-configuration config)
  `(("udiskie/config.yml"
     ,(mixed-text-file "config.yml"
                       (yaml-serialize
                        (home-udiskie-configuration-config config))))))

(define home-udiskie-service-type
  (service-type
   (name 'udiskie)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-udiskie-profile-service)
     (service-extension
      home-shepherd-service-type
      home-udiskie-shepherd-service)
     (service-extension
      home-xdg-configuration-files-service-type
      add-udiskie-configuration)))
   (description "Set up a udiskie daemon to automount removable media.")
   (default-value (home-udiskie-configuration))))
