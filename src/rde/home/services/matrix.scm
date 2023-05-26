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

(define-module (rde home services matrix)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages matrix)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (rde serializers ini)
  #:export (home-pantalaimon-configuration
            home-pantalaimon-service-type))


;;;
;;; pantalaimon.
;;;

(define-configuration/no-serialization home-pantalaimon-configuration
  (pantalaimon
   (file-like pantalaimon)
   "The @code{pantalaimon} package to use.")
  (config
   (ini-config '())
   "Alist of pairs to define the daemon configuration.
See @url{/man/pantalaimon.5,,Pantalaimon} for the list of available
options."))

(define (home-pantalaimon-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run the @code{pantalaimon} proxy daemon after a session
bus has been activated.")
    (provision '(pantalaimon))
    (requirement '(dbus))
    (start #~(make-forkexec-constructor
              (list #$(file-append
                       (home-pantalaimon-configuration-pantalaimon config)
                       "/bin/pantalaimon"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/pantalaimon.log")))
    (stop #~(make-kill-destructor)))))

(define (home-pantalaimon-files-service config)
  (list
   `("pantalaimon/pantalaimon.conf"
     ,(apply mixed-text-file
             "pantalaimon.conf"
             (serialize-ini-config
              (home-pantalaimon-configuration-config config))))))

(define (home-pantalaimon-profile-service config)
  (list (home-pantalaimon-configuration-pantalaimon config)))

(define home-pantalaimon-service-type
  (service-type
   (name 'pantalaimon)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             home-pantalaimon-files-service)
          (service-extension home-profile-service-type
                             home-pantalaimon-profile-service)
          (service-extension home-shepherd-service-type
                             home-pantalaimon-shepherd-service)))
   (default-value (home-pantalaimon-configuration))
   (description "Set up the Pantalaimon proxy daemon to add E2EE
(end-to-end-encryption) support for non-E2EE Matrix clients.")))

(define (generate-home-pantalaimon-documentation)
  (generate-documentation
   `((home-pantalaimon-configuration
      ,home-pantalaimon-configuration-fields))
   'home-pantalaimon-configuration))
