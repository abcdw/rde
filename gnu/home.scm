;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
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

(define-module (gnu home)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services symlink-manager)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services xdg)
  #:use-module (gnu home-services fontutils)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:use-module (guix diagnostics)

  #:export (home-environment
            home-environment?
            this-home-environment

            home-environment-derivation
            home-environment-user-services
            home-environment-essential-services
            home-environment-services
            home-environment-location

            home-environment-with-provenance))

;;; Comment:
;;;
;;; This module provides a <home-environment> record for managing
;;; per-user packages and configuration files in the similar way as
;;; <operating-system> do for system packages and configuration files.
;;;
;;; Code:

(define-record-type* <home-environment> home-environment
  make-home-environment
  home-environment?
  this-home-environment

  (packages home-environment-packages             ; list of (PACKAGE OUTPUT...)
            (default '()))

  (essential-services home-environment-essential-services ; list of services
                      (thunked)
                      (default (home-environment-default-essential-services
                                this-home-environment)))
  (services home-environment-user-services
            (default '()))

  (location home-environment-location             ; <location>
            (default (and=> (current-source-location)
                            source-properties->location))
            (innate)))

(define (home-environment-default-essential-services he)
  "Return the list of essential services for home environment."
  (list
   (service home-run-on-first-login-service-type)
   (service home-activation-service-type)
   (service home-environment-variables-service-type)

   (service home-symlink-manager-service-type)

   (service home-fontconfig-service-type)
   (service home-xdg-base-directories-service-type)
   (service home-shell-profile-service-type)

   (service home-service-type)
   (service home-profile-service-type (home-environment-packages he))))

(define* (home-environment-services he)
  "Return all the services of home environment."
  (instantiate-missing-services
   (append (home-environment-user-services he)
           (home-environment-essential-services he))))

(define* (home-environment-derivation he)
  "Return a derivation that builds OS."
  (let* ((services         (home-environment-services he))
         (home (fold-services services
                              #:target-type home-service-type)))
    (service-value home)))

(define* (home-environment-with-provenance he config-file)
  "Return a variant of HE that stores its own provenance information,
including CONFIG-FILE, if available.  This is achieved by adding an instance
of HOME-PROVENANCE-SERVICE-TYPE to its services."
  (home-environment
    (inherit he)
    (services (cons (service home-provenance-service-type config-file)
                    (home-environment-user-services he)))))
