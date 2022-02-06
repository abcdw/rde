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

(define-module (rde system services accounts)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)

  #:use-module (srfi srfi-1)

  #:export (rde-account-service-type))

(define (add-rde-account acc)
  (list acc))

(define (append-supplementary-groups acc groups)
  (user-account
   (inherit acc)
   (supplementary-groups
    (append (user-account-supplementary-groups acc) groups))))

(define rde-account-service-type
  (service-type (name 'rde-account)
                (extensions
                 (list (service-extension
                        account-service-type
			add-rde-account)))
		(compose concatenate)
		(extend append-supplementary-groups)
                (default-value #f)
                (description "\
Setups rde user, can be extended with a list of supplementary groups.")))
