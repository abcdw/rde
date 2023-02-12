;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (contrib features age)
  #:use-module (rde features)
  #:use-module (gnu packages golang)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu home services)
  #:use-module (guix gexp)

  #:export (feature-age))

(define* (feature-age
          #:key
          (age age)
          (age-ssh-key "~/.ssh/id_ed25519"))
  "This feature sets up age for encryption tasks. It uses the SSH key
given in the location age-ssh-key to encrypt and decrypt files in the
password-store."
  (ensure-pred file-like? age)
  (ensure-pred string? age-ssh-key)

  (define (get-home-services config)
    (list
     (simple-service
      'age-add-age-package
      home-profile-service-type
      (list age))))

  (feature
   (name 'age)
   (values `((age . ,age)
             (age-ssh-key . ,age-ssh-key)))
   (home-services-getter get-home-services)))
