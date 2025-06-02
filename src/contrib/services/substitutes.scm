;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (contrib services substitutes)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (guix gexp)
  #:export (guix-north-america-substitutes-service))

(define guix-north-america-substitutes-service
  (simple-service
   'guix-north-america-substitutes
   guix-service-type
   (guix-extension
    (substitute-urls (list "https://cuirass.genenetwork.org"))
    (authorized-keys
     (list
      (plain-file "guix-north-america-signing-key.pub" "\
(public-key
  (ecc
    (curve Ed25519)
      (q \
#9578AD6CDB23BA51F9C4185D5D5A32A7EEB47ACDD55F1CCB8CEE4E0570FBF961#)))"))))))
