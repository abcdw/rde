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

(define-module (rde features image-viewers)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (guix gexp)
  #:export (feature-imv))

(define* (feature-imv
          #:key
          (package imv))
  "Setup and configure the imv command-line image viewer."
  (ensure-pred file-like? imv)

  (define f-name 'imv)

  (define (get-home-services config)
    (append
     (list
      (simple-service
       'imv-add-package
       home-profile-service-type
       (list imv))
      (simple-service
       'add-imv-mime-entries
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration
        (default '((image/jpeg . imv.desktop)
                   (image/png . imv.desktop)
                   (image/svg+xml . imv.desktop))))))))

    (feature
     (name f-name)
     (values (make-feature-values imv))
     (home-services-getter get-home-services)))
