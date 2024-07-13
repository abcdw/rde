;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features finance)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-ledger))


;;;
;;; ledger.
;;;

(define* (feature-ledger
          #:key
          (ledger ledger)
          (emacs-ledger-mode emacs-ledger-mode))
  "Setup and configure ledger related things."
  (ensure-pred file-like? ledger)
  (ensure-pred file-like? emacs-ledger-mode)

  (define (get-home-services config)
    (define emacs-f-name 'ledger)

    (list
     (simple-service
      'ledger-add-ledger-package
      home-profile-service-type
      (list ledger)) ;; Needed for info pages

     ;; TODO: Build emacs-ledger with ledger specified in feature argument.
     (when (get-value 'emacs config #f)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((with-eval-after-load
           'ledger-mode
           (setq ledger-default-date-format ledger-iso-date-format)))
        #:summary "\
Tweaks for ledger-mode"
        #:commentary "\
Use ISO date."
        #:keywords '(convenience)
        #:elisp-packages (list emacs-ledger-mode)))))

  (feature
   (name 'ledger)
   (values `((ledger . #t)
             (emacs-ledger-mode . emacs-ledger-mode)))
   (home-services-getter get-home-services)))
