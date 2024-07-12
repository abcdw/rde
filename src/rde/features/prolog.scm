;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024 jgart <jgart@dismail.de>
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

(define-module (rde features prolog)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages prolog)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:export (feature-prolog))

(define* (feature-prolog
          #:key
          (prolog trealla)
          (emacs-ediprolog emacs-ediprolog))
  "Configure Prolog for Emacs."
  (ensure-pred file-like? prolog)
  (ensure-pred file-like? emacs-ediprolog)

  (define f-name 'prolog)

  (define (get-home-services config)
    (list
     (simple-service
      'add-prolog-home-package
      home-profile-service-type
      (list trealla))
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        f-name
        config
        `((with-eval-after-load 'ediprolog
            (setq ediprolog-program ,(file-append trealla "/bin/tpl"))))
        #:elisp-packages (list emacs-ediprolog)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
