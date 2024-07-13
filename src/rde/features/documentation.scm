;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
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

(define-module (rde features documentation)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:export (feature-manpages))

(define* (feature-manpages
          #:key
          (pager #f)
          (man-key "M")
          (woman-key "W"))
  "Configure tooling to read manual pages."
  (ensure-pred maybe-file-like? pager)
  (ensure-pred string? man-key)
  (ensure-pred string? woman-key)

  (define f-name 'manpages)

  (define (get-home-services config)
    "Return home services related to manpages."
    (append
     (list
      (simple-service
       'add-pager
       home-environment-variables-service-type
       `(("PAGER" . ,pager))))
     (if (get-value 'emacs config #f)
         `(,(rde-elisp-configuration-service
             f-name
             config
             `((with-eval-after-load 'rde-keymaps
                 (let ((map rde-app-map))
                   (define-key map (kbd ,woman-key) 'woman)
                   (define-key map (kbd ,man-key)
                     ',(if (get-value 'emacs-consult config #f)
                           'consult-man
                           'man))))
               (add-hook 'woman-mode-hook 'toggle-truncate-lines))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
