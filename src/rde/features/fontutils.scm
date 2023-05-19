;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
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

(define-module (rde features fontutils)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu services)
  #:use-module (gnu packages fonts)
  #:use-module (guix records)
  #:use-module (rde packages fonts)
  #:use-module (srfi srfi-9)

  #:export (feature-fonts

            font
            font-size
            font-name
            font-weight
            font-specification
            make-font
            font?))

(define-record-type* <font> font
  make-font
  font?
  (name font-name)
  (size font-size)
  (weight font-weight (default 'regular))
  (package font-package))

(define (font-specification font)
  "Convert <font> record to string."
  (string-join (list
                (font-name font)
                (string-capitalize (symbol->string (font-weight font)))
                (number->string (font-size font)))
               " "))

(define* (feature-fonts
          #:key
          (default-font-size 11)
          (font-monospace
           (font
            (name "Iosevka")
            (size default-font-size)
            (package font-iosevka)))
          (font-serif
           (font
            (name "Iosevka Aile")
            (size default-font-size)
            (package font-iosevka-aile)))
          (font-sans
           (font
            (name "Iosevka Etoile")
            (size default-font-size)
            (package font-iosevka-etoile)))
          (font-unicode
           (font
            (name "Unifont")
            (size default-font-size)
            (package font-gnu-unifont)))
          (extra-font-packages '()))
  "Configure fonts.  DEFAULT-FONT-SIZE will be used for making
font-monospace default value, and it will be ignored if
#:font-monospace argument is specified."

  (ensure-pred integer? default-font-size)
  (ensure-pred font? font-monospace)
  (ensure-pred font? font-serif)
  (ensure-pred font? font-sans)
  (ensure-pred font? font-unicode)
  (ensure-pred list-of-file-likes? extra-font-packages)

  (define f-name 'fonts)

  (define (get-home-services config)
    "Return home services related to fonts."
    (list
     (simple-service
      'add-extra-fonts
      home-profile-service-type
      (append
       (map font-package
            (list font-sans font-serif font-monospace font-unicode))
       extra-font-packages))
     (simple-service
      'add-fontconfig-font-families
      home-fontconfig-service-type
      (list
       `(alias
         (family "sans-serif")
         (prefer
          (family ,(font-name font-sans))))
       `(alias
         (family "serif")
         (prefer
          (family ,(font-name font-serif))))
       `(alias
         (family "monospace")
         (prefer
          (family ,(font-name font-monospace))))))))

  (feature
   (name f-name)
   (values
    (append
     `((,f-name . #t))
     (make-feature-values font-sans font-monospace
                          font-serif font-unicode)))
   (home-services-getter get-home-services)))
