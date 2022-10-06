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
  #:use-module (rde packages fonts)
  #:use-module (srfi srfi-9)

  #:export (feature-fonts

            font
            font-size
            font-name
            font-weight
            font-specification))

(define %rde-default-font-packages
  (list font-iosevka
        font-iosevka-aile
        font-iosevka-etoile
        font-liberation ;; Substitute for Arial, Times New Roman, Courier New
        font-noto-emoji
        font-gnu-unifont))

(define-record-type <font>
  (%font name size weight)
  font?
  (name font-name)
  (size font-size)
  (weight font-weight))

(define* (font name #:key size weight)
  (%font name size weight))

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
          (font-monospace (font "Iosevka"
                                #:size default-font-size #:weight 'regular))
          (font-sans      (font "Iosevka Aile"
                                #:size default-font-size #:weight 'regular))
          (font-serif     (font "Iosevka Etoile"
                                #:size default-font-size #:weight 'regular))
          (font-unicode   (font "Unifont"))
          (font-packages  '())
          (base-font-packages  %rde-default-font-packages))
  "Configure fonts.  DEFAULT-FONT-SIZE will be used for making
font-monospace default value, and it will be ignored if
#:font-monospace argument is specified."

  (ensure-pred integer? default-font-size)
  (ensure-pred font? font-monospace)
  (ensure-pred font? font-sans)
  (ensure-pred font? font-serif)
  (ensure-pred list-of-packages? font-packages)
  (ensure-pred list-of-packages? base-font-packages)

  ;; TODO: Make it configure fonts via fontconfig home service
  ;; (requires adding extending capabilities to service)
  (define (fonts-home-services config)
    "Returns home services related to fonts."
    (list
     (simple-service
      'font-packages
      home-profile-service-type
      (append
       font-packages
       base-font-packages))))

  (feature
   (name 'fonts)
   (values
    (append
     `((fonts . #t))
     (make-feature-values font-monospace font-sans font-serif font-unicode)))
   (home-services-getter fonts-home-services)))
