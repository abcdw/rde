;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features keyboard)
  #:use-module (rde features)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (rde home services keyboard)

  #:export (feature-keyboard
            %dvorak-layout
            %dvorak-jcuken-layout)

  #:re-export (keyboard-layout))

;; Example of multi-layer layout: https://neo-layout.org/index_en.html

(define %dvorak-layout
  (keyboard-layout "us" "dvorak" #:options '("ctrl:nocaps")))

(define %dvorak-jcuken-layout
  (keyboard-layout
   "us,ru" "dvorak,"
   #:options '("grp:win_space_toggle" "ctrl:nocaps")))

;; TODO: Add ability to provide custom layout package or file

;; There is no default value to force user specify some keyboard
;; layout in case they use this feature

;; TODO: [Andrew Tropin, 2024-04-28] Add an ability to add custom layouts
;; https://todo.sr.ht/~abcdw/tickets/8
(define* (feature-keyboard #:key keyboard-layout)
  "Sets keyboard layout.  Affects bootloader, and XKB_* variables for
the user."
  (ensure-pred keyboard-layout? keyboard-layout)

  (define (keyboard-services values)
    "Returns home-keyboard service."
    (list
     (service home-keyboard-service-type keyboard-layout)))

  (feature
   (name 'keyboard)
   (values (make-feature-values keyboard-layout))
   (home-services-getter keyboard-services)))
