;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
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

(define-module (rde home services keyboard)
  #:use-module (gnu system keyboard)
  #:use-module (gnu home services)

  #:export (home-keyboard-service-type))

;; We don't place it to xorg.scm, because it works for wayland

(define (set-xkb-variables layout)
  (if layout
      `(("XKB_DEFAULT_LAYOUT" . ,(keyboard-layout-name layout))
	("XKB_DEFAULT_VARIANT" . ,(keyboard-layout-variant layout))
	("XKB_DEFAULT_OPTIONS" . ,(string-join
				   (keyboard-layout-options layout) ","))
	("XKB_DEFAULT_MODEL" . ,(keyboard-layout-model layout)))
      '()))

(define home-keyboard-service-type
  (service-type (name 'home-keyboard)
                (extensions
                 (list (service-extension
			home-environment-variables-service-type
			set-xkb-variables)))
                (default-value #f)
                (description "Set layouts by configuring XKB_*
environment variables.  Service accepts an instance of
@code{keyboard-layout} from @code{(gnu system keyboard)}.")))
