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

(define-module (rde features bluetooth)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (guix gexp)
  #:export (feature-bluetooth))

(define* (feature-bluetooth
          #:key
          (bluez bluez)
          (emacs-bluetooth emacs-bluetooth)
          (auto-enable? #t)
          (bluetooth-key "B"))
  "Configure and set up Bluetooth."
  (ensure-pred file-like? bluez)
  (ensure-pred file-like? emacs-bluetooth)
  (ensure-pred boolean? auto-enable?)
  (ensure-pred string? bluetooth-key)

  (define f-name 'bluetooth)

  (define (get-home-services config)
    "Return home services related to Bluetooth."
    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,bluetooth-key) 'bluetooth-list-devices))
        (with-eval-after-load 'bluetooth
          (add-hook 'kill-emacs-hook 'bluetooth-toggle-powered)
          (define-key bluetooth-mode-map "C" 'bluetooth-connect-profile)))
      #:elisp-packages (list emacs-bluetooth))))

  (define (get-system-services config)
    "Return system services related to Bluetooth."
    (list
     (service bluetooth-service-type
              (bluetooth-configuration
               (bluez bluez)
               (auto-enable? auto-enable?)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
