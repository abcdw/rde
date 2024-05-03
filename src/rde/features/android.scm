;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
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

(define-module (rde features android)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde home services android)
  #:use-module (rde serializers json)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages android)
  #:use-module (guix gexp)
  #:export (feature-android))

(define* (feature-android
          #:key
          (fdroidcl fdroidcl)
          (emacs-fdroid emacs-fdroid)
          (extra-fdroid-repositories '())
          (fdroid-key "f"))
  "Set up Android-related tooling."
  (ensure-pred file-like? fdroidcl)
  (ensure-pred file-like? emacs-fdroid)
  (ensure-pred json-config? extra-fdroid-repositories)
  (ensure-pred string? fdroid-key)

  (define f-name 'android)

  (define (get-home-services config)
    "Return home services related to Android tools."
    (list
     (simple-service
      'add-android-tools
      home-profile-service-type
      (list adb fastboot))
     (simple-service
      'add-android-envs
      home-environment-variables-service-type
      `(("ADB_VENDOR_KEYS" . "$XDG_CONFIG_HOME/android/adb")))
     (service home-fdroidcl-service-type
              (home-fdroidcl-configuration
               (fdroidcl fdroidcl)
               (config `((repos . #(((id . "fdroid")
                                     (url . "https://f-droid.org/repo"))
                                    ((id . "fdroid-archive")
                                     (url . "https://f-droid.org/archive"))
                                    ((id . "IzzyOnDroid")
                                     (url . "https://apt.izzysoft.de/fdroid/repo"))
                                    ,@extra-fdroid-repositories))))))
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,fdroid-key) 'fdroid-map))
        (with-eval-after-load 'fdroid
          (setq fdroid-log-events t)
          (setq fdroid-sans-device t)))
      #:elisp-packages (list emacs-fdroid))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
