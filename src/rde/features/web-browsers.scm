;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 conses <contact@conses.eu>
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

(define-module (rde features web-browsers)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages browser-extensions)
  #:use-module (gnu packages chromium)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (feature-ungoogled-chromium))

(define* (feature-ungoogled-chromium
          #:key
          (ungoogled-chromium ungoogled-chromium)
          (default-browser? #f)
          (startup-flags '()))
  "Configure the Chromium browser."
  (ensure-pred any-package? ungoogled-chromium)
  (ensure-pred boolean? default-browser?)
  (ensure-pred list? startup-flags)

  (define f-name 'ungoogled-chromium)

  (define (get-home-services config)
    "Return home services related to Ungoogled Chromium."
    (append
     (if default-browser?
         (list
          (simple-service
           'set-chromium-as-default-browser
           home-environment-variables-service-type
           `(("BROWSER" . ,(file-append ungoogled-chromium "/bin/chromium"))))
          (simple-service
           'chromium-xdg-defaults
           home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (default
             '((x-scheme-handler/http . chromium.desktop)
               (x-scheme-handler/https . chromium.desktop)
               (x-scheme-handler/about . chromium.desktop)
               (text/html . chromium.desktop))))))
         '())
     (if (get-value 'emacs config)
         (list
          (rde-elisp-configuration-service
           f-name
           config
           `((with-eval-after-load 'browse-url
               (setq browse-url-chromium-arguments ',startup-flags))
             ,@(if (get-value 'emacs-embark config)
                   `((with-eval-after-load 'embark
                       (define-key embark-url-map "c" 'browse-url-chromium)))
                   '()))))
         '())
     (list
      (simple-service
       'add-chromium-packages
       home-profile-service-type
       (list
        ungoogled-chromium
        ublock-origin/chromium))
      (simple-service
       'add-chromium-xdg-desktop-entry
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration
        (desktop-entries
         (list
          (xdg-desktop-entry
           (file "chromium")
           (name "Chromium")
           (type 'application)
           (config
            `((exec . ,#~(string-join
                          (list
                           #$(file-append ungoogled-chromium "/bin/chromium")
                           #$@startup-flags "%U")))
              (terminal . #f)
              (comment . "Access the Internet")))))))))))

  (feature
   (name f-name)
   (values `((,f-name . ,ungoogled-chromium)))
   (home-services-getter get-home-services)))
