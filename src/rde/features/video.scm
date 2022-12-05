;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features video)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu packages video)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services video)
  #:use-module (guix gexp)
  #:export (feature-mpv))

(define* (feature-mpv
          #:key
          (mpv mpv)
          (extra-mpv-conf '()))
  "Setup and configure mpv."
  (ensure-pred file-like? mpv)
  (ensure-pred alist? extra-mpv-conf)

  (define (get-home-services config)
    (list
     (service
      home-mpv-service-type
      (home-mpv-configuration
       (package mpv)
       (default-options
         `((script . ,(file-append mpv-mpris "/lib/mpris.so"))
           (keep-open . #t)
           (save-position-on-quit . #t)
           ,@extra-mpv-conf))))))

  (feature
   (name 'mpv)
   (values (make-feature-values mpv))
   (home-services-getter get-home-services)))
