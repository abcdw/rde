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

(define-module (rde features tmux)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages tmux)
  #:use-module (guix gexp)

  #:export (feature-tmux))

(define* (feature-tmux
          #:key
          tmux-conf
          (tmux tmux))
  "Configure tmux."
  (ensure-pred maybe-file-like? tmux-conf)
  (ensure-pred file-like? tmux)

  (define (tmux-home-services config)
    "Returns home services related to tmux."
    (list
     (simple-service
      'home-tmux-tmux-conf
      home-xdg-configuration-files-service-type
      (filter list?
              (list (when tmux-conf
                      (list "tmux/tmux.conf" tmux-conf)))))
     (simple-service
      'home-tmux-package
      home-profile-service-type
      (list tmux))))

  (feature
   (name 'tmux)
   (values `((tmux . ,tmux)))
   (home-services-getter tmux-home-services)))
