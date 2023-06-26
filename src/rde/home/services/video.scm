;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (rde home services video)
  #:use-module (gnu home services)
  #:use-module (gnu packages video)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (rde serializers ini)
  #:export (home-mpv-configuration
            home-mpv-extension
            home-mpv-service-type))

;;; Commentary:
;;;
;;; This module contains services related to video playback and
;;; editing.
;;;
;;; Code:


;;;
;;; mpv.
;;;


(define-configuration/no-serialization home-mpv-configuration
  (mpv
   (file-like mpv)
   "The mpv package to use.")
  (mpv-conf
   (ini-config '())
   "Configuration that will be serialized to
@file{$XDG_CONFIG_HOME/mpv/mpv.conf} file.")
  (input-conf
   (ini-config '())
   "Configuration that will be serialized to
@file{$XDG_CONFIG_HOME/mpv/input.conf} file."))

(define-configuration/no-serialization home-mpv-extension
  (mpv-conf
   (ini-config '())
   "Configuration that will be added to mpv-conf field of original
configuration.")
  (input-conf
   (ini-config '())
   "Configuration that will be added to input-conf field of original
configuration."))

(define (mpv-files-service config)
  `(("mpv/mpv.conf"
     ,(apply
       mixed-text-file
       "mpv-mpv.conf"
       (serialize-ini-config (home-mpv-configuration-mpv-conf config))))
    ("mpv/input.conf"
     ,(apply
       mixed-text-file
       "mpv-input.conf"
       (serialize-ini-config (home-mpv-configuration-input-conf config))))))

(define (mpv-extensions original-config extensions)
  (let ((extensions (reverse extensions)))
    (home-mpv-configuration
     (inherit original-config)
     (mpv-conf
      (fold
       ini-merge
       (home-mpv-configuration-mpv-conf original-config)
       (map home-mpv-extension-mpv-conf extensions)))
     (input-conf
      (fold
       ini-merge
       (home-mpv-configuration-input-conf original-config)
       (map home-mpv-extension-input-conf extensions))))))

(define (mpv-profile-service config)
  (list (home-mpv-configuration-mpv config)))

(define home-mpv-service-type
  (service-type (name 'home-mpv)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        mpv-files-service)
                       (service-extension
                        home-profile-service-type
                        mpv-profile-service)))
                (compose identity)
                (extend mpv-extensions)
                (description "Install and configure mpv")))
