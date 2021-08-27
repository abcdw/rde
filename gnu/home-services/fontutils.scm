;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu home-services fontutils)
  #:use-module (gnu home-services)
  #:use-module (gnu packages fontutils)
  #:use-module (guix gexp)

  #:export (home-fontconfig-service-type))

;;; Commentary:
;;;
;;; Services related to fonts.  home-fontconfig service provides
;;; fontconfig configuration, which allows fc-* utilities to find
;;; fonts in Guix Home's profile and regenerates font cache on
;;; activation.
;;;
;;; Code:

(define (add-fontconfig-config-file he-symlink-path)
  `(("config/fontconfig/fonts.conf"
     ,(mixed-text-file
       "fonts.conf"
       "<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
  <dir>~/.guix-home/profile/share/fonts</dir>
</fontconfig>"))))

(define (regenerate-font-cache-gexp _)
  `(("profile/share/fonts"
     ,#~(system* #$(file-append fontconfig "/bin/fc-cache") "-fv"))))

(define home-fontconfig-service-type
  (service-type (name 'home-fontconfig)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-fontconfig-config-file)
                       (service-extension
                        home-run-on-change-service-type
                        regenerate-font-cache-gexp)
                       (service-extension
                        home-profile-service-type
                        (const (list fontconfig)))))
                (default-value #f)
                (description
                 "Provides configuration file for fontconfig and make
fc-* utilities aware of font packages installed in Guix Home's profile.")))
