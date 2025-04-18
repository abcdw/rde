;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2024 Kirill Yermak <kirill@kimimii.org>
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

(define-module (rde system bare-bone)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:export (bare-bone-os))

(define bare-bone-os
  (operating-system
   (host-name "antelope")
   (timezone  "Etc/UTC")
   (locale    "en_US.utf8")
   (bootloader (bootloader-configuration
                (bootloader grub-efi-removable-bootloader)
                (targets '("/boot/efi"))))
   (issue "This is RDE.  Welcome.\n")
   (services '())
   (sudoers-file #f)
   (file-systems %base-file-systems)))
