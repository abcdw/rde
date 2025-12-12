;;; rde --- Reproducible development environment
;;;
;;; Copyright © 2022, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (rde features virtualization)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu services)
  #:use-module (gnu services virtualization)
  #:use-module (gnu home services)
  #:use-module (rde system services accounts)
  #:use-module (srfi srfi-26)

  #:export (feature-qemu))

(define* (feature-qemu
          #:key
          (qemu qemu)
          (virt-manager virt-manager)
          (emulate-other-archs '()))
  "Configure qemu, a machine emulator and virtualizer.

EMULATE-OTHER-ARCHS is a list of valid architectures for the
@code{qemu-binfmt-service-type}, as recommended in (guix) Submitting Patches."

  (ensure-pred file-like? qemu)
  (ensure-pred file-like? virt-manager)
  (let ((platform-names (map qemu-platform-name %qemu-platforms)))
    (ensure-pred (list-of (cut member <> platform-names)) emulate-other-archs))

  (define f-name 'qemu)
  (define (get-home-services config)
    (list
     (simple-service
      'qemu-add-qemu-package
      home-profile-service-type
      (list qemu virt-manager))))

  (define (get-system-services config)
    (append
     (list
      (service libvirt-service-type)
      (service virtlog-service-type)
      (simple-service
       'qemu-add-kvm-group-to-user
       rde-account-service-type
       (list "kvm" "libvirt")))
     (if (not (null? emulate-other-archs))
         (list
          (service qemu-binfmt-service-type
                   (qemu-binfmt-configuration
                    (platforms
                     (apply lookup-qemu-platforms emulate-other-archs)))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
