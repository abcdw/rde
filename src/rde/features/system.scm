;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features system)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (rde system bare-bone)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services pam-mount)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system linux-initrd)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (feature-bootloader
            feature-host-info
            feature-file-systems
            feature-kernel))


(define* (feature-host-info
          #:key
          (host-name (operating-system-host-name bare-bone-os))
          (timezone  (operating-system-timezone  bare-bone-os))
          (locale    (operating-system-locale    bare-bone-os))
          (issue     (operating-system-issue     bare-bone-os)))
  "Provides basic information about host."
  (ensure-pred string? host-name)
  (ensure-pred string? timezone)
  (ensure-pred string? locale)
  (ensure-pred string? issue)

  (feature
   (name 'host-info)
   (values (make-feature-values
            host-name timezone locale issue))))


(define %default-bootloader-configuration
  (operating-system-bootloader bare-bone-os))

;; TODO: Add LUKS2 support https://lists.gnu.org/archive/html/grub-devel/2022-07/msg00037.html
(define* (feature-bootloader
          #:key (bootloader-configuration %default-bootloader-configuration))
  "Provides custom bootloader configuration for operating-system.
keyboard-layout will be overriden by feature-keyboard if it present."
  (ensure-pred bootloader-configuration? bootloader-configuration)

  (feature
   (name 'bootloader)
   (values (make-feature-values bootloader-configuration))))


(define* (feature-file-systems
          #:key
          (mapped-devices '())
          (swap-devices '())
          (file-systems '())
          (base-file-systems %base-file-systems)
          (user-pam-file-systems '()))
  "Provides file systems for operating-system.  By default
%base-file-systems will be added to the end of FILE-SYSTEMS, this
behavior can be overriden with BASE-FILE-SYSTEM argument.

USER-PAM-FILE-SYSTEMS are mounted with PAM-mount, when the user logs in rather
than at boot, and are unmounted when the user logs out."
  (ensure-pred list-of-mapped-devices? mapped-devices)
  (ensure-pred list-of-swap-devices? swap-devices)
  (ensure-pred list-of-file-systems? file-systems)
  (ensure-pred list-of-file-systems? base-file-systems)
  (ensure-pred list-of-file-systems? user-pam-file-systems)

  (define (get-system-services config)
    (if user-pam-file-systems
        (let ((file-system->pam-mount-volume
               (lambda (fs)
                 (match (file-system->spec fs)
                   ((file-name mount-point type flags options rest ...)
                    ;; mount-may-fail? check? skip-check-if-clean? repair
                    (pam-mount-volume
                     (user-name (get-value 'user-name config))
                     (file-system-type type)
                     (file-name file-name)
                     (mount-point mount-point)
                     (options options)))))))
          (list
           (simple-service
            'pam-user-volumes
            pam-mount-volume-service-type
            (map file-system->pam-mount-volume user-pam-file-systems))))
        '()))

  (let ((file-systems (append file-systems base-file-systems)))
    (feature
     (name 'file-systems)
     (values (make-feature-values mapped-devices swap-devices file-systems))
     (system-services-getter get-system-services))))


(define* (feature-kernel
          #:key
          (kernel linux-libre)
          (kernel-loadable-modules '())
          (kernel-arguments '())
          (default-kernel-arguments %default-kernel-arguments)
          (initrd base-initrd)
          (initrd-modules '())
          (base-initrd-modules %base-initrd-modules)
          (firmware '())
          (base-firmware %base-firmware))
  "Provides kernel configuration."
  (ensure-pred file-like? kernel)
  (ensure-pred list-of-file-likes? kernel-loadable-modules)
  (ensure-pred list-of-string-or-gexps? kernel-arguments)
  (ensure-pred list-of-string-or-gexps? default-kernel-arguments)
  (ensure-pred list-of-file-likes? firmware)
  (ensure-pred list-of-file-likes? base-firmware)
  (ensure-pred procedure? initrd)
  (ensure-pred list-of-strings? initrd-modules)
  (ensure-pred list-of-strings? base-initrd-modules)

  (let ((kernel-arguments (append kernel-arguments default-kernel-arguments))
        (firmware         (append firmware base-firmware))
        (initrd-modules   (append initrd-modules base-initrd-modules)))
    (feature
     (name 'kernel)
     (values (make-feature-values
              kernel kernel-loadable-modules kernel-arguments
              initrd initrd-modules firmware)))))
