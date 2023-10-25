;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 Peter Kannewitz <peter.kannewitz@posteo.net>
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

(define-module (rde features security-token)
   #:use-module (gnu packages security-token)
   #:use-module (gnu services base)
   #:use-module (gnu services security-token)
   #:use-module (gnu services)
   #:use-module (gnu system pam)
   #:use-module (guix gexp)
   #:use-module (rde features predicates)
   #:use-module (rde features)
   #:use-module (rde system services accounts)

   #:export (feature-pam-u2f-auth
             feature-security-token))

(define (feature-security-token)
  "Add specific configuration to make security tokens work. It
includes the configuration to be able to use the token as a user
(without sudo)."

  (define (get-system-services _)
    (list
     (service pcscd-service-type)
     (simple-service
      'security-token-add-plugdev-group-to-user
      rde-account-service-type
      (list "plugdev"))
     (udev-rules-service
      'yubikey
      libfido2
      #:groups '("plugdev"))))

  (feature
   (name 'security-token)
   (values `((security-token . #t)))
   (system-services-getter get-system-services)))

(define* (feature-pam-u2f-auth
          #:key
          (control "sufficient") ;; for possible control flags: man pam.conf
          (pam-modules
           '("login" "su" "sudo" "screen-locker" "swaylock" "greetd"))
          (pam-entry-args '()))

  "Configure pam modules to respect security token for authentication.  If not
specified otherwise default location for u2fcfg is `~/.config/Yubico/u2f-keys'
and can be initialised with `guix shell pam-u2f -- pamu2fcfg >
~/.config/Yubico/u2f-keys'."

  (ensure-pred string? control)
  (ensure-pred list-of-strings? pam-modules)
  (ensure-pred list-of-strings? pam-entry-args)

  (define f-name 'pam-u2f-auth)

  (define (pam-u2f-auth-extension pam)
    (if (member (pam-service-name pam) pam-modules)
        (pam-service
         (inherit pam)
         (auth
          (cons*
           (pam-entry (control control)
                      (module (file-append pam-u2f "/lib/security/pam_u2f.so"))
                      (arguments pam-entry-args))
           (pam-service-auth pam))))
        pam))

  (define (get-system-services config)
    (require-value 'security-token config)
    (list
     (simple-service 'pam-u2f pam-root-service-type
                     (list (pam-extension
                            (transformer pam-u2f-auth-extension))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (system-services-getter get-system-services)))
