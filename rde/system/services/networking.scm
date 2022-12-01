;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Nikita Domnitskii
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

(define-module (rde system services networking)
  #:use-module (rde serializers json)
  #:use-module (rde serializers ini)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu system shadow)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (yggdrasil-service-type
            yggdrasil-configuration

            iwd-service-type
            iwd-configuration))


;;;
;;; Yggdrasil.
;;;

(define-configuration/no-serialization yggdrasil-configuration
  (yggdrasil
   (file-like yggdrasil)
   "Yggdrasil package to use.")
  ;; TODO: Key generation functionality.
  (keys-json-path
   (string "/var/lib/yggdrasil/keys.json")
   "Path to file with keys.")
  (yggdrasil-conf
   (json-config '())
   "Yggdrasil configuration."))

(define %yggdrasil-accounts
  (list (user-group (name "yggdrasil") (system? #t))
        (user-account
         (name "yggdrasil")
         (group "yggdrasil")
         (system? #t)
         (comment "Yggdrasil daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (yggdrasil-configuration->yggdrasil-conf config)
  "Return a 'yggdrasil.conf' file for CONFIG."
  (apply mixed-text-file "yggdrasil.conf"
         (serialize-json-config
          (yggdrasil-configuration-yggdrasil-conf config))))

(define (yggdrasil-shepherd-service config)
  "Return a <shepherd-service> running Yggdrasil."
  (match config
    (($ <yggdrasil-configuration> )
     (let ((keys-json (yggdrasil-configuration-keys-json-path config))
           (yggdrasil (yggdrasil-configuration-yggdrasil config))
           (yggdrasil-conf (yggdrasil-configuration->yggdrasil-conf config)))
       (list (shepherd-service
              (provision '(yggdrasil))
              (requirement '(networking))

              (start #~(make-forkexec-constructor
                        (list #$(file-append yggdrasil "/bin/yggdrasil")
                              "-useconffile" #$yggdrasil-conf
                              "-extraconffile" #$keys-json)

                        #:log-file "/var/log/yggdrasil.log"
                        #:group "yggdrasil"))
              (stop #~(make-kill-destructor))
              (documentation "Run the Yggdrasil.")))))))

(define (yggdrasil-activation config)
  "Set up directories for Yggdrasil, if any."
  #~(begin
      (use-modules (guix build utils))

      (define (touch file-name)
        (system* #$(file-append coreutils "/bin/touch") file-name))

      (define %user (getpw "yggdrasil"))
      (define keys-json #$(yggdrasil-configuration-keys-json-path config))

      (mkdir-p (dirname keys-json))
      (chown (dirname keys-json) (passwd:uid %user) (passwd:gid %user))
      (chmod (dirname keys-json) #o700)
      (touch keys-json)
      (chown keys-json (passwd:uid %user) (passwd:gid %user))
      (chmod keys-json #o700)

      ;; Make sure /var/lib is accessible.
      (chmod "/var/lib" #o755)))

(define yggdrasil-service-type
  (service-type (name 'yggdrasil)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          yggdrasil-shepherd-service)
                       (service-extension account-service-type
                                          (const %yggdrasil-accounts))
                       (service-extension activation-service-type
                                          yggdrasil-activation)))

                (default-value (yggdrasil-configuration))
                (description
                 "Run the Yggdrasil networking daemon.")))


;;;
;;; iwd.
;;;

(define-configuration/no-serialization iwd-configuration
  (iwd (file-like iwd) "")
  (openresolv (file-like openresolv) "")
  (coreutils (file-like coreutils) "")
  (config (ini-config '()) ""))

(define (iwd-shepherd-service config)
  "Return a shepherd service for iwd"
  (let* ((iwd (iwd-configuration-iwd config))
         (openresolv (iwd-configuration-openresolv config))
         (coreutils (iwd-configuration-coreutils config))
         (environment #~(list (string-append
                               "PATH="
                               (string-append #$openresolv "/sbin")
                               ":"
                               (string-append #$coreutils "/bin")))))
    (list
     (shepherd-service
      (documentation "Run iwd")
      (provision '(iwd))
      (requirement '(user-processes dbus-system loopback))
      (start #~(make-forkexec-constructor
                (list (string-append #$iwd "/libexec/iwd"))
                #:log-file "/var/log/iwd.log"
                #:environment-variables #$environment))
      (stop #~(make-kill-destructor))))))

(define (iwd-etc-service config)
  (let ((cfg (iwd-configuration-config config)))
    `(("iwd/main.conf"
       ,(apply mixed-text-file
               "main.conf"
               (serialize-ini-config cfg))))))

(define iwd-package (compose list iwd-configuration-iwd))

(define iwd-service-type
  (service-type
   (name 'iwd)
   (extensions
    (list (service-extension
           shepherd-root-service-type
           iwd-shepherd-service)
          (service-extension
           dbus-root-service-type
           iwd-package)
          (service-extension
           etc-service-type
           iwd-etc-service)
          (service-extension
           profile-service-type
           iwd-package)))
   (default-value (iwd-configuration))
   (description "Configure and run iNet Wireless Daemon (iwd).")))
