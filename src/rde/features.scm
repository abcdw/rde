;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features)
  #:use-module (guix records)
  #:use-module (guix ui)
  #:use-module (guix gexp)

  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system nss)
  #:use-module (gnu services guix)
  #:use-module (rde system bare-bone)
  #:use-module (rde system services accounts)
  #:use-module (rde system services admin)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu home)
  #:use-module (gnu services configuration)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)

  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)

  #:export (rde-config
            rde-config-features
            rde-config-home-environment
            rde-config-home-services
            rde-config-home-packages
            rde-config-operating-system
            rde-config-system-services

            override-rde-config-with-values
            pretty-print-rde-config

            feature
            feature-name
            feature-values
            feature-home-services-getter
            feature-system-services-getter

            merge-features

            make-feature-values
            require-value
            get-value
            get-value-eval
            service-type->rde-value

            ensure-pred
            throw-message))

(define (alist? lst)
  (every pair? lst))

(define (services-getter? fn)
  (procedure? fn))

(define-configuration/no-serialization feature
  (name
   (symbol)
   "Name for feature to identify it in debug messages.")
  (values
   (alist '())
   "List of pairs available to share across services.")
  (home-services-getter
   (services-getter (const '()))
   "Function taking one argument (@code{values}) and returning a list
of services.  Service can be either @code{service?} or
@code{#f}. Resulting list will be appended to home environment.")
  (system-services-getter
   (services-getter (const '()))
   "Function taking one argument (@code{values}) and returning a list
of services.  Service can be either @code{service?} or
@code{#f}. Resulting list will be appended to operating system."))

(define-record-type* <rde-config> rde-config
  make-rde-config
  rde-config?
  this-rde-config

  (features rde-config-features
            (default '()))

  (values
   rde-config-values
   (thunked)
   (default
     (fold-values
      (rde-config-features this-rde-config))))

  (values-alist
   rde-config-values-alist
   (thunked)
   (default
     ;; Doesn't ensure that there is no duplicates in values.  This
     ;; field is inteded for debugging/development purposes.
     (apply append
            (map feature-values
                 (rde-config-features this-rde-config)))))

  (home-services
   rde-config-home-services
   (thunked)
   (default
     (fold-home-services
      (rde-config-features this-rde-config)
      this-rde-config)))
  (home-environment
   rde-config-home-environment
   (thunked)
   (default
     (get-home-environment this-rde-config)))
  (home-packages
   rde-config-home-packages
   (thunked)
   (default
     (get-home-environment-packages this-rde-config)))

  (integrate-he-in-os?
   rde-config-integrate-he-in-os?
   (default #f))

  (initial-os
   rde-config-initial-os
   (default bare-bone-os))
  (system-services
   rde-config-system-services
   (thunked)
   (default
     (fold-system-services
      (rde-config-features this-rde-config)
      this-rde-config)))
  (operating-system
   rde-config-operating-system
   (thunked)
   (default
     (get-operating-system this-rde-config))))

(define-syntax ensure-pred
  (syntax-rules ()
    ((ensure-pred pred field)
     (when (not (pred field))
       (raise (condition
               (&message
                (message
                 (format
                  #f (G_ "~a: The predicate '~a' is not satisfied with value '~a'.")
                  'field
                  (procedure-name pred)
                  field)))))))))

;; TODO: Move it to approprate module
(define-syntax throw-message
  (syntax-rules ()
    ((throw-message pred msg)
     (when pred
       (raise (condition
               (&message
                (message
                 msg))))))))

(define-syntax make-feature-values
  (syntax-rules ()
    ((provide-values field ...)
     `((field . ,field) ...))))


(define (fold-values features)
  (let ((f-values (apply append (map feature-values features))))
    (fold
     (lambda (feature acc)
       (fold
        (lambda (x acc)
          (throw-message
           (hash-get-handle acc (car x))
           (format #f (G_ "Duplicate entry came from ~a feature:\n~a\n
The previous value was:\n~a\n")
                   (feature-name feature)
                   x
                   (hash-get-handle acc (car x))))
          (hash-set! acc (car x) (cdr x))
          acc)
        acc
        (feature-values feature))
       acc)
     (make-hash-table)
     features)))

(define (print-values features)
  (hash-for-each-handle pretty-print
                        (fold-values features)))

(define (fold-some-services features config services-getter)
  (filter service?
          (apply append
                 (map (lambda (f)
                        ((services-getter f) config))
                      features))))

(define (fold-home-services features config)
  "Generates a list of home-services from FEATURES by passing CONFIG
to each home-services-getter function."
  (fold-some-services features config feature-home-services-getter))

(define (fold-system-services features config)
  "Generates a list of system-services from FEATURES by passing CONFIG
to each system-services-getter function."
  (fold-some-services features config feature-system-services-getter))


(define* (get-value key config #:optional (default-value *unspecified*))
  "Get @code{key} from @code{config.}  If @code{default-value} is not
 specified,call @code{require-value} to make sure that @code{key} is present
 in the @code{config}."
  (when (unspecified? default-value)
    (require-value key config))
  (let ((handle (hash-get-handle (rde-config-values config) key)))
    (if handle
        (cdr handle)
        default-value)))

(define* (get-value-eval key config #:optional default-value)
  "Get KEY from rde-config-values, if the value is a function apply it
to config one more time."
  (let* ((handle (hash-get-handle (rde-config-values config) key))
         (val (if handle (cdr handle) default-value)))
      (if (procedure? val) (val config) val)))

(define* (require-value key config #:optional (additional-msg #f))
  (throw-message
   (not (hash-get-handle (rde-config-values config) key))
   (format
    #f "Value ~a is not provided by any feature.\n~a"
    key (or (and=> additional-msg (lambda (x) (string-append x "\n"))) ""))))

(define (service-type->rde-value service-type)
  "Returns a pair of service name and a value of SERVICE-TYPE.  This rde value
can be later used to extend original service with additional configuration."
  `(,(service-type-name service-type) . ,service-type))

;; TODO: [Andrew Tropin, 2024-02-08] Move to module definition
(use-modules (gnu home services)
             (gnu home services xdg)
             (gnu home services fontutils)
             (gnu home services symlink-manager)
             (rde home services shells))

(define (get-home-environment config)
  (home-environment
   (essential-services
    (list
     ;; MAYBE: Upstream it.
     (simple-service
      'consider-relogin
      home-run-on-change-service-type
      `(("setup-environment"
         ,#~(display "The setup-environment script was updated.
  Re-login to get environment variables updated.\n"))
        ("on-first-login"
         ,#~(display "The on-first-login script was updated.
  Re-login to get the changes applied.\n"))))
     (service home-run-on-first-login-service-type)
     (service home-activation-service-type)
     (service home-environment-variables-service-type)

     (service home-symlink-manager-service-type)

     ;; TODO: Remove fontconfig-minimal and shepherd package if he used for
     ;; building rde for standalone project
     (service home-fontconfig-service-type)
     (service home-xdg-base-directories-service-type)
     (service home-shell-profile-service-type)

     (service home-service-type)
     (service home-profile-service-type '())))
   (services (rde-config-home-services config))))

(define* (home-environment-packages he)
  "Return a list of packages."
  (let* ((services (home-environment-services he))
         (packages (fold-services services
                                  #:target-type home-profile-service-type)))
    (service-value packages)))

(define (get-home-environment-packages config)
  (home-environment-packages (get-home-environment config)))

(define (get-operating-system config)
  (let* ((initial-os (rde-config-initial-os config))

         (host-name        (get-value
                            'host-name config
                            (operating-system-host-name initial-os)))
         (timezone         (get-value
                            'timezone config
                            (operating-system-timezone initial-os)))
         (locale           (get-value
                            'locale config
                            (operating-system-locale initial-os)))
         (issue            (get-value
                            'issue config
                            (operating-system-locale initial-os)))
         (keyboard-layout  (get-value
                            'keyboard-layout config
                            (operating-system-keyboard-layout initial-os)))
         (bootloader-cfg   (get-value
                            'bootloader-configuration config
                            (operating-system-bootloader initial-os)))
         (bootloader       (bootloader-configuration
                            (inherit bootloader-cfg)
                            (keyboard-layout keyboard-layout)))
         (mapped-devices   (get-value
                            'mapped-devices config
                            (operating-system-mapped-devices initial-os)))
         (swap-devices     (get-value
                            'swap-devices config
                            (operating-system-swap-devices initial-os)))
         (file-systems     (get-value
                            'file-systems config
                            (operating-system-file-systems initial-os)))

         (user-name        (get-value 'user-name config #f))
         (full-name        (get-value 'full-name config ""))
         (user-groups      (get-value 'user-groups config '()))
         (home-directory   (get-value
                            'home-directory config
                            (string-append "/home/" (or user-name "user"))))
         (login-shell      (get-value 'login-shell config (default-shell)))
         (user-password    (get-value 'user-initial-password-hash config #f))

         (user             (if user-name
                               (user-account
                                (name user-name)
                                (comment full-name)
                                (password user-password)
                                (home-directory home-directory)
                                (shell login-shell)
                                (group "users")
                                (supplementary-groups user-groups))
                               #f))

         (services         (rde-config-system-services config))

         (kernel           (get-value
                            'kernel config
                            (operating-system-kernel initial-os)))
         (kernel-arguments (get-value
                            'kernel-arguments config
                            (operating-system-user-kernel-arguments initial-os)))
         (kernel-modules   (get-value
                            'kernel-loadable-modules config
                            (operating-system-kernel-loadable-modules initial-os)))
         (initrd           (get-value
                            'initrd config
                            (operating-system-initrd initial-os)))
         (initrd-modules   (get-value
                            'initrd-modules config
                            (operating-system-initrd-modules initial-os)))
         (firmware         (get-value
                            'firmware config
                            (operating-system-firmware initial-os)))
         (name-service-switch
                           (get-value
                            'name-service config
                            %default-nss)))

    (when (rde-config-integrate-he-in-os? config)
      (require-value 'user-name config))

    (operating-system
      (inherit initial-os)
      (host-name host-name)
      (timezone timezone)
      (locale locale)
      (issue issue)
      (bootloader bootloader)
      (mapped-devices mapped-devices)
      (swap-devices swap-devices)
      (file-systems file-systems)
      (keyboard-layout keyboard-layout)
      (kernel kernel)
      (kernel-arguments kernel-arguments)
      (kernel-loadable-modules kernel-modules)
      (initrd initrd)
      (initrd-modules initrd-modules)
      (firmware firmware)
      (services (append
                 services
                 (if (rde-config-integrate-he-in-os? config)
                     (list (service guix-home-service-type
                                    ;; TODO: [Andrew Tropin, 2024-05-27]
                                    ;; Temporary fix, remove it, when
                                    ;; https://issues.guix.gnu.org/71111 is
                                    ;; merged
                                    `(,(list
                                        user-name
                                        (get-home-environment config)))))
                     '())
                 (list (service sudoers-service-type))
                 (if user-name
                     (list (service rde-account-service-type user))
                     '())))
      (sudoers-file #f)
      (name-service-switch name-service-switch))))

(define (pretty-print-rde-config config)
  (use-modules (gnu services)
               (ice-9 pretty-print))
  (pretty-print
   (rde-config-values-alist
    config))
  (pretty-print
   (map service-kind
        (rde-config-home-services
         config)))
  (pretty-print
   (map service-kind
        (rde-config-system-services
         config))))

(define (override-rde-config-with-values config value-pairs)
  "Override an <rde-config> with a list of value pairs."
  (let ((values-hash-table (rde-config-values config)))
    ;; Updates the values hash table
    (fold
     (lambda (x acc)
       (match x
         ((name . value) (hash-set! acc name value)))
       acc)
     values-hash-table
     value-pairs)
    ;; Returns an updated config
    (rde-config
     (inherit config)
     (values values-hash-table))))

(define (merge-features features)
  "Combine a few features into one."
  (feature
   (values (append-map feature-values features))
   (home-services-getter
    (lambda (config)
      (append-map
       (lambda (f) ((feature-home-services-getter f) config)) features)))
   (system-services-getter
    (lambda (config)
      (append-map
       (lambda (f) ((feature-system-services-getter f) config)) features)))))

(define (features->home-services features)
  "Get home services out of list of rde features."
  ;; It's written for demonstration purpose, not intended to be a part of rde
  ;; features API.
  (let ((config (rde-config (features features))))
    (fold-home-services features config)))

;; (pretty-print-rde-config
;;  (rde-config
;;   (features my-features)))

;; (rde-config-home-environment my-cfg)
