(define-module (gnu home-services password-utils)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-password-store-configuration
            home-password-store-service-type))

;;; Commentary:
;;;
;;; Services related to password management.
;;;
;;; Code:

;;;
;;; Password store.
;;;
;;; (home-password-store-configuration
;;;   (directory "~/.local/var/lib/pass")
;;;   (config '((clip-time . 34)
;;;             (gpg-opts . ("--keyring" "~/.local/share/keyring.kbx")))))
;;;

;; TODO: Add proper Browserpass integration with Chromium, IceCat, and
;; Nyxt.
(define-configuration/no-serialization home-password-store-configuration
  (package
    (package password-store)
    "The password store package to use.")
  (directory
   (string "${XDG_STATE_HOME:-$HOME/.local/var/lib}/password-store")
   "The directory in which to put the passwords in.")
  (browserpass-native?
   (boolean #f)
   "Whether to install the @var{browserpass-native} package for browser
integration.  The @code{browserpass} extension will have to be
installed separately.")
  (config
   (alist '())
   "Association list of environment variables to set for the password
store.  The key of the pair will automatically be prepended with
``@code{PASSWORD_STORE_}'', meaning that @code{clip-time} will result
in ``@code{PASSWORD_STORE_CLIP_TIME}''."))

(define (serialize-field field-name val)
  (cons
   (string-append "PASSWORD_STORE_"
                  (object->snake-case-string field-name 'upper))
   (cond
    ((list? val) (string-join val " "))
    (else (maybe-object->string val)))))

(define (home-password-store-environment-variables-services config)
 (cons*
  `("PASSWORD_STORE_DIR"
    . ,(home-password-store-configuration-directory config))
  (map (match-lambda
         ((car . cdr) (serialize-field car cdr)))
       (home-password-store-configuration-config config))))

(define (home-password-store-profile config)
  (let ((base-package (list
                       (home-password-store-configuration-package config))))
    (cond
     ((home-password-store-configuration-browserpass-native? config)
      (cons browserpass-native base-package))
     (else base-package))))

(define home-password-store-service-type
  (service-type (name 'home-password-store)
                (extensions
                 (list (service-extension
                        home-environment-variables-service-type
                        home-password-store-environment-variables-services)
                       (service-extension
                        home-profile-service-type
                        home-password-store-profile)))
		(default-value (home-password-store-configuration))
                (description
                 "Install and configure @code{pass}, the password store.")))

(define (generate-home-password-store-documentation)
  (generate-documentation
   `((home-password-store-configuration
      ,home-password-store-configuration-fields))
   'home-password-store-configuration))
