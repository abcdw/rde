(define-module (rde features security-token)
  #:use-module (rde features)
  #:use-module (gnu packages security-token)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)
  #:use-module (rde system services accounts)
  #:use-module (guix gexp)

  #:export (feature-security-token))

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
