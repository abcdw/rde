(define-module (gnu services home)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)

  #:export (guix-home-service-type))


(define (guix-home-shepherd-service config)
  (map
   (lambda (x)
     (let ((user (car x))
           (he (cdr x)))
       (shepherd-service
        (documentation "Activate Guix Home.")
        (requirement '(user-homes))
        (provision (list (symbol-append 'guix-home- (string->symbol user))))
        (one-shot? #t)
        (auto-start? #f)
        (start #~(make-forkexec-constructor
                  '(#$(file-append he "/activate"))
                  #:user #$user
                  #:environment-variables
                  (list (string-append "HOME=" (passwd:dir (getpw #$user))))
                  #:group (group:name (getgrgid (passwd:gid (getpw #$user))))))
        (stop #~(make-kill-destructor)))))
     config))

(define (guix-home-gc-roots config)
  (map cdr config))

(define guix-home-service-type
  (service-type
   (name 'guix-home)
   (description "Setups home-environments specified in the value.")
   (extensions (list (service-extension
                      shepherd-root-service-type
                      guix-home-shepherd-service)
                     (service-extension
                      gc-root-service-type
                      guix-home-gc-roots)))
   ;; (compose append)
   ;; (extend append)
   (default-value '())))
