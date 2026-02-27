(define-module (rde features difftastic)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix gexp)
  #:export (feature-difftastic))

(define* (feature-difftastic
          #:key
          (difftastic difftastic)
          (parse-error-limit #f))
  "Setup and configure Difftastic, a structural diff tool."
  (ensure-pred file-like? difftastic)
  (ensure-pred maybe-integer? parse-error-limit)

  (define f-name 'difftastic)

  (define difft-cmd
    (let ((difft (file-append difftastic "/bin/difft")))
      (if parse-error-limit
          (program-file "difft-wrapper"
            #~(apply execl #$difft "difft"
                     "--parse-error-limit"
                     #$(number->string parse-error-limit)
                     (cdr (command-line))))
          difft)))

  (define (get-home-services config)
    (list
     (simple-service
      'add-difftastic-home-package
      home-profile-service-type
      (list difftastic))
     (simple-service
      'difftastic-git-config
      home-git-service-type
      (home-git-extension
       (config
        `((diff
           ((external . ,difft-cmd)))))))))

  (feature
   (name f-name)
   (values `((,f-name . ,difftastic)))
   (home-services-getter get-home-services)))
