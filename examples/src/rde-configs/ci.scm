(define-module (rde-configs ci)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features shells))


;;; Code:

(define minimal-rde-config
  (rde-config
   (features
    (list
     (feature-user-info
      #:user-name "bob"
      #:full-name "Andrew Tropin"
      #:email "andrew@trop.in"
      #:emacs-advanced-user? #t)
     (feature-zsh)))))

(rde-config-home-environment minimal-rde-config)
