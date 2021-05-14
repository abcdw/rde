(use-modules (rde features)
	     (rde features base)
	     (rde features gnupg)
	     (ice-9 match))

;; (define my-features
;;   (list
;;    (feature-user-info #:user-name "bob"
;; 		#:full-name "Andrew Tropin"
;; 		#:email "andrew@trop.in")
;;    ;; (feature-gnupg #:gpg-primary-key "74830A276C328EC2")
;;    ;; (feature-keyboard #:layout "us,ru" #:options "dvorak,")
;;    ;; (feature-sway)
;;    ;; (feature-projects #:directory "work")
;;    ;; (feature-emacs #:server? #t)
;;    ;; (feature-emacs-rss)
;;    ;; (feature-emacs-org-roam)
;;    ))

(define %bob-features
  (list
   (feature-user-info
    #:user-name "bob"
    #:full-name "Andrew Tropin"
    #:email "andrew@trop.in")
   (feature-gnupg
    #:gpg-primary-key "74830A276C328EC2")))

(define %abcdw-features
  (list
   (feature-user-info
    #:user-name "abcdw"
    #:full-name "Andrew Tropin"
    #:email "andrew@trop.in")
   (feature-gnupg
    #:gpg-primary-key "74830A276C328EC2")))

(define %devenv-features
  (list
   ;; (feature-git
   ;;  ;; #:gpg-sign-key ""
   ;;  #:sign-commits? #t)
   ))

(define %generic-features
  (list))

(define %laptop-features '())

(define ixy-config
  (rde-config
   (features
    (append
     %bob-features
     %laptop-features
     %devenv-features))))

(use-modules (gnu home))
(home-environment-location (rde-config-home-environment ixy-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("ixy-home" (rde-config-home-environment ixy-config))
      ("ixy-system" (rde-config-operating-system ixy-config)))))

;; Commented to prevent channel from throwing exception on compilation
;; Uncomment for your personal config
;; (dispatcher)
