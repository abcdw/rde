(define-module (rde features shells)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home-services shellutils)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (guix gexp)

  #:export (feature-zsh))

(define* (feature-zsh
	  #:key
	  (package zsh)
	  (default-shell? #t)
	  (enable-zsh-autosuggestions? #t))
  "Configure Zsh."
  (ensure-pred package? package)

  (define (zsh-home-services config)
    "Returns home services related to Zsh."
    (list
     (when default-shell?
       (simple-service
	'set-default-shell-to-zsh
	home-environment-variables-service-type
	`(("SHELL" . ,(file-append package "/bin/zsh")))))

     ;; zsh-autosuggestions is very cool plugin, but a little
     ;; distractive, I find it a little against Attention-friendly
     ;; principle
     (when enable-zsh-autosuggestions?
       (service home-zsh-autosuggestions-service-type
                zsh-autosuggestions-latest))

     ;; https://github.com/purcell/envrc
     ;; home-zsh-direnv-service
     (service
      home-zsh-service-type
      (home-zsh-configuration
       (xdg-flavor? #t)
       (package package)
       (zshrc
	(list
	 (slurp-file-gexp (local-file "./zsh/zshrc"))
	 "alias state-sync='herd sync state && pass git push origin master'"))))))

  (feature
   (name 'zsh)
   (values `((zsh . #t)))
   (home-services-getter zsh-home-services)))
