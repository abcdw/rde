(define-module (rde features shellutils)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home-services shellutils)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-direnv))

(define* (feature-direnv
	  #:key
	  (package direnv))
  "Configure direnv and related Emacs packages."
  (ensure-pred package? package)

  (define (get-home-services config)
    "Returns home services related to direnv."
    (list
     (simple-service
      'direnv-packages
      home-profile-service-type (list package))
     (when (get-value 'zsh config)
       (simple-service
        'direnv-zsh-hook
        home-zsh-service-type
        (home-zsh-extension
	 (zshrc (list "eval \"$(direnv hook zsh)\"")))))

     ;; (add-hook 'Info-mode-hook
     ;;      (lambda ()
     ;;        (setq Info-additional-directory-list (split-string (getenv "INFOPATH") ":"))))
     (elisp-configuration-service
      'envrc
      `((add-hook 'after-init-hook 'envrc-global-mode)
        (with-eval-after-load 'envrc
         (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)))
      #:elisp-packages (list emacs-envrc))
    ))

  (feature
   (name 'direnv)
   (values `((direnv . #t)))
   (home-services-getter get-home-services)))
