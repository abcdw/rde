(define-module (rde features shellutils)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-direnv))

(define* (feature-direnv
	  #:key
	  (direnv direnv))
  "Configure direnv and related Emacs packages."
  (ensure-pred any-package? direnv)

  (define (get-home-services config)
    "Returns home services related to direnv."
    (list
     (simple-service
      'direnv-packages
      home-profile-service-type (list direnv))
     (simple-service
      'direnv-config
      home-xdg-configuration-files-service-type
      `(("direnv/direnvrc" ,(plain-file
                             "direnvrc"
                             "\
use_guixs() {
  eval \"$(guix shell \"$@\" --search-paths)\"
}"))))
     (when (get-value 'zsh config)
       (simple-service
        'direnv-zsh-hook
        home-zsh-service-type
        (home-zsh-extension
         (zshrc
          (list
           "command -v direnv > /dev/null && eval \"$(direnv hook zsh)\"")))))

     ;; (add-hook 'Info-mode-hook
     ;;      (lambda ()
     ;;        (setq Info-additional-directory-list (split-string (getenv "INFOPATH") ":"))))
     (rde-elisp-configuration-service
      'envrc
      config
      `((eval-when-compile (require 'envrc))
        (add-hook 'after-init-hook 'envrc-global-mode)
        (with-eval-after-load 'envrc
         (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)))
      #:summary "\
Source environment for the project from envrc"
      #:commentary "\
Default keybinding for `envrc-command-map'."
      #:keywords '(convenience project)
      #:elisp-packages (list emacs-envrc))))

  (feature
   (name 'direnv)
   (values `((direnv . ,direnv)))
   (home-services-getter get-home-services)))
