(define-module (gnu home-services shellutils)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home services)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services shells)
  #:use-module (gnu packages shellutils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (home-zsh-plugin-manager-service-type
	    home-zsh-autosuggestions-service-type
	    home-zsh-direnv-service-type
            home-bash-direnv-service-type))

(define (add-zsh-plugins-load-command packages)
  (home-zsh-extension
   (zshrc
    (map
     (lambda (p)
       (let ((x (package-name p)))
	 #~(string-append
	    "source " #$p #$(format #f "/share/zsh/plugins/~a/~a.zsh" x x))))
     packages))))

(define home-zsh-plugin-manager-service-type
  (service-type (name 'home-zsh-plugin-manager)
                (extensions
                 (list (service-extension
                        home-zsh-service-type
                        add-zsh-plugins-load-command)
                       (service-extension
                        home-profile-service-type
                        identity)))
		(compose concatenate)
		(extend append)
                (default-value '())
                (description "\
Install plugins into the home profile and configure Zsh to load them.")))

(define home-zsh-autosuggestions-service-type
  (service-type
   (name 'home-zsh-autosuggestions)
   (extensions
    (list
     (service-extension home-zsh-plugin-manager-service-type list)
     (service-extension
      home-zsh-service-type
      (const
       (home-zsh-extension
	;; We set variables in zshrc because we need them only in
	;; interactive shell.
	(zshrc '("# Improve the behavior and perfomance of auto suggestions"
		 "ZSH_AUTOSUGGEST_MANUAL_REBIND=true"
		 "ZSH_AUTOSUGGEST_USE_ASYNC=true"
		 "ZSH_AUTOSUGGEST_STRATEGY=(history completion)")))))))
   (default-value zsh-autosuggestions)
   (description "Enable Fish-like fast and unobtrusive autosuggestions
for Zsh, and set reasonable default values for some plugin's variables
to improve perfomance.")))

;; MAYBE: Remove this services?  They are quite unflexible without
;; nix/rde extensions mechanism.
(define home-zsh-direnv-service-type
  (service-type
   (name 'home-zsh-direnv)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      (const (list direnv)))
     (service-extension
      home-zsh-service-type
      (const (home-zsh-extension
	      (zshrc (list "eval \"$(direnv hook zsh)\"")))))))
   (default-value #f)
   (description "Enable Direnv integration for Zsh.")))

(define home-bash-direnv-service-type
  (service-type
   (name 'home-bash-direnv)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      (const (list direnv)))
     (service-extension
      home-bash-service-type
      (const (home-bash-extension
              (bashrc (list "eval \"$(direnv hook bash)\"")))))))
   (default-value #f)
   (description "Enable Direnv integration for bash.")))
