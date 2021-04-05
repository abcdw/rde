(define-module (gnu home-services shellutils)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services shells)
  #:use-module (gnu packages shellutils)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (home-zsh-plugin-manager-service-type
	    home-zsh-direnv-service))

(define (add-zsh-plugins-load-command packages)
  (home-zsh-extension
   (zshrc
    (append
     '("he_zsh_plugins_dir=$GUIX_HOME_ENVIRONMENT_DIRECTORY/profile/share/zsh/plugins")
     (map
      (lambda (p)
	(let ((x (package-name p)))
	  (format #f "source $he_zsh_plugins_dir/~a/~a.zsh" x x)))
      packages)))))

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
Install plugins in profile and configure Zsh to load them.")))

(define home-zsh-direnv-service
  (home-generic-service
   'home-zsh-direnv
   #:packages (list direnv)
   #:extensions (list
		 (cons
		  home-zsh-service-type
		  (home-zsh-extension
		    (zshrc (list "eval \"$(direnv hook zsh)\"")))))))
