(define-module (gnu home-services shellutils)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home services)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services shells)
  #:use-module (gnu packages shellutils)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (home-zsh-plugin-manager-service-type
	    home-zsh-autosuggestions-service-type
	    home-zsh-direnv-service-type
            home-bash-direnv-service-type))

(define-deprecated/alias home-zsh-plugin-manager-service-type
  (@ (rde home services shellutils) home-zsh-plugin-manager-service-type))
(define-deprecated/alias home-zsh-autosuggestions-service-type
  (@ (rde home services shellutils) home-zsh-autosuggestions-service-type))

(define-deprecated/alias home-zsh-direnv-service-type #f)
(define-deprecated/alias home-bash-direnv-service-type #f)
