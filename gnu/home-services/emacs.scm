(define-module (gnu home-services emacs)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services files)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services configuration)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 curried-definitions)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system emacs)
  #:export (home-emacs-service-type
	    home-emacs-configuration))

(define (packages? lst)
  (and (list? lst) (every package? lst)))

(define elisp-packages? packages?)
(define serialize-elisp-packages
  (const ""))
(define serialize-boolean
  (const ""))

(define-configuration home-emacs-configuration
  (package
   (package emacs)
   "The Emacs package to use.")
  (elisp-packages
   (elisp-packages '())
   "A list of Emacs Lisp packages.")
  (rebuild-elisp-packages?
   (boolean #f)
   "Rebuild Emacs Lisp packages with version of Emacs specified in
PACKAGE field.")
  (server-mode?
   (boolean #f)
   "Create a shepherd service, which starts emacs in a server-mode.")
  ;; (xdg-flavor?
  ;;  (boolean #f)
  ;;  "Place all the configs to @file{$XDG_CONFIG_HOME/emacs}.")
  )


(define ((update-emacs-argument-for-package desired-emacs) p)
  "Set @code{#:emacs} to EMACS-PACKAGE for package P.  To build elisp
packages with proper GNU Emacs version."
  (if (equal?
       (package-build-system p)
       emacs-build-system)
      (package (inherit p)
	       (arguments
		(substitute-keyword-arguments
		 (package-arguments p)
		 ((#:emacs e #f) desired-emacs))))
      p))

(define (emacs-argument-updater desired-emacs)
  "Recursively updates @code{#:emacs} argument for package and all the
inputs."
  (package-mapping (update-emacs-argument-for-package desired-emacs)
		   (lambda (p) #f)))

(define (add-emacs-packages config)
  (let* ((emacs-package  (home-emacs-configuration-package config))
	 (elisp-packages (home-emacs-configuration-elisp-packages config))

	 ;; TODO: build elisp packages with provided emacs version
	 (updated-elisp-packages
	  (if (home-emacs-configuration-rebuild-elisp-packages? config)
	      (map (emacs-argument-updater emacs-package)
		   elisp-packages)
	      elisp-packages)))
    (cons emacs-package
	  updated-elisp-packages)))


(define (add-emacs-shepherd-service config)
  (if (home-emacs-configuration-server-mode? config)
      (list (shepherd-service
             (documentation "Emacs server.  Use @code{emacsclient} to
connect to it.")
             (provision '(emacs-server))
             (start #~(make-forkexec-constructor
                       (list #$(file-append
				(home-emacs-configuration-package config)
				"/bin/emacs") "--fg-daemon")
                       #:log-file (string-append (getenv "XDG_LOG_HOME")
						 "/emacs.log")))
             (stop #~(make-kill-destructor))))
      '()))

(define home-emacs-service-type
  (service-type (name 'home-emacs)
                (extensions
                 (list (service-extension
			home-shepherd-service-type
			add-emacs-shepherd-service)
		       ;; (service-extension
                       ;;  home-files-service-type
                       ;;  add-emacs-configuration)
		       (service-extension
			home-profile-service-type
			add-emacs-packages)))
		;; (compose identity)
		;; (extend home-git-extensions)
                (default-value (home-emacs-configuration))
                (description "Install and configure GNU Emacs.")))
