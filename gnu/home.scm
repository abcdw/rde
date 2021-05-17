(define-module (gnu home)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu home-services symlink-manager)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services xdg)
  #:use-module (gnu home-services fontutils)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)

  #:export (home-environment
	    home-environment?
	    this-home-environment

	    home-environment-derivation
	    home-environment-home-directory
	    home-environment-user-services
	    home-environment-essential-services
	    home-environment-services
	    home-environment-symlink-name
	    home-environment-symlink-path
	    home-environment-location

	    home-environment-with-provenance))

(define-record-type* <home-environment> home-environment
  make-home-environment
  home-environment?
  this-home-environment

  (packages home-environment-packages             ; list of (PACKAGE OUTPUT...)
            (default '()))

  (essential-services home-environment-essential-services ; list of services
                      (thunked)
                      (default (home-environment-default-essential-services
                                this-home-environment)))
  (services home-environment-user-services
	    (default '()))

  ;; (user-name home-environment-user-name)
  (home-directory home-environment-home-directory)
  (symlink-name home-environment-symlink-name
		(default ".guix-home"))

  (symlink-path home-environment-symlink-path (thunked)
		(default
		  (string-append
		   (home-environment-home-directory this-home-environment)
		   "/"
		   (home-environment-symlink-name this-home-environment))))

  (location home-environment-location             ; <location>
            (default (and=> (current-source-location)
                            source-properties->location))
            (innate)))


(define (home-environment-default-essential-services he)
  "Return the list of essential services for home environment."

  (define (update-environment-gexp home-environment-path)
    "Return G-Expression, which sets environment variable values
according to the content of @command{setup-environment} script."
    #~(let* ((port   ((@@ (ice-9 popen) open-input-pipe)
		      (string-append "source " #$home-environment-path
				     "/setup-environment && env")))
	     (result ((@@ (ice-9 rdelim) read-delimited) "" port))
	     (vars (map (lambda (x) (string-split x #\=))
			((@@ (srfi srfi-1) remove)
			 string-null? (string-split result #\newline)))))
	(close-port port)
	(map (lambda (x)
	       (setenv (car x) (cadr x)))
	     vars)))

  (let* ((he-path (home-environment-symlink-path he)))
    (list
     (service home-run-on-first-login-service-type)

     (service home-fontconfig-service-type he-path)
     (service home-symlink-manager-service-type)
     (service home-activation-service-type
	      (update-environment-gexp he-path))

     ;; It should be safe to use symlink-path as
     ;; GUIX_HOME_DIRECTORY, however
     ;; /var/guix/profiles/per-user/... is another option
     (service home-environment-variables-service-type
	      `(("GUIX_HOME_DIRECTORY" . ,he-path)))

     ;; Make guix aware of `guix home` after first reconfigure, this
     ;; declaration must go before xdg-base-dirs.  Potentially
     ;; dangerous "fix", it makes possible for malicious channel
     ;; expose it's own guix subcommands.
     ;; TODO: Remove it once upstreamed.
     (simple-service
      'make-guix-aware-of-guix-home-subcomand
      home-environment-variables-service-type
      '(("GUILE_LOAD_PATH" .
	 "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0\
:$GUILE_LOAD_PATH")
	("GUILE_LOAD_COMPILED_PATH" .
	 "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache\
:$GUILE_LOAD_COMPILED_PATH")))
     (service home-xdg-base-directories-service-type)

     (service home-shell-profile-service-type
	      (home-shell-profile-configuration
	       (he-symlink-path he-path)))
     (service home-service-type)
     (service home-profile-service-type (home-environment-packages he)))))

(define* (home-environment-services he)
  "Return all the services of home environment."
  (instantiate-missing-services
   (append (home-environment-user-services he)
           (home-environment-essential-services he))))

(define* (home-environment-derivation he)
  "Return a derivation that builds OS."
  (let* ((services         (home-environment-services he))
         (home (fold-services services
			      #:target-type home-service-type)))
    (service-value home)))

(define* (home-environment-with-provenance he config-file)
  "Return a variant of HE that stores its own provenance information,
including CONFIG-FILE, if available.  This is achieved by adding an instance
of HOME-PROVENANCE-SERVICE-TYPE to its services."
  (home-environment
    (inherit he)
    (services (cons (service home-provenance-service-type config-file)
                    (home-environment-user-services he)))))
