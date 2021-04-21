(define-module (gnu home-services shepherd)
  #:use-module (gnu home-services)
  #:use-module (gnu packages admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix records)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)

  #:re-export (shepherd-service
	       shepherd-action))

(define-record-type* <home-shepherd-configuration>
  home-shepherd-configuration make-home-shepherd-configuration
  home-shepherd-configuration?
  (shepherd home-shepherd-configuration-shepherd
            (default shepherd)) ; package
  (services home-shepherd-configuration-services
            (default '())))

(define (home-shepherd-configuration-file services shepherd)
  "Return the shepherd configuration file for SERVICES.  SHEPHERD is used
as shepherd package."
  (assert-valid-graph services)

  (let ((files (map shepherd-service-file services))
	;; TODO: Add compilation of services, it can improve start
	;; time.
	;; (scm->go (cute scm->go <> shepherd))
	)
    (define config
      #~(begin
          (use-modules (srfi srfi-34)
                       (system repl error-handling))

          ;; Specify the default environment visible to all the services.
          ;; Without this statement, all the environment variables of PID 1
          ;; are inherited by child services.
          ;; (default-environment-variables
          ;;   '("PATH=/run/current-system/profile/bin"))

          ;; (default-pid-file-timeout 5)

	  (apply
	   register-services
	   (map
	    (lambda (file) (load file))
	    '#$files))
	  (action 'root 'daemonize)
          (format #t "starting services...~%")
          (for-each
	   (lambda (service) (start service))
           '#$(append-map shepherd-service-provision
                          (filter shepherd-service-auto-start?
                                  services)))))

    (scheme-file "shepherd.conf" config)))

(define (launch-shepherd-gexp config)
  (let* ((shepherd (home-shepherd-configuration-shepherd config))
	 (services (home-shepherd-configuration-services config)))
    #~(begin
	(system*
	 #$(file-append shepherd "/bin/shepherd")
	 "--logfile"
	 (string-append
	  (or (getenv "XDG_LOG_HOME")
	      (format #f "~s/.local/var/log" (getenv "HOME")))
	   "/shepherd.log")
	 "--config"
	 #$(home-shepherd-configuration-file services shepherd)))))

(define (reload-configuration-gexp config)
  (let* ((shepherd (home-shepherd-configuration-shepherd config))
	 (services (home-shepherd-configuration-services config)))
    #~(system*
       #$(file-append shepherd "/bin/herd")
       "load" "root"
       #$(home-shepherd-configuration-file services shepherd))))

(define (ensure-shepherd-gexp config)
  #~(if (file-exists?
	 (string-append
	  (or (getenv "XDG_RUNTIME_DIR")
	      (format #f "/run/user/~s" (getuid)))
	  "/shepherd/socket"))
	#$(reload-configuration-gexp config)
	#$(launch-shepherd-gexp config)))

(define-public home-shepherd-service-type
  (service-type (name 'home-shepherd)
                (extensions
                 (list (service-extension
			home-run-on-first-login-service-type
                        launch-shepherd-gexp)
		       (service-extension
			home-run-on-reconfigure-service-type
			ensure-shepherd-gexp)
		       (service-extension
			home-profile-service-type
			(lambda (config)
			  `(,(home-shepherd-configuration-shepherd config))))))
		(compose concatenate)
		(extend
		 (lambda (config extra-services)
		   (home-shepherd-configuration
		    (inherit config)
		    (services
		     (append (home-shepherd-configuration-services config)
			     extra-services)))))
		(default-value (home-shepherd-configuration))
                (description "Configures and installs user's shepherd.")))


