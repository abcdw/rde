(define-module (gnu home-services shepherd)
  #:use-module (gnu home-services)
  #:use-module (gnu services shepherd)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix i18n)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)

  #:re-export (shepherd-service
	       shepherd-action))


(define (assert-valid-graph services)
  "Raise an error if SERVICES does not define a valid shepherd service graph,
for instance if a service requires a nonexistent service, or if more than one
service uses a given name.

These are constraints that shepherd's 'register-service' verifies but we'd
better verify them here statically than wait until PID 1 halts with an
assertion failure."
  (define provisions
    ;; The set of provisions (symbols).  Bail out if a symbol is given more
    ;; than once.
    (fold (lambda (service set)
            (define (assert-unique symbol)
              (when (set-contains? set symbol)
                (raise (condition
                        (&message
                         (message
                          (format #f (G_ "service '~a' provided more than once")
                                  symbol)))))))

            (for-each assert-unique (shepherd-service-provision service))
            (fold set-insert set (shepherd-service-provision service)))
          (setq 'shepherd)
          services))

  (define (assert-satisfied-requirements service)
    ;; Bail out if the requirements of SERVICE aren't satisfied.
    (for-each (lambda (requirement)
                (unless (set-contains? provisions requirement)
                  (raise (condition
                          (&message
                           (message
                            (format #f (G_ "service '~a' requires '~a', \
which is not provided by any service")
                                    (match (shepherd-service-provision service)
                                      ((head . _) head)
                                      (_          service))
                                    requirement)))))))
              (shepherd-service-requirement service)))

  (for-each assert-satisfied-requirements services))

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

          (format #t "starting services...~%")
          (for-each
	   (lambda (service) (start service))
           '#$(append-map shepherd-service-provision
                          (filter shepherd-service-auto-start?
                                  services)))))

    (scheme-file "shepherd.conf" config)))

(define (launch-shepherd-daemon config)
  "Return commands for on-login script."
  (let* ((shepherd (shepherd-configuration-shepherd config))
	 (services (shepherd-configuration-services config)))
  `("mkdir -p $HOME/.local/var/log/"
    "\n"

    ,(file-append shepherd "/bin/shepherd")
    "\\\n --logfile=$HOME/.local/var/log/shepherd.log"
    "\n"

    ,(file-append shepherd "/bin/herd")
    "\\\n load root " ,(home-shepherd-configuration-file services shepherd)
    "\n")))


(define-public home-shepherd-service-type
  (service-type (name 'home-shepherd)
                (extensions
                 (list (service-extension
			home-run-on-first-login-service-type
                        launch-shepherd-daemon)
		       (service-extension
			home-profile-service-type
			(lambda (config)
			  `(,(shepherd-configuration-shepherd config))))))
		(compose concatenate)
		(extend (lambda (config extra-services)
			  (shepherd-configuration
			   (inherit config)
			   (services (append (shepherd-configuration-services config)
					     extra-services)))))
		(default-value (shepherd-configuration))
                (description "Configures and installs shepherd.")))


