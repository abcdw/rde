(define-module (guix scripts home)
  #:use-module (gnu packages admin)
  #:use-module (gnu home)
  #:use-module (guix derivations)
  #:use-module (guix ui)
  #:use-module (guix grafts)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix scripts package)
  #:use-module (guix scripts build)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (guix-home))

(define %user-module
  ;; Module in which the machine description file is loaded.
  (make-user-module '((gnu home))))

(define %guix-home-profile
  (string-append %profile-directory "/guix-home-profile"))

(define (show-help)
  (display (G_ "Usage: guix home [OPTION ...] ACTION [ARG ...] [FILE]
Build the home environment declared in FILE according to ACTION.
Some ACTIONS support additional ARGS.\n"))
    (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   build            build the home environment without installing anything\n"))
  ;; (show-build-options-help)
  (newline)
  (show-bug-report-information))

(define (verbosity-level opts)
  "Return the verbosity level based on OPTS, the alist of parsed options."
  (or (assoc-ref opts 'verbosity)
      (if (eq? (assoc-ref opts 'action) 'build)
          2 1)))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix show")))
        (option '(#\v "verbosity") #t #f
                (lambda (opt name arg result)
                  (let ((level (string->number* arg)))
                    (alist-cons 'verbosity level
                                (alist-delete 'verbosity result)))))
        (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)))

(define %default-options
  `((substitutes? . #t)
    (graft? . #t)
    (debug . 0)))


(define* (perform-action action he
			 #:key
                         dry-run?
			 derivations-only?
                         use-substitutes?)
  "Perform ACTION for home environment. "

  (define println
    (cut format #t "~a~%" <>))

  (mlet* %store-monad
      ((he-drv   (home-environment-derivation he))
       (drvs     (mapm/accumulate-builds lower-object (list he-drv)))
       (%        (if derivations-only?
                     (return
		      (for-each (compose println derivation-file-name) drvs))
                     (built-derivations drvs)))

       (he-profile -> (derivation->output-path he-drv)))
    (if (or dry-run? derivations-only?)
	(return #f)
        (begin
          (for-each (compose println derivation->output-path) drvs)

          (case action
	    ((reconfigure)
	     (let* ((number (generation-number %guix-home-profile))
                    (generation (generation-file-name
				 %guix-home-profile (+ 1 number))))
	       (switch-symlinks generation he-profile)
	       (switch-symlinks %guix-home-profile generation)
	       ;; (println generation)
	       (return #f)
	       ))
            (else
             (newline)
	     (return he-profile)))))))

(define (process-action action args opts)
  "Process ACTION, a sub-command, with the arguments are listed in ARGS.
ACTION must be one of the sub-commands that takes a home environment
declaration as an argument (a file name.)  OPTS is the raw alist of options
resulting from command-line parsing."
  (define (ensure-home-environment file-or-exp obj)
    (unless (home-environment? obj)
      (leave (G_ "'~a' does not return a home environment ~%")
             file-or-exp))
    obj)

  (let* ((file   (match args
                   (() #f)
                   ((x . _) x)))
         (expr   (assoc-ref opts 'expression))
         (system (assoc-ref opts 'system))

         (home-environment
          (ensure-home-environment
           (or file expr)
           (cond
            ((and expr file)
             (leave
              (G_ "both file and expression cannot be specified~%")))
            (expr
             (read/eval expr))
            (file
             (load* file %user-module
                    #:on-error (assoc-ref opts 'on-error)))
            (else
             (leave (G_ "no configuration specified~%"))))))

         (dry?        (assoc-ref opts 'dry-run?)))

    (with-store store
      (set-build-options-from-command-line store opts)
      (with-build-handler (build-notifier #:use-substitutes?
                                          (assoc-ref opts 'substitutes?)
                                          #:verbosity
                                          (verbosity-level opts)
                                          #:dry-run?
                                          (assoc-ref opts 'dry-run?))

        (run-with-store store
          (mbegin %store-monad
	    (set-guile-for-build (default-guile))

	    (case action
              (else
               ;; (unless (eq? action 'build)
               ;;   (warn-about-old-distro #:suggested-command
               ;;                          "guix home reconfigure"))
               (perform-action action home-environment
                               #:dry-run? dry?
                               #:derivations-only? (assoc-ref opts 'derivations-only?)
                               #:use-substitutes? (assoc-ref opts 'substitutes?))
	       ))))))
    (warn-about-disk-space)))


(define (process-command command args opts)
  "Process COMMAND, one of the 'guix home' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (case command
    ;; The following commands do not need to use the store, and they do not need
    ;; an operating home environment file.
    ;; ((search)
    ;;  (apply (resolve-subcommand "search") args))
    (else (process-action command args opts))))

(define-command (guix-home . args)
  (synopsis "build and deploy home environments")

  (define (parse-sub-command arg result)
    ;; Parse sub-command ARG and augment RESULT accordingly.
    (if (assoc-ref result 'action)
        (alist-cons 'argument arg result)
        (let ((action (string->symbol arg)))
          (case action
            ((build
	      reconfigure
	      extension-graph shepherd-graph
	      list-generations describe
	      delete-generations roll-back
	      switch-generation search)
             (alist-cons 'action action result))
            (else (leave (G_ "~a: unknown action~%") action))))))

  (define (match-pair car)
    ;; Return a procedure that matches a pair with CAR.
    (match-lambda
      ((head . tail)
       (and (eq? car head) tail))
      (_ #f)))

  (define (option-arguments opts)
    ;; Extract the plain arguments from OPTS.
    (let* ((args   (reverse (filter-map (match-pair 'argument) opts)))
           (count  (length args))
           (action (assoc-ref opts 'action))
           (expr   (assoc-ref opts 'expression)))
      (define (fail)
        (leave (G_ "wrong number of arguments for action '~a'~%")
               action))

      (unless action
        (format (current-error-port)
                (G_ "guix home: missing command name~%"))
        (format (current-error-port)
                (G_ "Try 'guix home --help' for more information.~%"))
        (exit 1))

      (case action
        ((build reconfigure)
         (unless (or (= count 1)
                     (and expr (= count 0)))
           (fail)))
        ((init)
         (unless (= count 2)
           (fail))))
      args))

  (with-error-handling
    (let* ((opts     (parse-command-line args %options
					 (list %default-options)
                                         #:argument-handler
                                         parse-sub-command))
           (args     (option-arguments opts))
           (command  (assoc-ref opts 'action)))
      ;; (pretty-print opts)
      ;; (pretty-print args)
      ;; (pretty-print command)
      ;; (pretty-print (assoc-ref opts 'graft?))
      (parameterize ((%graft? (assoc-ref opts 'graft?)))
        (with-status-verbosity (verbosity-level opts)
          (process-command command args opts))))))
