(define-module (guix scripts home)
  #:use-module (guix ui)
  #:use-module (guix grafts)
  #:use-module (guix scripts)
  #:use-module (guix scripts package)
  #:use-module ((guix scripts build)
                #:select (%standard-build-options))
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix scripts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (guix-home))

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
  '((verbosity . #f)
    (graft? . #t)))

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

  ;; (define (handle-argument arg result)
  ;;   ;; Treat all non-option arguments as regexps.
  ;;   (cons `(query show ,arg)
  ;;         result))

  ;; (define opts
  ;;   (args-fold* args %options
  ;;               (lambda (opt name arg . rest)
  ;;                 (leave (G_ "~A: unrecognized option~%") name))
  ;;               handle-argument
  ;;               '()))

  ;; (unless (assoc-ref opts 'query)
  ;;   (leave (G_ "missing arguments: no package to show~%")))

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
                (G_ "guix system: missing command name~%"))
        (format (current-error-port)
                (G_ "Try 'guix system --help' for more information.~%"))
        (exit 1))

      (case action
        ((build container vm vm-image disk-image docker-image reconfigure)
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
      (pretty-print opts)
      (pretty-print args)
      (pretty-print command)
      (pretty-print (assoc-ref opts 'graft?))
      (parameterize ((%graft? (assoc-ref opts 'graft?)))
        (with-status-verbosity (verbosity-level opts)
          ;; (process-command command args opts)
	  (pretty-print command)))))
    
  (display "hi there!\n")
  (display args))
