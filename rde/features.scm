(define-module (rde features)
  #:use-module (guix records)
  #:use-module (guix ui)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-35))

(define (alist? lst)
  (every pair? lst))

(define (list-of-maybe-services-function? fn)
  (procedure? fn))

(define serialize-alist (const ""))
(define serialize-list-of-maybe-services-function (const ""))

(define-configuration feature
  (name
   (symbol)
   "Name for feature to identify it in debug messages."
   no-serialization)
  (values
   (alist '())
   "List of pairs avaliable to share across services.")
  (get-home-services
   (list-of-maybe-services-function (const '()))
   "Function returning a list of maybe-services.  Service can be
either @code{service?} or @code{#f}. Will go to home environment.")
  (get-system-services
   (list-of-maybe-services-function (const '()))
   "Function returning a list of maybe-services.  Service can be
either @code{service?} or @code{#f}. Will go to operating system."))

(define-record-type* <rde-config> rde-config
  make-rde-config
  rde-config?
  this-rde-config

  (features rde-config-features
	    (default '()))

  (values
   rde-config-values
   (thunked)
   (default
     (fold-values
      (rde-config-features this-rde-config))))

  (values-alist
   rde-config-values-alist
   (thunked)
   (default
     ;; Doesn't ensure that there is no duplicates in values.  This
     ;; field is inteded for debugging/development purposes.
     (apply append
	    (map feature-values
		 (rde-config-features this-rde-config)))))
  
  (home-services
   rde-config-home-services
   (thunked)
   (default
     (fold-home-services
      (rde-config-features this-rde-config)
      (rde-config-values this-rde-config))))

  (system-services
   rde-config-system-services
   (thunked)
   (default
     (fold-system-services
      (rde-config-features this-rde-config)
      (rde-config-values this-rde-config))))

  (home-environment
   rde-config-home-environment
   (thunked)
   (default
     (get-home-environment this-rde-config))))


(define-syntax ensure-pred
  (syntax-rules ()
    ((ensure-pred pred field)
     (when (not (pred field)) 
       (raise (condition
               (&message
		(message
		 (format
		  #f (G_ "~a: The predicate '~a' is not satisfied with value '~a'.")
		  'field
		  (procedure-name pred)
		  field)))))))))

(define-syntax throw-message
  (syntax-rules ()
    ((throw-message pred msg)
     (when pred 
       (raise (condition
               (&message
		(message
		 msg))))))))

(define-syntax make-feature-values
  (syntax-rules ()
    ((provide-values field ...)
     `((field . ,field) ...))))

(define* (f-user-info
	  #:key user-name full-name email
	  (home-directory (format #f "/home/~a" user-name)))
  "Provides basic information about user for all features."
  (ensure-pred string? user-name)
  (ensure-pred string? full-name)
  (ensure-pred string? email)
  
  (feature
   (name 'user-info)
   (values (make-feature-values
	    user-name full-name email home-directory))))

(use-modules (gnu home-services gnupg))
(define* (f-gnupg
	  #:key gpg-primary-key
	  (gpg-ssh-agent? #t)
	  (pinentry-flavor 'qt))
  "Sets up gnupg, if SSH-AGENT? specified also sets up gpg's ssh-agent
and provides GPG-PRIMARY-KEY value for other features."

  (ensure-pred string? gpg-primary-key)
  (ensure-pred boolean? gpg-ssh-agent?)

  (define (home-gnupg-services values)
    "Return a list of home-services, required for gnupg to operate."
    (list
     (service home-gnupg-service-type
	      (home-gnupg-configuration
	       (gpg-config
		(home-gpg-configuration
		 (extra-config
		  '((keyid-format . long)
		    (with-subkey-fingerprint . #t)
		    (keyserver . "hkps://keys.openpgp.org")))))
	       (gpg-agent-config
		(home-gpg-agent-configuration
		 (ssh-agent? gpg-ssh-agent?)
		 (pinentry-flavor pinentry-flavor)))))))
    
  (feature
   (name 'gnupg)
   (values (append
	    (make-feature-values gpg-primary-key gpg-ssh-agent?)
	    (if gpg-ssh-agent?
		'((ssh-agent? . #t))
		'())))
   (get-home-services home-gnupg-services)))

(define my-features
  (list
   (f-user-info #:user-name "bob"
		#:full-name "Andrew Tropin"
		#:email "andrew@trop.in")
   (f-gnupg #:gpg-primary-key "74830A276C328EC2")
   ;; (f-keyboard #:layout "us,ru" #:options "dvorak,")
   ;; (f-sway)
   ;; (f-projects #:directory "work")
   ;; (f-emacs #:server? #t)
   ;; (f-emacs-rss)
   ;; (f-emacs-org-roam)
   ))

(use-modules (ice-9 hash-table))
(use-modules (ice-9 pretty-print))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(define (fold-values features)
  (let ((f-values (apply append (map feature-values features))))
    (fold
     (lambda (feature acc)
       (fold
	(lambda (x acc)
	  (throw-message
	   (hash-get-handle acc (car x))
	   (format #f (G_ "Duplicate entry came from ~a feature:\n~a\n
The previous value was:\n~a\n")
		   (feature-name feature)
		   x
		   (hash-get-handle acc (car x))))
	  (hash-set! acc (car x) (cdr x))
	  acc)
	acc
	(feature-values feature))
       acc)
     (make-hash-table)
     features)))

(define (print-values features)
  (hash-for-each-handle pretty-print
			(fold-values features)))

(define (fold-some-services features values getter)
  (filter service?
	  (apply append
		 (map (lambda (f)
			((getter f) values))
		      features))))

(define (fold-home-services features values)
  "Generates a list of home-services from FEATURES by passing VALUES
to each get-home-services function."
  (fold-some-services features values feature-get-home-services))

(define (fold-system-services features values)
  "Generates a list of system-services from FEATURES by passing VALUES
to each get-system-services function."
  (fold-some-services features values feature-get-system-services))

(define (get-home-environment rde-config)
  (home-environment
   (services (rde-config-home-services rde-config))))

(define (pretty-print-rde-config config)
  (use-modules (gnu services)
	       (ice-9 pretty-print))
  (pretty-print
   (rde-config-values-alist
    config))
  (pretty-print
   (map service-kind
	(rde-config-home-services
	 config))))

(pretty-print-rde-config
 (rde-config
  (features my-features)))

(rde-config-home-environment
 (rde-config
  (features my-features)))

;; (use-modules (srfi srfi-69))
;; alist->hash-table

;; (general-info)

;; (feature
;;  (values '((user-name . "abcdw"))))
