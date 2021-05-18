(define-module (rde features)
  #:use-module (guix records)
  #:use-module (guix ui)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu home)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)

  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 pretty-print)

  #:export (rde-config
	    rde-config-home-environment
	    rde-config-operating-system

	    pretty-print-rde-config

	    feature
	    make-feature-values
	    require-value
	    get-value

	    ensure-pred
	    throw-message

	    bare-bone-os))

(define (alist? lst)
  (every pair? lst))

(define (services-getter? fn)
  (procedure? fn))

(define-configuration feature
  (name
   (symbol)
   "Name for feature to identify it in debug messages.")
  (values
   (alist '())
   "List of pairs avaliable to share across services.")
  (home-services-getter
   (services-getter (const '()))
   "Function taking one argument (@code{values}) and returning a list
of services.  Service can be either @code{service?} or
@code{#f}. Resulting list will be appended to home environment.")
  (system-services-getter
   (services-getter (const '()))
   "Function taking one argument (@code{values}) and returning a list
of services.  Service can be either @code{service?} or
@code{#f}. Resulting list will be appended to operating system.")
  (no-serialization))

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
  (home-environment
   rde-config-home-environment
   (thunked)
   (default
     (get-home-environment this-rde-config)))

  (initial-os
   rde-config-initial-os
   (default bare-bone-os))
  (system-services
   rde-config-system-services
   (thunked)
   (default
     (fold-system-services
      (rde-config-features this-rde-config)
      (rde-config-values this-rde-config))))
  (operating-system
   rde-config-operating-system
   (thunked)
   (default
     (get-operating-system this-rde-config))))


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

(define (fold-some-services features values services-getter)
  (filter service?
	  (apply append
		 (map (lambda (f)
			((services-getter f) values))
		      features))))

(define (fold-home-services features values)
  "Generates a list of home-services from FEATURES by passing VALUES
to each home-services-getter function."
  (fold-some-services features values feature-home-services-getter))

(define (fold-system-services features values)
  "Generates a list of system-services from FEATURES by passing VALUES
to each system-services-getter function."
  (fold-some-services features values feature-system-services-getter))


(define* (get-value key config #:optional default-value)
  "Get KEY from rde-config values."
  (let ((handle (hash-get-handle (rde-config-values config) key)))
    (if handle
	(cdr handle)
	default-value)))

(define* (require-value key config #:optional (additional-msg ""))
  (throw-message
   (not (hash-get-handle (rde-config-values config) key))
   (format #f "Value ~a is not provided by any feature.\n~a"
	   key additional-msg)))

(define (get-home-environment config)
  (require-value 'home-directory config
		 "You may want to use feature-user-info.")
  (home-environment
   (home-directory (get-value 'home-directory config))
   (services (rde-config-home-services config))))

(define bare-bone-os
  (operating-system
   (host-name "antelope")
   (timezone  "Europe/Paris")
   (locale  "en_US.utf8")
   (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
		(target "/boot/efi")))
   (file-systems %base-file-systems)))

(define (get-operating-system config)
  (let* ((initial-os (rde-config-initial-os config))

	 (host-name       (get-value
			   'host-name config
			   (operating-system-host-name initial-os)))
	 (timezone        (get-value
			   'timezone config
			   (operating-system-timezone initial-os)))
	 (keyboard-layout (get-value
			   'keyboard-layout config
			   (operating-system-keyboard-layout initial-os)))
	 (bootloader-cfg  (get-value
			   'bootloader-configuration config
			   (operating-system-bootloader initial-os)))
	 (bootloader      (bootloader-configuration
			   (inherit bootloader-cfg)
			   (keyboard-layout keyboard-layout)))
	 (mapped-devices  (get-value
			   'mapped-devices config
			   (operating-system-mapped-devices initial-os)))
	 (file-systems    (get-value
			   'file-systems config
			   (operating-system-file-systems initial-os)))

	 (user-name       (get-value 'user-name config))
	 (full-name       (get-value 'full-name config ""))
	 (home-directory  (get-value
			   'home-directory config
			   (string-append "/home/" (or user-name "user"))))
	 (login-shell     (get-value 'login-shell config (default-shell)))
	 (user-password   (get-value 'user-initial-password-hash config #f))

	 (users           (if user-name
			      (cons
			       (user-account
				(name user-name)
				(comment full-name)
				(password user-password)
				(home-directory home-directory)
				(shell login-shell)
				(group "users")
				(supplementary-groups '("wheel" "netdev"
							"audio" "video")))
			       %base-user-accounts)
			      (operating-system-users initial-os)))

	 ;; NOTE: Can be very frustrating, when a dozen of features
	 ;; doesn't provide any system services and next added feature
	 ;; will provide system service and initial-os user-services
	 ;; will be wiped.
	 (system-services (rde-config-system-services config))
	 (services        (if (null? system-services)
			      (operating-system-user-services initial-os)
			      system-services)))

    (operating-system
      (inherit initial-os)
      (host-name host-name)
      (timezone timezone)
      (bootloader bootloader)
      (mapped-devices mapped-devices)
      (file-systems file-systems)
      (users users)
      (keyboard-layout keyboard-layout)
      (services services))))

(define (pretty-print-rde-config config)
  (use-modules (gnu services)
	       (ice-9 pretty-print))
  (pretty-print
   (rde-config-values-alist
    config))
  (pretty-print
   (map service-kind
	(rde-config-home-services
	 config)))
  (pretty-print
   (map service-kind
	(rde-config-system-services
	 config))))

;; (pretty-print-rde-config
;;  (rde-config
;;   (features my-features)))

;; (rde-config-home-environment my-cfg)
