(define-module (gnu home-services configuration)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)

  #:export (filter-configuration-fields

            interpose
            list-of

            list-of-strings?
            alist?
            string-or-gexp?
	    serialize-string-or-gexp
	    text-config?
            serialize-text-config))

(define* (filter-configuration-fields configuration-fields fields
				      #:optional negate?)
  "Retrieve the fields listed in FIELDS from CONFIGURATION-FIELDS.
If NEGATE? is @code{#t}, retrieve all fields except FIELDS."
  (filter (lambda (field)
            (let ((member? (member (configuration-field-name field) fields)))
              (if (not negate?) member? (not member?))))
          configuration-fields))


(define* (interpose ls  #:optional (delimiter "\n") (grammar 'infix))
  "Same as @code{string-join}, but without join and string, returns an
DELIMITER interposed LS.  Support 'infix and 'suffix GRAMMAR values."
  (when (not (member grammar '(infix suffix)))
    (raise
     (formatted-message
      (G_ "The GRAMMAR value must be 'infix or 'suffix, but ~a provided.")
      grammar)))
  (fold-right (lambda (e acc)
		(cons e
		      (if (and (null? acc) (eq? grammar 'infix))
			  acc
			  (cons delimiter acc))))
	      '() ls))

(define (list-of pred?)
  "Return a procedure that takes a list and check if all the elements of
the list result in @code{#t} when applying PRED? on them."
    (lambda (x)
      (if (list? x)
          (every pred? x)
          #f)))


(define list-of-strings?
  (list-of string?))

(define alist? list?)

(define (string-or-gexp? sg) (or (string? sg) (gexp? sg)))
(define (serialize-string-or-gexp field-name val) "")

(define (text-config? config)
  (and (list? config) (every string-or-gexp? config)))
(define (serialize-text-config field-name val)
  #~(string-append #$@(interpose val "\n" 'suffix)))
