(define-module (gnu home-services-utils)
  #:use-module (gnu services configuration)
  #:use-module (guix ui)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix monads)

  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-26)

  #:export (slurp-file-gexp

	    alist-entry->mixed-text
            boolean->yes-or-no
            list->human-readable-list
            maybe-object->string
            filter-configuration-fields
            generic-serialize-alist-entry
            generic-serialize-alist

	    interpose
	    string-or-gexp?
	    serialize-string-or-gexp
	    text-config?
	    serialize-text-config
            symbol->snake-case
            ini-config?
            generic-serialize-ini-config))

;;;
;;; User's utils.
;;;

(define (slurp-file-gexp file)
  "Returns a gexp, which reads all the content of the FILE and returns
it as a string.  FILE must be a file-like object."
  (when (not (file-like? file))
    (raise (formatted-message
            (G_ "~a is not a file-like object.")
            file)))
  #~(call-with-input-file #$file
	(@@ (ice-9 textual-ports) get-string-all)))


;;;
;;; Configuration related helpers.
;;;

(define* ((alist-entry->mixed-text prefix sep #:optional (suffix "\n"))
	  alist-entry)
  "Create a list from ALIST-ENTRY, which can be used with
@code{mixed-text-file} for example to create key-value configuration
file or shell script.

PREFIX is the string to prefix the key-value pair with.  For example,
@code{\"export\"} will return @code{'(\"export\" KEY SEP VALUE)},
where KEY is the first element of ALIST-ENTRY, and VALUE is the second
element of ALIST-ENTRY.

SEP is the separator between the key and the value.

SUFFIX is the optional argument, default to newline.

Different things will happen depending on the value of VALUE:
@itemize @bullet
@item If VALUE is #f, ignore everything in the entry and just return
an empty list.

@item If VALUE is #t or not provided (empty list), ignore the VALUE
and SEP and just return a list of PREFIX and KEY followed by a
SUFFIX.

@item If VALUE is a flat list, it will get added to the resulting
list. If not flat the exception will be raised.

@item If VALUE is not a list (string, file-like object, etc), return a
list of PREFIX, KEY, SEP and VALUE followed by a SUFFIX.

The following code
@lisp
((alist-entry->mixed-text \"export \" \"=\") '(\"EDITOR\" . \"emacsclient\"))
((alist-entry->mixed-text \"export \" \"=\") '(\"EDITOR\" . #t))
((alist-entry->mixed-text \"export \" \"=\") '(\"EDITOR\"))
((alist-entry->mixed-text \"export \" \"=\") '(\"EDITOR\" . #f))
((alist-entry->mixed-text \"export \" \"=\") '(\"EDITOR\" . (\"emacsclient\" \"vim\")))
@end lisp

would yield

@example
(\"export \" \"EDITOR\" \"=\" \"emacsclient\" \"\n\")
(\"export \" \"EDITOR\" \"\n\")
(\"export \" \"EDITOR\" \"\n\")
()
(\"export \" \"EDITOR\" \"=\" \"emacsclient\" \"vim\" \"\n\")
@end example"
  (define (not-alist-entry-error)
    (raise (formatted-message
            (G_ "~a has to be an association list entry")
            alist-entry)))
  (match alist-entry
    ((key . value)
     (let* ((values (cond
		    ((eq? value #f)
		     #f)
		    ((or (eq? value #t) (null? value))
		     '(""))
		    ((list? value)
		     (if (any list? value)
			 (raise (formatted-message
				 (G_ "~a is not a flat list")
				 value))
			 value))
		    (else
		     (list value))))
           (sep (if (eq? values '(""))
                    ""
                    sep)))
       (if values
           `(,prefix ,key ,sep ,@values ,suffix)
           '())))
    (_ (not-alist-entry-error))))

(define* (boolean->yes-or-no bool #:optional (capitalize? #f))
  "Convert a boolean BOOL to \"yes\" or \"no\".
Setting CAPITALIZE? to @code{#t} will capitalize the word, it is set to
@code{#f} by default."
  (let ((word (if (eq? bool #t) "yes" "no")))
    (if capitalize?
        (string-capitalize word)
        word)))

(define (maybe-object->string object)
  "Like @code{object->string} but don't do anyting if OBJECT already is
a string."
  (if (string? object)
      object
      (object->string object)))

(define* (list->human-readable-list lst
                                    #:key
                                    (cumulative? #f)
                                    (proc identity))
  "Turn a list LST into a sequence of terms readable by humans.
If CUMULATIVE? is @code{#t}, use ``and'', otherwise use ``or'' before
the last term.

PROC is a procedure to apply to each of the elements of a list before
turning them into a single human readable string.

@example
(list->human-readable-list '(1 4 9) #:cumulative? #t #:proc sqrt)
@result{} \"1, 2, and 3\"
@end example

yields:"
  (let* ((word (if cumulative? "and " "or "))
         (init (append (drop-right lst 1))))
    (format #f "~a" (string-append
                     (string-join
                      (map (compose maybe-object->string proc) init)
                      ", " 'suffix)
                     word
                     (maybe-object->string (proc (last lst)))))))

(define* (filter-configuration-fields configuration-fields fields
				      #:optional negate?)
  "Retrieve the fields FIELDS from CONFIGURATION.
If NEGATE? is @code{#t}, retrieve the FIELDS that are not in CONFIGURATION."
  (filter (lambda (field)
            (let ((membership? (member (configuration-field-name field)
                                       fields)))
              (if (not negate?)
                  membership?
                  (not membership?))))
          configuration-fields))

;; Snake case: <https://en.wikipedia.org/wiki/Snake_case>
(define* (symbol->snake-case symbol #:optional (style 'lower) (prefix ""))
  "Convert the symbol SYMBOL to the equivalent string in ``snake
case''.  STYLE can be three `@code{lower}', `@code{upper}', or
`@code{capitalize}', defaults to `@code{lower}'.  PREFIX is a string
to prefix the snake case version with.  Useful for converting symbols
to environment variables.

@example
(symbol->snake-case 'variable-name 'upper \"$\")
@result{} \"$VARIABLE_NAME\" @end example"
  (if (not (member style '(lower upper capitalize)))
      (error 'invalid-style (format #f "~a is not a valid style" style))
      (let ((stringified (symbol->string symbol)))
        (string-append prefix
                       (string-replace-substring
                        (cond
                         ((equal? style 'lower) stringified)
                         ((equal? style 'upper) (string-upcase stringified))
                         (else (string-capitalize stringified)))
                        "-" "_")))))

;;;
;;; Serializers.
;;;

(define ((generic-serialize-alist-entry serialize-field) entry)
  "Apply the SERIALIZE-FIELD procedure on the field and value of ENTRY."
  (match entry
    ((field . val) (serialize-field field val))))

(define (generic-serialize-alist combine serialize-field fields)
  "Generate a configuration from an association list FIELDS.

SERIALIZE-FIELD is a procedure that takes two arguments, it will be
applied on the fields and values of FIELDS using the
@code{generic-serialize-alist-entry} procedure.

COMBINE is a procedure that takes one or more arguments and combines
all the alist entries into one value, @code{string-append} or
@code{append} are usually good candidates for this.

See the @code{serialize-alist} procedure in `@code{(gnu home-services
version-control}' for an example usage.)}"
  (apply combine
         (map (generic-serialize-alist-entry serialize-field) fields)))

(define (string-or-gexp? sg) (or (string? sg) (gexp? sg)))
(define (serialize-string-or-gexp field-name val) "")

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

(define (text-config? config)
  (and (list? config) (every string-or-gexp? config)))
(define (serialize-text-config field-name val)
  #~(string-append #$@(interpose val "\n" 'suffix)))

(define ini-config? list?)
(define (generic-serialize-ini-config-section section proc)
  "Format a section from SECTION for an INI configuration.
Apply the procedure PROC on SECTION after it has been converted to a string"
  (format #f "[~a]\n" (proc (maybe-object->string section))))

(define* (generic-serialize-ini-config #:key
                                       (format-section identity)
                                       (combine string-append)
                                       serialize-field
                                       fields)
  "Create an INI configuration from nested lists FIELDS.  This uses
@code{generic-serialize-ini-config-section} and @{generic-serialize-alist} to
serialize the section and the association lists, respectively.

@example
(generic-serialize-ini-config
 #:serialize-field (lambda (a b) (format #f \"~a = ~a\n\" a b))
 #:format-section string-capitalize
 #:fields '((application ((key . value)))))
@end example

@result{} \"[Application]\nkey = value\n\""
  (string-join
   (map (match-lambda
          ((section alist)
           (string-append
            (generic-serialize-ini-config-section section format-section)
            (generic-serialize-alist combine serialize-field alist))))
        fields)
   "\n"))
