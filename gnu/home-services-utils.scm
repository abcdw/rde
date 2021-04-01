(define-module (gnu home-services-utils)
  #:use-module (gnu services configuration)
  #:use-module (guix ui)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix monads)

  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-26)

  #:export (alist-entry->mixed-text
            boolean->yes-or-no
	    text-file->gexp
            list->human-readable-list
            maybe-object->string
            filter-configuration-fields
            generic-serialize-alist-entry
            generic-serialize-alist
	    string-or-gexp?
	    serialize-string-or-gexp
	    text-config?
	    serialize-text-config))


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

(define (text-file->gexp path)
  #~(call-with-input-file
	#$(local-file path (string-trim (basename path) #\.))
	(@@ (ice-9 textual-ports) get-string-all)))

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

(define* (filter-configuration-fields configuration fields #:optional negate?)
  "Retrieve the fields FIELDS from CONFIGURATION.
If NEGATE? is @code{#t}, retrieve the FIELDS that are not in CONFIGURATION."
  (filter (lambda (field)
            (let ((membership? (member (configuration-field-name field)
                                       fields)))
              (if (not negate?)
                  membership?
                  (not membership?))))
          configuration))



;;;
;;; Serializers.
;;;

(define ((generic-serialize-alist-entry serialize-field) entry)
  "Apply the SERIALIZE-FIELD procedure on the field and value of ENTRY."
  (match entry
    ((field . val) (serialize-field field val))))

(define ((generic-serialize-alist combine serialize-field) field-name fields)
  "Apply the SERIALIZE-FIELD procedure on the fields and values of FIELDS.
Apply the COMBINE procedure to combine all the alist entries into one
value, @code{string-append} or @code{append} are usually good
candidates for this."
  (apply combine
         (map (generic-serialize-alist-entry serialize-field) fields)))

(define (string-or-gexp? sg) (or (string? sg) (gexp? sg)))
(define (serialize-string-or-gexp field-name val) "")

(define* (interpose sep lst)
  (fold-right (lambda (e acc)
		(cons e
		      (if (null? acc)
			  acc
			  (cons sep acc))))
	      '() lst))

(define (text-config? config)
  (and (list? config) (every string-or-gexp? config)))
(define (serialize-text-config field-name val)
  #~(string-append #$@(interpose "\n" val)))
