(define-module (gnu home-services-utils)
  #:use-module (guix ui)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix monads)

  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171)

  #:export (alist-entry->mixed-text
            boolean->yes-or-no))

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
