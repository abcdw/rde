(define-module (gnu home-services-utils)
  #:use-module (guix ui)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix monads)

  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-171)

  #:export (alist->key-value))

(define ((alist->key-value prefix sep) alist)
  "Create a key-value list from the associative list ALIST.

PREFIX is the string to prefix the key-value pair with.  For example,
@code{export} will return @code{'(export KEY SEP VALUE)}, where KEY is
the first element of ALIST, and VALUE is the second element of ALIST.

SEP is the separator between the key and the value.

Different things will happen depending on the value of VALUE:
@itemize @bullet
@item If VALUE is #f, ignore everything in the entry and just return
an empty list.

@item If VALUE is #t, ignore the VALUE and SEP and just return a list
of PREFIX and KEY followed by a newline.

@item If VALUE is a list, it will get flattened in the resulting list.

@item If VALUE is a string, just return it as is.

The following code
@lisp
((alist->key-value \"export \" \"=\") '(\"EDITOR\" \"emacsclient\"))
((alist->key-value \"export \" \"=\") '(\"EDITOR\" #t))
((alist->key-value \"export \" \"=\") '(\"EDITOR\" #f))
((alist->key-value \"export \" \"=\") '(\"EDITOR\" (\"emacsclient\" \"vim\")))
@end lisp

would yield

@example
(\"export \" \"EDITOR\" \"=\" \"emacsclient\" \"\n\")
(\"export \" \"EDITOR\" \"\n\")
()
(\"export \" \"EDITOR\" \"=\" \"emacsclient\" \"vim\" \"\n\")
@end example"
  (define (not-alist-error)
    (raise (formatted-message
            (G_ "~a has to be an association list")
            alist)))
  (match alist
    ((_ _ _ _ ...) (not-alist-error))
    ((key . value)
     (let ((values (cond
                    ((string? value)
                     (list value))
                    ((eq? value #f)
                     #f)
                    ((eq? value #t)
                     '(""))
                    ((null? value)
                     (raise (formatted-message
                             (G_ "~a cannot be an empty list")
                             value)))
                    ((list? value)
                     (cond
                      ((eq? (car value) #f)
                       #f)
                      ((eq? (car value) #t)
                       '(""))
                      (else
                       (list-transduce tflatten rcons value))))
                    (else
                     (raise (formatted-message
                             (G_ "~a is not a string, list or boolean")
                             value)))))
           (sep (if (eq? values '(""))
                    ""
                    sep)))
       (if values
           `(,prefix ,key ,sep ,@values "\n")
           '())))
    (_ (not-alist-error))))
