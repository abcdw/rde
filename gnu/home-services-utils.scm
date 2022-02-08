;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu home-services-utils)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services utils)
  #:use-module (guix ui)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix i18n)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)

  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)

  #:re-export (filter-configuration-fields

               list-of

               list-of-strings?
               alist?
               text-config?
               serialize-text-config
               generic-serialize-alist-entry
               generic-serialize-alist

               maybe-object->string
               object->snake-case-string
               object->camel-case-string)

  #:export (slurp-file-gexp

            alist-entry->mixed-text
            boolean->yes-or-no
            boolean->true-or-false
            list->human-readable-list

            ini-config?
            generic-serialize-ini-config
            generic-serialize-git-ini-config

            yaml-config?
            serialize-yaml-config

            string-or-gexp?
            serialize-string-or-gexp

            gexp-text-config?
            serialize-gexp-text-config

            rest
            maybe-list
            optional
            wrap-package

            define-enum
            enum-name
            enum-value))


;;;
;;; User's utils.
;;;

(define* (slurp-file-gexp file #:key (encoding "UTF-8"))
  "Returns a gexp, which reads all the content of the FILE and returns
it as a string.  FILE must be a file-like object."
  (when (not (file-like? file))
    (raise (formatted-message
            (G_ "~a is not a file-like object.")
            file)))
  #~(call-with-input-file #$file
      (@ (ice-9 textual-ports) get-string-all)
      #:encoding #$encoding))


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

(define* (boolean->true-or-false bool #:optional (capitalize? #f))
  "Convert a boolean BOOL to \"true\" or \"false\".
Setting CAPITALIZE? to @code{#t} will capitalize the word, it is set to
@code{#f} by default."
  (let ((word (if bool "true" "false")))
    (if capitalize?
        (string-capitalize word)
        word)))

;; TODO: Remove once upstreamed
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



;;;
;;; Serializers.
;;;

(define ini-config? list?)
(define (generic-serialize-ini-config-section section proc)
  "Format a section from SECTION for an INI configuration.
Apply the procedure PROC on SECTION after it has been converted to a string"
  (format #f "[~a]\n" (proc section)))

(define default-ini-format-section
  (match-lambda
    ((section subsection)
     (string-append (maybe-object->string section) " "
                    (maybe-object->string subsection)))
    (section
     (maybe-object->string section))))

(define* (generic-serialize-ini-config
          #:key
          (combine-ini string-join)
          (combine-alist string-append)
          (combine-section-alist string-append)
          (format-section default-ini-format-section)
          serialize-field
          fields)
  "Create an INI configuration from nested lists FIELDS.  This uses
@code{generic-serialize-ini-config-section} and @{generic-serialize-alist} to
serialize the section and the association lists, respectively.

@example
(generic-serialize-ini-config
 #:serialize-field (lambda (a b) (format #f \"~a = ~a\n\" a b))
 #:format-section (compose string-capitalize symbol->string)
 #:fields '((application ((key . value)))))
@end example

@result{} \"[Application]\nkey = value\n\""
  (combine-ini
   (map (match-lambda
          ((section alist)
           (combine-section-alist
            (generic-serialize-ini-config-section section format-section)
            (generic-serialize-alist combine-alist serialize-field alist))))
        fields)
   "\n"))

(define* (generic-serialize-git-ini-config
          #:key
          (combine-ini string-join)
          (combine-alist string-append)
          (combine-section-alist string-append)
          (format-section default-ini-format-section)
          serialize-field
          fields)
  "Like @code{generic-serialize-ini-config}, but the section can also
have a @dfn{subsection}.  FORMAT-SECTION will take a list of two
elements: the section and the subsection."
  (combine-ini
   (map (match-lambda
          ((section subsection alist)
           (combine-section-alist
            (generic-serialize-ini-config-section
             (list section subsection) format-section)
            (generic-serialize-alist combine-alist serialize-field alist)))
          ((section alist)
           (combine-section-alist
            (generic-serialize-ini-config-section section format-section)
            (generic-serialize-alist combine-alist serialize-field alist))))
        fields)
   "\n"))

(define yaml-config? list?)
(define (make-yaml-indent depth)
  (make-string (* 2 depth) #\space))

(define ((serialize-yaml-value depth) value)
  (let* ((tab (make-yaml-indent depth)))
    (cond
     ((string? value)
      (list (format #f "'~a'" value)))
     ((boolean? value)
      (list (format #f "~a" (if value "true" "false"))))
     ((file-like? value)
      (list value))
     ((alist? value)
      (serialize-yaml-alist value #:depth (1+ depth)))
     ((vector? value)
      (serialize-yaml-vector value #:depth depth))
     (else (list (format #f "~a" value))))))

(define ((serialize-yaml-key depth) key)
  (when (vector? key)
    (raise (formatted-message
            (G_ "Vector as key value are not supported by serializer, \
try to avoid them. ~a") key)))
  ((serialize-yaml-value depth) key))

(define ((serialize-yaml-key-value depth) key value)
  (let ((tab (make-yaml-indent depth)))
    `("\n"
      ,tab
      ,@((serialize-yaml-key depth) key) ": "
      ,@((serialize-yaml-value depth) value))))

(define ((serialize-yaml-vector-elem depth) elem)
  (let ((tab (make-yaml-indent (1+ depth))))
    (cons*
     "\n" tab "- "
     ((serialize-yaml-value (1+ depth)) elem))))

(define* (serialize-yaml-vector vec #:key (depth 0))
  (append-map (serialize-yaml-vector-elem depth) (vector->list vec)))

(define* (serialize-yaml-alist lst #:key (depth 0))
  (generic-serialize-alist append (serialize-yaml-key-value depth) lst))

(define (serialize-yaml-config config)
  "Simplified yaml serializer, which supports only a subset of yaml, use
it with caution."
  (serialize-yaml-alist config))

(define (string-or-gexp? sg) (or (string? sg) (gexp? sg)))
(define (serialize-string-or-gexp field-name val) "")

;; Guix proper has a different version of text-config.
(define (gexp-text-config? config)
  (and (list? config) (every string-or-gexp? config)))
(define (serialize-gexp-text-config field-name val)
  #~(string-append #$@(interpose val "\n" 'suffix)))

;;;
;;; Miscellaneous.
;;;

(define rest cdr)

;; Confusing with maybe-list type.
(define (maybe-list a)
  "If A is a list, return it, otherwise return a singleton list with A."
      (if (list? a)
          a
          (list a)))

;; If EXPR1 evaluates to a non-@code{#f} value and EXPR2 is specified,
;; return EXPR2; if it isn't specified, return EXPR1.  Otherwise, return
;; an empty list @code{'()}.
(define-syntax optional
  (syntax-rules ()
    ((_ expr1)
     (if expr1 expr1 '()))
    ((_ expr1 expr2)
     (if expr1 expr2 '()))))

(define (wrap-package pkg executable-name gexp)
  "Create a @code{<package>} object that is a wrapper for PACKAGE, and
runs GEXP.  NAME is the name of the executable that will be put in the store."
  (let* ((wrapper-name (string-append executable-name "-wrapper"))
         (wrapper (program-file wrapper-name gexp)))
    (package
      (inherit pkg)
      (name wrapper-name)
      (source wrapper)
      (propagated-inputs `((,(package-name pkg) ,pkg)))
      (build-system trivial-build-system)
      (arguments
       `(#:modules
         ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils)
                        (srfi srfi-1))
           (let* ((bin (string-append %output "/bin"))
                  (wrapper (assoc-ref %build-inputs "source")))
             (mkdir-p bin)
             (copy-file wrapper (string-append bin "/" ,executable-name)))))))))


;;;
;;; Enums.
;;;

(define-record-type <enum>
  (make-enum name value)
  enum?
  (name enum-name)
  (value enum-value))

;; Copied from (gnu services configuration)
(define-syntax-rule (id ctx parts ...)
  "Assemble PARTS into a raw (unhygienic)  identifier."
  (datum->syntax ctx (symbol-append (syntax->datum parts) ...)))

;; (define-enum pinentry-flavor
;;   '(emacs gtk qt ncurses tty))
;;
;; (pinentry-flavor? 'gtk)
;; => #t
;;
;; (enum-value pinentry-flavor)
;; => '(emacs gtk qt ncurses tty)
;;
;; (pinentry-flavor? 'vim)
;; exception: `pinetry-flavor' must be one of `emacs', `gtk', `qt',
;; `ncurses', or `tty', was given `vim'

(define-syntax define-enum
  (lambda (x)
    (syntax-case x ()
      ((_ stem value)
       (with-syntax ((stem? (id #'stem #'stem #'?))
                     (msg (list->human-readable-list
                           (second (syntax->datum #'value))
                           #:proc (cut format #f "`~a'" <>))))
         #'(begin
             (define stem (make-enum (quote stem) value))

             (define (stem? val)
               (if (member val value)
                   #t
                   (raise (formatted-message
                           (G_ "`~a' must of ~a, was given: ~s")
                           (enum-name stem)
                           (syntax->datum msg)
                           val))))))))))
