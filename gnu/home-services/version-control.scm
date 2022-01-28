(define-module (gnu home-services version-control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module ((guix import utils) #:select (flatten))

  #:export (home-git-configuration
	    home-git-extension
	    home-git-service-type
	    serialize-git-config

            home-hg-configuration
            home-hg-extension
            serialize-hg-config
            home-hg-service-type))

;;; Commentary:
;;;
;;; Version control related services.
;;;
;;; Code:

;;;
;;; Git.
;;;
;;; (service home-git-service-type
;;; 	 (home-git-configuration
;;; 	  (attributes
;;; 	   '((* . text=auto)
;;; 	     (*.sh . "text eol=lf")))
;;; 	  (ignore
;;; 	   '("*.so" "*.o"))
;;; 	  (ignore-extra-content
;;; 	   "*.dll\n*.exe\n")
;;; 	  (config
;;; 	   `((http "https://weak.example.com"
;;; 		   ((ssl-verify . #f)))
;;; 	     (gpg
;;; 	      ((program . ,(file-append gnupg "/bin/gpg"))))
;;; 	     (sendmail
;;; 	      ((annotate . #t))))
;;; 	   (config-extra-content (slurp-file-gexp
;;;                                (local-file "./gitconfig")))))
;;;
;;; (simple-service
;;;  'add-something-to-git
;;;  home-git-service-type
;;;  (home-git-extension
;;;   (config
;;; 	`((sendmail
;;; 	   ((annotate . #t)))))))


(define (uglify-field-name field-name)
  "Convert symbol FIELD-NAME to a camel case string.
@code{symbol-name} => \"@code{symbolName}\"."
  (let* ((str (symbol->string field-name))
	 (spl-str (string-split str #\-)))
    (apply string-append
	   (car spl-str)
	   (map string-capitalize (cdr spl-str)))))

(define (serialize-git-term term)
  (match term
    (#t "true")
    (#f "false")
    ((? symbol? e) (symbol->string e))
    ((? number? e) (number->string e))
    ((? string? e) e)
    ((lst ...)
     (raise (formatted-message
             (G_ "Git term should be a non-list value (string, \
boolean, number, symbol, or gexp). Provided term is:\n ~a") lst)))
    (e e)))

(define (serialize-field field-name val)
  (list (format #f "\t~a = " (uglify-field-name field-name))
	(serialize-git-term val) "\n"))

(define (serialize-alist field-name val)
  (generic-serialize-alist append serialize-field val))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "true" "false")))

(define git-config? list?)

(define (serialize-git-section-header name value)
  (format #f "[~a~a]\n" (uglify-field-name name)
	  (if value (format #f " \"~a\"" value) "")))

(define serialize-git-section
  (match-lambda
    ((name options)
     (cons
      (serialize-git-section-header name #f)
      (serialize-alist #f options)))
    ((name value options)
     (cons
      (serialize-git-section-header name value)
      (serialize-alist #f options)))))

;; TODO: cover it with tests
(define (serialize-git-config field-name val)
  #~(string-append #$@(append-map serialize-git-section val)))

(define (git-ignore? patterns)
  (list-of-strings? patterns))
(define (serialize-git-ignore field-name val)
  (string-join val "\n" 'suffix))

(define (git-attributes? attrs)
  (list? attrs))
(define (serialize-git-attributes field-name val)
  (string-join
   (map
    (match-lambda
      ((key . value) (format #f "~a\t~a" key value)))
    val)
   "\n"
   'suffix))

(define-configuration home-git-extension
  (attributes
   (git-attributes '())
   "Alist of pattern attribute pairs for @file{git/attributes.}")
  (ignore
   (git-ignore '())
   "List of patterns for @file{git/ignore.}")
  (config
   (git-config '())
   "List of git sections.  The same format as in
@code{home-git-configuration}."))

(define-configuration home-git-configuration
  (package
   (package git)
   "The Git package to use.")
  (attributes
   (git-attributes '())
   "Alist of pattern attribute pairs for @file{git/attributes.}")
  (attributes-extra-content
   (string-or-gexp "")
    "String or value of string-valued g-exps will be added to the end
of the @file{git/attributes} file.")
  (ignore
   (git-ignore '())
   "List of patterns for git/ignore.")
  (ignore-extra-content
   (string-or-gexp "")
    "String or value of string-valued g-exps will be added to the end
of the git/ignore file.")
  (config
   (git-config '())
   "List of sections and corresponding options.  Something like this:

@lisp
`((sendmail
   ((annotate . #t))))
@end lisp

will turn into this:

@example
[sendmail]
        annotate = true
@end example")
  (config-extra-content
   (string-or-gexp "")
   "String or value of string-valued g-exps will be added to the end
of the configuration file."))

(define (add-git-configuration config)
  (define (filter-fields fields)
    (filter-configuration-fields home-git-configuration-fields fields))
  `(("config/git/attributes"
     ,(mixed-text-file
       "git-attributes"
       (serialize-configuration
        config
	(filter-fields '(attributes)))
       (home-git-configuration-attributes-extra-content config)))
    ("config/git/ignore"
     ,(mixed-text-file
       "git-ignore"
       (serialize-configuration
        config
	(filter-fields '(ignore)))
       (home-git-configuration-ignore-extra-content config)))
    ("config/git/config"
     ,(mixed-text-file
	     "git-config"
	     (serialize-configuration
              config
	      (filter-fields '(config)))
	     (home-git-configuration-config-extra-content config)))))

(define (add-git-packages config)
  (list (home-git-configuration-package config)))

(define (home-git-extensions original-config extension-configs)
  (home-git-configuration
   (inherit original-config)
   (attributes
    (append (home-git-configuration-attributes original-config)
	    (append-map
	     home-git-extension-attributes extension-configs)))
   (ignore
    (append (home-git-configuration-ignore original-config)
	    (append-map
	     home-git-extension-ignore extension-configs)))
   (config
    (append (home-git-configuration-config original-config)
	    (append-map
	     home-git-extension-config extension-configs)))))

(define home-git-service-type
  (service-type (name 'home-git)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-git-configuration)
                       (service-extension
                        home-profile-service-type
                        add-git-packages)))
		(compose identity)
		(extend home-git-extensions)
                (default-value (home-git-configuration))
                (description "Install and configure Git.")))

(define (generate-home-git-documentation)
  (generate-documentation
   `((home-git-configuration
      ,home-git-configuration-fields))
   'home-git-configuration))


;;;
;;; Mercurial.
;;;
;;; (home-hg-configuration
;;;   (regexp-ignore '("^\\.pc/"))
;;;   (glob-ignore '("*.elc" "*~"))
;;;   (config
;;;    '((commands
;;;       ((commit.post-status . #t)))
;;;      (ui
;;;       ((username . "Alice Bobson <charlie@example.org")))
;;;      (defaults
;;;        (log . "-v")))))
;;;

;; TODO: Add separate field for name and email?
(define-configuration/no-serialization home-hg-configuration
  (package
    (package mercurial)
    "The Mercurial package to use.")
  (regexp-ignore
   (list-of-strings '())
   "List of regular expressions to ignore globally.  The default syntax
is Python/Perl-style regular expression (see @command{man 5 hgignore}).

The @code{*-ignore} fields are equivalent to adding @code{ui.ignore =
/file/with/ignore/rules} in your @file{hgrc}.")
  (glob-ignore
   (list-of-strings '())
   "List of globs to ignore globally.")
  (rootglob-ignore
   (list-of-strings '())
   "List of @dfn{rootglobs} to ignore globally.")
  (config
   (ini-config '())
   "List of list representing the contents of the @file{hgrc}
configuration file.  The syntax is similar to that of the Git service.
The key of a pair can be a symbol or string, and the value can be a
boolean, string, symbol, number, gexp (@pxref{gexp,,,guix.info}), or a
list of one the above.

@lisp
(config
 `((commands
    ((commit.post-status . #t)))
   (graph
    ((width . 4)))
   (hooks
    ((incoming.email . ,(local-file \"/path/to/email/hook\"))))))
@end lisp

will turn into this:

@example
[commands]
    commit.post-status = True
[graph]
    width = 4
[hooks]
  incoming.email = /gnu/store/123...-email-hook
@end example"))

(define (serialize-hg-config config)
  (define (serialize-boolean val)
    (list (if val "True" "False")))

  (define (serialize-list val)
    (interpose (map serialize-val val) ", "))

  (define (serialize-val val)
    (cond
     ((list? val) (serialize-list val))
     ((boolean? val) (serialize-boolean val))
     ((or (number? val) (symbol? val)) (list (maybe-object->string val)))
     (else (list val))))

  (define (serialize-field key val)
    (let ((val (serialize-val val))
          (key (symbol->string key)))
      `(,key " = " ,@val "\n")))

  (flatten (generic-serialize-ini-config
            #:combine-ini interpose
            #:combine-alist list
            #:combine-section-alist cons
            #:serialize-field serialize-field
            #:fields config)))

(define* (serialize-hg-ignores #:key regexp glob rootglob)
  (define (add-ignore lst type)
    (if (not (null? lst))
        (string-append (format #f "syntax: ~a\n" type)
                       (string-join lst "\n" 'suffix))
        ""))

  (string-join (map (cut add-ignore <> <>)
                    (list regexp glob rootglob)
                         '(regexp glob rootglob))
               "\n"))

(define (home-hg-files-service config)
  (define rest cdr)

  (define (compare-sections section1 section2)
    (string<? (symbol->string (first section1))
              (symbol->string (first section2))))

  (define (fold-sections section1 section2)
    (cond
     ((equal? (first section1) (first section2))
      (list (list (first section1)
                  (append (second section1) (second section2)))))
     (else
      (list section1 section2))))

  (define (merge-sections config)
    (let ((sorted-config (sort config compare-sections)))
      (fold (lambda (section acc)
              (if (null? acc)
                  (list section)
                  (append (fold-sections section (first acc))
                          (rest acc))))
            '()
            sorted-config)))

  (let* ((ignores (serialize-hg-ignores
                   #:regexp
                   (home-hg-configuration-regexp-ignore config)
                   #:glob
                   (home-hg-configuration-glob-ignore config)
                   #:rootglob
                   (home-hg-configuration-rootglob-ignore config)))
         (final-config (merge-sections
                        (append (home-hg-configuration-config config)
                                `((ui
                                   ((ignore . ,(plain-file "hg-ignores"
                                                           ignores)))))))))
    `(("config/hg/hgrc"
       ,(apply mixed-text-file
               "hgrc"
               (serialize-hg-config final-config))))))

(define-configuration/no-serialization home-hg-extension
  (regexp-ignore
   (list-of-strings '())
   "List of regular expressions to ignore globally.")
  (glob-ignore
   (list-of-strings '())
   "List of glob expressions to ignore globally.")
  (rootglob-ignore
   (list-of-strings '())
   "List of @dfn{rootglobs} to ignore globally.")
  (config
   (ini-config '())
   "List of lists representing the contents of the @file{hgrc} file."))

(define (home-hg-extensions original-config extension-configs)
  (home-hg-configuration
   (inherit original-config)
   (regexp-ignore
    (append (home-hg-configuration-regexp-ignore original-config)
            (append-map
             home-hg-extension-regexp-ignore extension-configs)))
   (glob-ignore
    (append (home-hg-configuration-glob-ignore original-config)
            (append-map
             home-hg-extension-glob-ignore extension-configs)))
   (rootglob-ignore
    (append (home-hg-configuration-rootglob-ignore original-config)
            (append-map
             home-hg-extension-rootglob-ignore extension-configs)))
   (config
    (append (home-hg-configuration-config original-config)
            (append-map
             home-hg-extension-config extension-configs)))))

(define (home-hg-profile-service config)
  (list (home-hg-configuration-package config)))

(define home-hg-service-type
  (service-type (name 'home-hg)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-hg-files-service)
                       (service-extension
                        home-profile-service-type
                        home-hg-profile-service)))
                (compose identity)
                (extend home-hg-extensions)
                (default-value (home-hg-configuration))
                (description "\
Install and configure the Mercurial version control system.")))

(define (generate-home-hg-documentation)
  (string-append
   (generate-documentation
    `((home-hg-configuration
       ,home-hg-configuration-fields))
    'home-hg-configuration)
   "\n\n"
   (generate-documentation
    `((home-hg-extension
       ,home-hg-extension-fields))
    'home-hg-extension)))
