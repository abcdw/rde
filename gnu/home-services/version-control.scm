(define-module (gnu home-services version-control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services files)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)

  #:export (home-git-configuration
	    home-git-extension
	    home-git-service-type
	    serialize-git-config))

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

(define (serialize-field field-name val)
   (cond
    ((boolean? val) (serialize-boolean field-name val))
    (else
     (list (format #f "\t~a = " (uglify-field-name field-name))
	   val "\n"))))

(define (serialize-alist field-name val)
  (generic-serialize-alist append serialize-field val))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "true" "false")))

(define serialize-string serialize-field)
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
  (listof-strings? patterns))
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
   "Alist of pattern attribute pairs for git/attributes.")
  (ignore
   (git-ignore '())
   "List of patterns for git/ignore.")
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
   "Alist of pattern attribute pairs for git/attributes.")
  (attributes-extra-content
   (string-or-gexp "")
    "String or value of string-valued g-exps will be added to the end
of the git/attributes file.")
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


