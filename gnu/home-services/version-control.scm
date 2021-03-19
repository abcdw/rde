(define-module (gnu home-services version-control)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)

  #:export (home-git-configuration
	    home-git-service-type
	    ;; git-section
	    make-git-section))

;;; Commentary:
;;;
;;; Version control related services.
;;;
;; (service home-git-service-type
;; 	      (home-git-configuration
;; 	       (attributes
;; 		'((* . text=auto)
;; 		  (*.sh . "text eol=lf")))
;; 	       (ignore
;; 		'("*.so" "*.o"))
;; 	       (ignore-extra-content
;; 		"*.dll\n*.exe\n")
;; 	       (extra-config
;; 		(list
;; 		 (make-git-section 'http "https://weak.example.com"
;; 		  		   '((ssl-verify . #f)))
;; 		 (make-git-section 'sendmail
;; 				   '((annotate . #t)))))
;; 	       (extra-content (text-file->gexp "/home/bob/.gitconfig"))))
;;; Code:

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
    (let ((field-name (uglify-field-name field-name)))
      (if (member field-name '(git-section))
          (format #f "[~a \"~a\"]\n" field-name val)
          (format #f "\t~a = ~a\n" field-name val))))))

;; (field . name) => Field name
(define (serialize-alist-entry entry)
  (match entry
    ((field . val) (serialize-field field val))))

(define (serialize-alist field-name fields)
  (if (null? fields)
      ""
      (apply string-append
             (map serialize-alist-entry fields))))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "true" "false")))

(define serialize-string serialize-field)
(define alist? list?)
(define git-config? list?)

(define-record-type <git-section>
  (git-section name value options)
  git-section?
  (name git-section-name)
  (value git-section-value)
  (options git-section-options))

(define make-git-section
  (case-lambda
    ((name options) (git-section name #f options))
    ((name value options) (git-section name value options))))

(define (serialize-git-section-header name value)
  (format #f "[~a~a]\n" (uglify-field-name name)
	  (if value (format #f " \"~a\"" value) "")))

(define serialize-git-section
  (match-lambda
    (($ <git-section> name value options)
     (string-append
      (serialize-git-section-header name value)
      (serialize-alist #f options)))))

(define (serialize-git-config field-name val)
  (apply string-append
         (map serialize-git-section val)))

(define (string-or-gexp? sg)
  (or (string? sg) (gexp? sg)))
(define (serialize-string-or-gexp field-name val) "")


(define (git-ignore? patterns)
  (list? patterns))
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
  (extra-config
   (git-config '())
   "List of sections and corresponding options.  Something like this:

@lisp
(list
 (make-git-section 'sendmail
                   '((annotate . #t))))
@end lisp

will turn into this:

@example
[sendmail]
        annotate = true
@end example")
  (extra-content
   (string-or-gexp "")
   "String or value of string-valued g-exps will be added to the end
of the configuration file."))

(define (filter-fields fields)
  (filter (lambda (field)
	    (member (configuration-field-name field) fields))
          home-git-configuration-fields))

(define (add-git-configuration config)
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
	(filter-fields '(extra-config)))
       (home-git-configuration-extra-content config)))))

(define (add-git-packages config)
  (list (home-git-configuration-package config)))

(define home-git-service-type
  (service-type (name 'home-git)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-git-configuration)
                       (service-extension
                        home-profile-service-type
                        add-git-packages)))
                (default-value (home-git-configuration))
                (description "Install and configure Git.")))


