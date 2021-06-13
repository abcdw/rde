(define-module (gnu home-services emacs)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services files)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services configuration)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 pretty-print)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (home-emacs-service-type
	    home-emacs-configuration
	    home-emacs-extension
	    elisp-configuration-package))

(define packages? (listof package?))

(define serialize-packages empty-serializer)
(define serialize-boolean empty-serializer)

(define elisp-config? list?)
(define (serialize-elisp-config field-name val)
  (define (serialize-list-element elem)
    (cond
     ((gexp? elem)
      elem)
     (else
      #~(string-trim-right
	   (with-output-to-string
	     (lambda ()
	       ((@@ (ice-9 pretty-print) pretty-print)
		'#$elem
                #:max-expr-width 79)))
	   #\newline))))

  #~(string-append
     #$@(interpose
	 (map serialize-list-element val)
	 "\n" 'suffix)))

(define-configuration home-emacs-configuration
  (package
   (package emacs)
   "Emacs package to use.")
  (elisp-packages
   (packages '())
   "List of Emacs Lisp packages to install.")
  (rebuild-elisp-packages?
   (boolean #f)
   "Rebuild Emacs Lisp packages with version of Emacs specified in
PACKAGE field.")
  (server-mode?
   (boolean #f)
   "Create a shepherd service, which starts emacs in a server-mode.  Use
can use @command{emacsclient} to connect to the server (@pxref{Emacs
Server,,,emacs.info}).")
  (xdg-flavor?
   (boolean #t)
   "Whether to place all the configuration files in
@file{$XDG_CONFIG_HOME/emacs}.")
  (init-el
   (elisp-config '())
   "List of expressions, each expression can be a Sexp or Gexp.

Sexp is a Emacs Lisp form, preferably valid.  Be aware, if you include
values of Guile variables, they won't be automatically converted to
Elisp.  Strings doesn't require conversion, but for example booleans
do: @code{#t} -> @code{t}, @code{#f} -> @code{nil}.  Be careful here.

However, Sexp can contain file-like objects; String with path to a
corresponding file will appear in place of each such object.  See an
example below for more details.

Gexp should be string-valued.  The value of Gexp will be appended to
resulting Emacs Lisp file.

The list of expressions will be interposed with \\n and everything
will end up in @file{init.el}.

@example
(let ((guile-bool-value #f))
  (home-emacs-configuration
   (init-el
    `((setq rg-binary ,(file-append ripgrep \"/bin/rg\"))
      (load-file ,(local-file \"./emacs/test-init.el\"))
      \"just a string\"
      ;; Make sure you converted guile values to Elisp
      (setq tmp-boolean ,(if guile-bool-value 't 'nil))
      ,(if guile-bool-value '(setq v1 nil) '(setq v2 t))

      ,#~\"\\n;;; Section with gexps results:\"

      ,(slurp-file-gexp (local-file \"./emacs/test-init.el\"))
      ,#~(string-append \"(princ \" \"'hello)\")
      ,#~\"\\n\"
      ,#~\";; Another comment\"))))
@end example

would yield something like:

@example
(setq rg-binary
      \"/gnu/store/dw884p9d2jb83j4fqvdj2i10fn9xgwqd-ripgrep-12.1.1/bin/rg\")
(load-file
  \"/gnu/store/9b1s48crng5dy9xmxskcdnillw18bkg2-test-init.el\")
\"just a string\"
(setq tmp-boolean nil)
(setq v2 t)

;;; Section with gexps results:
;; Here is
\"a sample\"
;; content of test-init.el

(princ 'hello)


;; Another comment
@end example")
  (early-init-el
   (elisp-config '())
   "List of expressions, each expression can be a Sexp or Gexp.
Same as @code{init-el}, but result will go to @file{early-init.el}."))


(define ((update-emacs-argument-for-package target-emacs) p)
  "Set @code{#:emacs} to EMACS-PACKAGE for package P.  To build elisp
packages with proper GNU Emacs version."
  (if (equal?
       (package-build-system p)
       emacs-build-system)
      (package (inherit p)
	       (arguments
		(substitute-keyword-arguments
		    (package-arguments p)
		  ((#:emacs e #f) target-emacs))))
      p))

(define (emacs-argument-updater target-emacs)
  "Recursively updates @code{#:emacs} argument for package and all the
inputs."
  (package-mapping (update-emacs-argument-for-package target-emacs)
		   (lambda (p) #f)))

(define (updated-elisp-packages config)
  (let* ((emacs-package  (home-emacs-configuration-package config))
	 (elisp-packages (home-emacs-configuration-elisp-packages config))

	 (updated-elisp-packages
	  (if (home-emacs-configuration-rebuild-elisp-packages? config)
	      (map (emacs-argument-updater emacs-package)
		   elisp-packages)
	      elisp-packages)))
    updated-elisp-packages))

(define (add-emacs-packages config)
  (append (updated-elisp-packages config)
          ;; It's important for packages to go first to override
          ;; built-in emacs packages in case of collisions
	  (list (home-emacs-configuration-package config))))


(define (add-emacs-shepherd-service config)
  (optional (home-emacs-configuration-server-mode? config)
      (list (shepherd-service
             (documentation "Emacs server.  Use @code{emacsclient} to
connect to it.")
             (provision '(emacs-server))
             (start #~(make-forkexec-constructor
                       (list #$(file-append
				(home-emacs-configuration-package config)
				"/bin/emacs") "--fg-daemon")
                       #:log-file (string-append
				   (or (getenv "XDG_LOG_HOME")
				       (format #f "~a/.local/var/log"
					       (getenv "HOME")))
				   "/emacs.log")))
             (stop #~(make-kill-destructor))))))

(define (add-emacs-configuration config)
  (let* ((xdg-flavor? (home-emacs-configuration-xdg-flavor? config)))
    (define prefix-file
      (cut string-append
	(if xdg-flavor?
	    "config/emacs/"
	    "emacs.d/")
	<>))

    (define (filter-fields field)
      (filter-configuration-fields home-emacs-configuration-fields
				   (list field)))

    (define (serialize-field field)
      (serialize-configuration
       config
       (filter-fields field)))

    (define (file-if-not-empty field)
      (let ((file-name (string-append
                        (string-drop-right (symbol->string field) 3)
                        ".el"))
            (field-obj (car (filter-fields field))))
        (optional (not (null? ((configuration-field-getter field-obj) config)))
                  `(,(prefix-file file-name)
                    ,(mixed-text-file
                      file-name
                      (serialize-field field))))))

    (filter
     (compose not null?)
     (list
      (file-if-not-empty 'init-el)
      (file-if-not-empty 'early-init-el)))))


(define-configuration home-emacs-extension
  (elisp-packages
   (packages '())
   "List of additional Emacs Lisp packages.")
  (init-el
   (elisp-config '())
   "List of expressions to add to @code{init-el}.  See
@code{home-emacs-service-type} for more information.")
  (early-init-el
   (elisp-config '())
   "List of expressions to add to @code{ealy-init-el}.  See
@code{home-emacs-service-type} for more information."))

(define (home-emacs-extensions original-config extension-configs)
  (home-emacs-configuration
   (inherit original-config)
   (elisp-packages
    (append (home-emacs-configuration-elisp-packages original-config)
	    (append-map
	     home-emacs-extension-elisp-packages extension-configs)))
   (init-el
    (append (home-emacs-configuration-init-el original-config)
	    (append-map
	     home-emacs-extension-init-el extension-configs)))
   (early-init-el
    (append (home-emacs-configuration-early-init-el original-config)
	    (append-map
	     home-emacs-extension-early-init-el extension-configs)))))


(define home-emacs-service-type
  (service-type (name 'home-emacs)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-emacs-packages)
		       (service-extension
			home-shepherd-service-type
			add-emacs-shepherd-service)
		       (service-extension
                        home-files-service-type
                        add-emacs-configuration)))
		(compose identity)
		(extend home-emacs-extensions)
                (default-value (home-emacs-configuration))
                (description "Install and configure GNU Emacs, the
extensible, self-documenting editor.")))

(define (generate-home-emacs-documentation)
  (generate-documentation
   `((home-emacs-configuration
      ,home-emacs-configuration-fields))
   'home-emacs-configuration))


(define* (elisp-configuration-package
	  package-name elisp-expressions
	  #:key
	  (elisp-packages '())
	  (autoloads? #t))
  "Takes a list of Elisp expressions, create emacs-NAME package.
@code{#~\";;;###autoload\"} can be used to make next expression be
loaded on startup."

  (define (package->package-input pkg)
    (list ((@@ (guix packages) package-name) pkg) pkg))

  (define (add-autoloads elisp-expressions)
    (fold-right
     (lambda (e acc)
       (if (list? e)
	   (cons* #~";;;###autoload" e #~"" acc)
	   (cons* e #~"" acc)))
     '() elisp-expressions))

  (package
   (name (string-append "emacs-" package-name))
   (version "1.0.0")
   (build-system emacs-build-system)
   (source
    (mixed-text-file
     (string-append package-name ".el")
     (serialize-elisp-config
      #f
      ((if autoloads? add-autoloads identity) elisp-expressions))))
   (propagated-inputs (map package->package-input elisp-packages))
   (synopsis "Generated Emacs configuration package.")
   (description "Package generated by @code{elisp-configuration-package}.")
   (home-page "https://www.gnu.org/software/guix/")
   (license license:gpl3+)))
