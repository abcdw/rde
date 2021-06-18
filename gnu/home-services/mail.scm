(define-module (gnu home-services mail)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services files)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages mail)
  #:use-module (gnu services configuration)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:export (home-isync-service-type
	    home-isync-configuration

            home-notmuch-service-type
	    home-notmuch-configuration
            home-notmuch-extension))

(define (serialize-isync-config field-name val)
  (define (serialize-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? string? e) (format #f "~s" e))
      (e e)))
  (define (serialize-item entry)
    (match entry
      ((? gexp? e) e)
      ((? list lst)
       #~(string-join '#$(map serialize-term lst)))))

  #~(string-append #$@(interpose (map serialize-item val) "\n" 'suffix)))

(define-configuration/no-serialization home-isync-configuration
  (package
   (package isync)
   "isync package to use.")
  (xdg-flavor?
   (boolean #t)
   "Whether to use the {$XDG_CONFIG_HOME/isync/mbsyncrc} configuration
file or not.  If @code{#t} creates a wrapper for mbsync binary.")
  (config
   (list '())
   "AList of pairs, each pair is a String and String or Gexp."))

(define (add-isync-package config)
  (if (home-isync-configuration-xdg-flavor? config)
      (list
       (home-isync-configuration-package config)
       (wrap-package
        (home-isync-configuration-package config)
        "mbsync"
        #~(system
           (string-join
            (cons
             #$(file-append (home-isync-configuration-package config)
                            "/bin/mbsync")
             (if (or (member "-c" (command-line))
                     (member "--config" (command-line)))
                 (cdr (command-line))
                 (append
                  (list "--config"
                        "${XDG_CONFIG_HOME:-$HOME/.config}/isync/mbsyncrc")
                  (cdr (command-line)))))))))
      (list (home-isync-configuration-package config))))

(define (add-isync-configuration config)
  `((,(if (home-isync-configuration-xdg-flavor? config)
          "config/isync/mbsyncrc"
          "mbsyncrc")
     ,(mixed-text-file
       "mbsyncrc"
       (serialize-isync-config #f (home-isync-configuration-config config))))))

(define (home-isync-extensions cfg extensions)
  (home-isync-configuration
   (inherit cfg)
   (config (append (home-isync-configuration-config cfg) extensions))))

(define home-isync-service-type
  (service-type (name 'home-isync)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-isync-package)
		       (service-extension
                        home-files-service-type
                        add-isync-configuration)))
		(compose concatenate)
		(extend home-isync-extensions)
                (default-value (home-isync-configuration))
                (description "Install and configure isync.")))

(define (list-of-gexps? lst)
  (and (list? lst) (every gexp? lst)))

(define-configuration/no-serialization home-notmuch-configuration
  (package
   (package notmuch)
   "notmuch package to use.")
  (xdg-flavor?
   (boolean #t)
   "Whether to use the {$XDG_CONFIG_HOME/notmuch/default/config}
configuration file or not.")
  (config
   (ini-config '())
   "AList of pairs, each pair is a String and String or Gexp.")
  (pre-new
   (list-of-gexps '())
   "List of gexp to add in @file{pre-new} hook. Read @code{man
notmuch-hooks} for more information.")
  (post-new
   (list-of-gexps '())
   "List of gexp to add in @file{post-new} hook. Read @code{man
notmuch-hooks} for more information.")
  (post-insert
   (list-of-gexps '())
   "List of gexp to add in @file{post-insert} hook. Read @code{man
notmuch-hooks} for more information."))

(define-configuration/no-serialization home-notmuch-extension
  (config
   (ini-config '())
   "AList of pairs, each pair is a String and String or Gexp.")
  (pre-new
   (list-of-gexps '())
   "List of gexp to add in @file{pre-new} hook. Read @code{man
notmuch-hooks} for more information.")
  (post-new
   (list-of-gexps '())
   "List of gexp to add in @file{post-new} hook. Read @code{man
notmuch-hooks} for more information.")
  (post-insert
   (list-of-gexps '())
   "List of gexp to add in @file{post-insert} hook. Read @code{man
notmuch-hooks} for more information."))

(define (add-notmuch-package config)
  (list (home-notmuch-configuration-package config)))

(define (add-notmuch-configuration config)
  (define (serialize-field key val)
    (let ((val (cond
                ((list? val) (string-join (map maybe-object->string val) ";"))
                (else val))))
      (format #f "~a=~a\n" key val)))

  (define (filter-fields field)
    (filter-configuration-fields home-notmuch-configuration-fields
				 (list field)))

  (define (hook-file hook gexps)
    (list (string-append "config/notmuch/default/hooks/" hook)
          (program-file (string-append "notmuch-" hook) #~(begin #$@gexps))))

  (define (get-hook hook)
    (let* ((field-obj (car (filter-fields (string->symbol hook))))
           (gexps ((configuration-field-getter field-obj) config)))
      (if (not (null? gexps))
          (hook-file hook gexps)
          '())))

  (remove null?
  `(,@(map get-hook '("pre-new" "post-new" "post-insert"))
    (,(if (home-notmuch-configuration-xdg-flavor? config)
          "config/notmuch/default/config"
          "notmuch-config")
     ,(mixed-text-file
       "notmuch-config"
       (generic-serialize-ini-config
        #:serialize-field serialize-field
        #:fields (home-notmuch-configuration-config config)))))))

(define (home-notmuch-extensions cfg extensions)
  (home-notmuch-configuration
   (inherit cfg)
   (config
    (append (home-notmuch-configuration-config cfg)
            (append-map home-notmuch-extension-config extensions)))
   (pre-new
    (append (home-notmuch-configuration-pre-new cfg)
            (append-map home-notmuch-extension-pre-new extensions)))
   (post-new
    (append (home-notmuch-configuration-post-new cfg)
            (append-map home-notmuch-extension-post-new extensions)))
   (post-insert
    (append (home-notmuch-configuration-post-insert cfg)
            (append-map home-notmuch-extension-post-insert extensions)))))

(define home-notmuch-service-type
  (service-type (name 'home-notmuch)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-notmuch-package)
		       (service-extension
                        home-files-service-type
                        add-notmuch-configuration)))
		(compose identity)
		(extend home-notmuch-extensions)
                (default-value (home-notmuch-configuration))
                (description "Install and configure notmuch.")))
