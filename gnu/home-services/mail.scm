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
	    home-isync-configuration))

(define (serialize-isync-config field-name val)
  (define (serialize-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      (e e)))
  (define (serialize-item entry)
    (match entry
      ((? gexp? e) e)
      ((field val)
       #~(string-append #$(serialize-term field)
                        " "
                        #$(serialize-term val)))))

  #~(string-append #$@(interpose
                       (map serialize-item val)
                       "\n" 'suffix)))

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
  (list
   (if (home-isync-configuration-xdg-flavor? config)
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
                 (command-line)
                 (append
                  (list "--config"
                        "${XDG_CONFIG_HOME:-$HOME/.config}/isync/mbsyncrc")
                  (command-line)))))))
       (home-isync-configuration-package config))))

(define (add-isync-configuration config)
  `((,(if (home-isync-configuration-xdg-flavor? config)
          "config/isync/mbsyncrc"
          "mbsyncrc")
     ,(mixed-text-file
       "mbsyncrc"
       (serialize-isync-config #f (home-isync-configuration-config config))))))

(define (home-isync-extensions cfg extensions)
  (display extensions)
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
