(define-module (gnu home-services)
  #:use-module (gnu services)
  #:use-module (gnu home-services-utils)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix ui)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-service-type
	    home-profile-service-type
	    home-environment-variables-service-type
	    home-run-on-first-login-service-type
	    home-run-on-reconfigure-service-type
            fold-home-service-types)

  #:re-export (service
	       service-type
	       service-extension))

(define (home-derivation entries mextensions)
  "Return as a monadic value the derivation of the 'home-environment'
directory containing the given entries."
  (mlet %store-monad ((extensions (mapm/accumulate-builds identity
                                                          mextensions)))
    (lower-object
     (file-union "home-environment"
                 (append entries (concatenate extensions))))))

(define home-service-type
  ;; This is the ultimate service type, the root of the service DAG.
  ;; The service of this type is extended by monadic name/item pairs.
  ;; These items end up in the "home-environment directory" as
  ;; returned by 'home-environment-derivation'.
  (service-type (name 'home)
                (extensions '())
                (compose identity)
                (extend home-derivation)
		(default-value '())
                (description
                 "Build the home environment top-level directory,
which in turn refers to everything the home environment needs: its
packages, configuration files, activation script, and so on.")))

(define (packages->profile-entry packages)
  "Return a system entry for the profile containing PACKAGES."
  ;; XXX: 'mlet' is needed here for one reason: to get the proper
  ;; '%current-target' and '%current-target-system' bindings when
  ;; 'packages->manifest' is called, and thus when the 'package-inputs'
  ;; etc. procedures are called on PACKAGES.  That way, conditionals in those
  ;; inputs see the "correct" value of these two parameters.  See
  ;; <https://issues.guix.gnu.org/44952>.
  (mlet %store-monad ((_ (current-target-system)))
    (return `(("profile" ,(profile
                           (content (packages->manifest
                                     (delete-duplicates packages eq?)))))))))

(define home-profile-service-type
  (service-type (name 'home-profile)
                (extensions
                 (list (service-extension home-service-type
                                          packages->profile-entry)))
                (compose concatenate)
                (extend append)
                (description
                 "This is the @dfn{home profile}, available as
@file{~/.guix-home-environment/profile}.  It contains packages that
the user wants to be available.")))

(define (environment-variables->setup-environment-script vars)
  "Return a file that can be sourced by bash/zsh that initialize the
environment.  Sources home environment profile, sets default variables
and sets variables provided in @code{vars}.  @code{vars} is a list of
pairs (@code{(key . value)}), @code{key} is a string and @code{value}
is a string or gexp.

If value is @code{#f} variable will be omitted.
If value is @code{#t} variable will be just exported.
For any other, value variable will be set to the @code{value} and
exported.
"
  (define (warn-about-duplicate-defenitions)
    (fold
     (lambda (x acc)
       (when (equal? (car x) (car acc))
	 (warning
	  (G_ "duplicate definition for `~a' environment variable ~%") (car x)))
       x)
     (cons "" "")
     (sort vars (lambda (a b)
		  (string<? (car a) (car b))))))

  (warn-about-duplicate-defenitions)
  (with-monad
   %store-monad
   (return
    `(("setup-environment"
       ,(apply mixed-text-file "setup-environment"
	       (string-append
	        "HOME_ENVIRONMENT="
		(assoc-ref vars "GUIX_HOME_ENVIRONMENT_DIRECTORY")
		"
GUIX_PROFILE=\"$HOME_ENVIRONMENT/profile\" ; \\
. \"$HOME_ENVIRONMENT/profile/etc/profile\"

[[ :$XDG_DATA_DIRS: =~ :$HOME_ENVIRONMENT/profile/share: ]] || \
export XDG_DATA_DIRS=$HOME_ENVIRONMENT/profile/share:$XDG_DATA_DIRS
[[ :$MANPATH: =~ :$HOME_ENVIRONMENT/profile/share/man: ]] || \
export MANPATH=$HOME_ENVIRONMENT/profile/share/man:$MANPATH
[[ :$INFOPATH: =~ :$HOME_ENVIRONMENT/profile/share/info: ]] || \
export INFOPATH=$HOME_ENVIRONMENT/profile/share/info:$INFOPATH
[[ :$XDG_CONFIG_DIRS: =~ :$HOME_ENVIRONMENT/profile/etc/xdg: ]] || \
export XDG_CONFIG_DIRS=$HOME_ENVIRONMENT/profile/etc/xdg:$XDG_CONFIG_DIRS
[[ :$XCURSOR_PATH: =~ :$HOME_ENVIRONMENT/profile/share/icons: ]] || \
export XCURSOR_PATH=$HOME_ENVIRONMENT/profile/share/icons:$XCURSOR_PATH

")

	       (append-map
		(match-lambda
		  ((key . #f)
		   '())
		  ((key . #t)
		   (list "export " key "\n"))
		  ((key . value)
                   (list "export " key "=" value "\n")))
		vars)))))))

(define home-environment-variables-service-type
  (service-type (name 'home-environment-variables)
                (extensions
                 (list (service-extension
			home-service-type
                        environment-variables->setup-environment-script)))
                (compose concatenate)
                (extend append)
		(default-value '())
                (description "Sets the environment variables.")))

(define (compute-on-first-login-script _ gexps)
  (gexp->file
   "on-first-login"
   #~(let* ((xdg-runtime-dir (or (getenv "XDG_RUNTIME_DIR")
				 (format #f "/run/user/~a" (getuid))))
	    (flag-file-path (string-append
			     xdg-runtime-dir "/on-first-login-executed"))
	    (touch (lambda (file-name)
		     (call-with-output-file file-name (const #t)))))
       ;; XDG_RUNTIME_DIR dissapears on logout, that means such trick
       ;; allows to launch on-first-login script on first login only
       ;; after complete logout/reboot.
       (when (not (file-exists? flag-file-path))
	 (begin #$@gexps (touch flag-file-path))))))

(define (on-first-login-script-entry m-on-first-login)
  "Return, as a monadic value, an entry for the on-first-login script
in the home environment directory."
  (mlet %store-monad ((on-first-login m-on-first-login))
	(return `(("on-first-login" ,on-first-login)))))

(define home-run-on-first-login-service-type
  (service-type (name 'home-run-on-first-login)
                (extensions
                 (list (service-extension
			home-service-type
                        on-first-login-script-entry)))
                (compose identity)
                (extend compute-on-first-login-script)
		(default-value #f)
                (description "Runs gexps on first user login.  Can
be extended with one gexp.")))

(define (compute-on-reconfigure-script _ gexps)
  (gexp->file "on-reconfigure"
              #~(begin #$@gexps)))

(define (on-reconfigure-script-entry m-on-reconfigure)
  "Return, as a monadic value, an entry for the on-reconfigure script
in the home environment directory."
  (mlet %store-monad ((on-reconfigure m-on-reconfigure))
    (return `(("on-reconfigure" ,on-reconfigure)))))

(define home-run-on-reconfigure-service-type
  (service-type (name 'home-run-on-reconfigure)
                (extensions
                 (list (service-extension
			home-service-type
                        on-reconfigure-script-entry)))
                (compose identity)
                (extend compute-on-reconfigure-script)
		(default-value #f)
                (description "Runs gexps to update current state of
home during reconfiguration.  All gexps must be idempotent.  Can
be extended with one gexp.")))



;; Used for searching for services
(define (parent-directory directory)
  "Get the parent directory of DIRECTORY"
  (string-join (drop-right (string-split directory #\/) 1) "/"))

(define %guix-home-root-directory
  ;; Absolute file name of the module hierarchy.
  ;; TODO: Change this when merged upstream
  (parent-directory (dirname (search-path %load-path "gnu/home.scm"))))

(define %service-type-path
  ;; Search path for service types.
  (make-parameter `((,%guix-home-root-directory . "gnu/home-services"))))

(define (all-service-modules)
  "Return the default set of home-service modules."
  (cons (resolve-interface '(gnu home-services))
        (all-modules (%service-type-path)
                     #:warn warn-about-load-error)))

(define* (fold-home-service-types proc seed)
  (fold-service-types proc seed (all-service-modules)))
