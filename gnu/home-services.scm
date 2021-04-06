(define-module (gnu home-services)
  #:use-module (gnu services)
  #:use-module (gnu home-services-utils)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-service-type
	    home-profile-service-type
	    home-environment-vars-service-type
	    home-run-on-first-login-service-type
	    home-run-on-reconfigure-service-type)

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
	       (format #f "\
HOME_ENVIRONMENT=\"~a\"
GUIX_PROFILE=\"$HOME_ENVIRONMENT/profile\" ; \\
. \"$HOME_ENVIRONMENT/profile/etc/profile\"

export XDG_DATA_DIRS=$HOME_ENVIRONMENT/profile/share:$XDG_DATA_DIRS
export MANPATH=$HOME_ENVIRONMENT/profile/share/man:$MANPATH
export INFOPATH=$HOME_ENVIRONMENT/profile/share/info:$INFOPATH
export XDG_CONFIG_DIRS=$HOME_ENVIRONMENT/profile/etc/xdg:$XDG_CONFIG_DIRS
export XCURSOR_PATH=$HOME_ENVIRONMENT/profile/share/icons:$XCURSOR_PATH
" (assoc-ref vars "GUIX_HOME_ENVIRONMENT_DIRECTORY"))

	       (append-map
		(match-lambda
		  ((key . #f)
		   '())
		  ((key . #t)
		   (list "export " key "\n"))
		  ((key . value)
                   (list "export " key "=" value "\n")))
		vars)))))))

(define home-environment-vars-service-type
  (service-type (name 'home-environment-vars)
                (extensions
                 (list (service-extension
			home-service-type
                        environment-variables->setup-environment-script)))
                (compose concatenate)
                (extend append)
		(default-value '())
                (description "Sets the environment variables.")))

;; TODO: maybe change to guile script instead of bash to make it more
;; convinient to generate it. In contrast to the
;; environment-vars-service it won't be sourced by any shells.
(define (commands->on-login-script cmds)
  "Return a script that can be started by bash/zsh's profile that will
run @code{cmds} on login."
  (with-monad
   %store-monad
   (return
    `(("on-login"
       ,(apply mixed-text-file
	 "on-login"
	 ;; XDG_RUNTIME_DIR dissapears on logout, that means such
	 ;; trick allows to launch on-login script on first login only
	 ;; after complete logout/reboot.
	 "\
if [ ! -f $XDG_RUNTIME_DIR/on-login-executed ]; then

"
	  (append (interpose cmds "\n" 'suffix)
		  '("
touch $XDG_RUNTIME_DIR/on-login-executed
fi\n"))))))))

(define home-run-on-first-login-service-type
  (service-type (name 'home-run-on-first-login)
                (extensions
                 (list (service-extension
			home-service-type
                        commands->on-login-script)))
                (compose concatenate)
                (extend append)
		(default-value '())
                (description "Runs commands on first user login.")))

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
                (description "Runs idempotent commands to update
current state of home.")))
