(define-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix profiles)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix records)
  #:export (home-environment
	    home-environment?
	    this-home-environment
	    home-environment-derivation))


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
@file{~/.local/share/home-environment/profile} ???.  It contains
packages that the user wants to be available.")))


(define (environment-variables->setup-environment-script vars)
  "Return a file that can be sourced by bash/zsh that initialize the
environment. Sources home profile, sets default variables and sets
variables provided in @code{vars}. @code{vars} is a list of pairs
string + string/file-like object or list of items, where the first one
is variable and rest are strings/file-like objects.

It's done to be able to express both following cases:
@example
`((\"TMP\" . \"VAR_VALUE\")
  (\"SSH_AUTH_SOCK\" \"$(\" (\\, (file-append gnupg \"/bin/gpgconf\")) \" --list-dirs agent-ssh-socket)\"))
@end example
"

  (with-monad
   %store-monad
   (return
    `(("setup-environment"
       ,(apply mixed-text-file "setup-environment"
	       "\
HOME_ENVIRONMENT=\"$HOME/.guix-home-environment\"
GUIX_PROFILE=\"$HOME_ENVIRONMENT/profile\" ; \\
. \"$HOME_ENVIRONMENT/profile/etc/profile\"

# export MANPATH=$HOME/.guix-home-environment/profile/share/man:$MANPATH
# export INFOPATH=$HOME/.guix-home-environment/profile/share/info:$INFOPATH
export XDG_DATA_DIRS=$HOME_ENVIRONMENT/profile/share:$XDG_DATA_DIRS
# export XDG_CONFIG_DIRS=$HOME/.guix-home-environment/profile/etc/xdg:$XDG_CONFIG_DIRS
# export XCURSOR_PATH=$HOME/.guix-home-environment/profile/share/icons:$XCURSOR_PATH
"
	       (append-map
		(match-lambda
		  ((key . value)
		   (let ((values (if (not (list? value))
				  (list value) value)))

                     `("export " ,key "=" ,@values "\n"))))
		  vars)))))))

(define home-environment-vars-service-type
  (service-type (name 'home-environment-vars)
                (extensions
                 (list (service-extension
			home-service-type
                        environment-variables->setup-environment-script)))
                (compose concatenate)
                (extend append)
                (description "Sets the environment variables on first
login.")))

;; TODO: maybe change to guile script instead of bash to make it more
;; convinient to generate it. Despite the environment-vars-service it
;; won't be sourced by any shells.
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
	  (append cmds
		  '("
touch $XDG_RUNTIME_DIR/on-login-executed
fi\n"))))))))

(define home-run-on-first-login-service-type
  (service-type (name 'home-environment-vars)
                (extensions
                 (list (service-extension
			home-service-type
                        commands->on-login-script)))
                (compose concatenate)
                (extend append)
                (description "Runs commands on first user login.")))

(define (add-ssh-agent-socket %)
  "Adds SSH_AUTH_SOCK variable to user's environment."
  `(("SSH_AUTH_SOCK" "$(" ,(file-append gnupg "/bin/gpgconf") " --list-dirs agent-ssh-socket)")))

(define-public home-gnupg-service-type
  (service-type (name 'home-gnupg)
                (extensions
                 (list (service-extension
			home-environment-vars-service-type
                        add-ssh-agent-socket)
		       (service-extension
			home-profile-service-type
			(lambda (%) `(,gnupg)))))
		(default-value #f)
                (description "Configures and installs gpg-agent.")))

(define-record-type* <home-environment> home-environment
  make-home-environment
  home-environment?
  this-home-environment

  ;; (layout)
  ;; (xdg-dirs)
  (packages home-environment-packages             ; list of (PACKAGE OUTPUT...)
            (default '()))

  (essential-services home-environment-essential-services ; list of services
                      (thunked)
                      (default (home-environment-default-essential-services
                                this-home-environment)))
  (services home-environment-user-services
	    (default '()))
  
  ;; (location home-environment-location             ; <location>
  ;;           (default (and=> (current-source-location)
  ;;                           source-properties->location))
  ;;           (innate))
  )

(define (home-environment-default-essential-services he)
  "Return the list of essential services for home environment."
  (list
   ;; cleanup-service, which will remove links of previous generation?
   ;; home-shepherd-service
   ;; home-activation-service
   ;; home-environment-service
   ;; xdg-configuration
   ;; brightness-service

   (service home-service-type `())
   (service home-environment-vars-service-type `(("TEST_VARI" . "value")))
   (service home-profile-service-type (home-environment-packages he))))

(define* (home-environment-services he)
  "Return all the services of home environment."
  (instantiate-missing-services
   (append (home-environment-user-services he)
           (home-environment-essential-services he))))

(define* (home-environment-derivation he)
  "Return a derivation that builds OS."
  (let* ((services         (home-environment-services he))
         (home (fold-services services
			      #:target-type home-service-type)))
    (service-value home)))

;; home-profile-service-type
;; home-activation-service-type
;; home-shepherd-services-type

;; shepherd-service-type
;; https://specifications.freedesktop.org/autostart-spec/autostart-spec-latest.html


;; Guix home manager intro:
;; https://lists.gnu.org/archive/html/guix-devel/2019-09/msg00218.html
;; Service extension alternatives:
;; https://issues.guix.gnu.org/issue/27155
