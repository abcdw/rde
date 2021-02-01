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


(define (environment-variables->environment-vars-file vars)
  "Return a file that can be sourced by bash/zsh that contains
environment variables VARS."

  (with-monad %store-monad
    (return
     `(("environment-vars.sh"
	,(apply mixed-text-file "environment-vars"
		(append-map (match-lambda
			      ((key . value)
                               (list "export " key "=" value "\n")))
			    vars)))))))

(define home-environment-vars-service-type
  (service-type (name 'home-environment-vars)
                (extensions
                 (list (service-extension
			home-service-type
                        environment-variables->environment-vars-file)))
                (compose concatenate)
                (extend append)
                (description "Sets the environment variables on first
login.")))


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
