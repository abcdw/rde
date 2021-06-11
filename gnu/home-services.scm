(define-module (gnu home-services)
  #:use-module (gnu services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages base)
  #:use-module (guix channels)
  #:use-module (guix describe)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix ui)
  #:autoload   (guix openpgp) (openpgp-format-fingerprint)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  ;; #:use-module (guix modules)
  #:use-module ((guix import utils) #:select (flatten))

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)

  #:export (home-service-type
	    home-profile-service-type
	    home-environment-variables-service-type
	    home-run-on-first-login-service-type
	    home-activation-service-type
	    home-provenance-service-type
            home-run-on-change-service-type

            fold-home-service-types)

  #:re-export (service
	       service-type
	       service-extension))

(define (home-derivation entries mextensions)
  "Return as a monadic value the derivation of the 'home'
directory containing the given entries."
  (mlet %store-monad ((extensions (mapm/accumulate-builds identity
                                                          mextensions)))
    (lower-object
     (file-union "home"
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
                 "This is the @dfn{home profile} and can be found in
@file{~/.guix-home/profile}.  It contains packages and
configuration files that the user has declared in their
@code{home-environment} record.")))

(define (environment-variables->setup-environment-script vars)
  "Return a file that can be sourced by a POSIX compliant shell which
initializes the environment.  The file will source the home
environment profile, set some default environment variables, and set
environment variables provided in @code{vars}.  @code{vars} is a list
of pairs (@code{(key . value)}), @code{key} is a string and
@code{value} is a string or gexp.

If value is @code{#f} variable will be omitted.
If value is @code{#t} variable will be just exported.
For any other, value variable will be set to the @code{value} and
exported."
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
		(assoc-ref vars "GUIX_HOME_DIRECTORY")
		;; TODO: It's necessary to source ~/.guix-profile too on foreign distros
		"
GUIX_PROFILE=\"$HOME_ENVIRONMENT/profile\"
. \"$HOME_ENVIRONMENT/profile/etc/profile\"

case $XDG_DATA_DIRS in
  *$HOME_ENVIRONMENT/profile/share*) ;;
  *) export XDG_DATA_DIRS=$HOME_ENVIRONMENT/profile/share:$XDG_DATA_DIRS ;;
esac
case $MANPATH in
  *$HOME_ENVIRONMENT/profile/share/man*) ;;
  *) export MANPATH=$HOME_ENVIRONMENT/profile/share/man:$MANPATH
esac
case $INFOPATH in
  *$HOME_ENVIRONMENT/profile/share/info*) ;;
  *) export INFOPATH=$HOME_ENVIRONMENT/profile/share/info:$INFOPATH ;;
esac
case $XDG_CONFIG_DIRS in
  *$HOME_ENVIRONMENT/profile/etc/xdg*) ;;
  *) export XDG_CONFIG_DIRS=$HOME_ENVIRONMENT/profile/etc/xdg:$XDG_CONFIG_DIRS ;;
esac
case $XCURSOR_PATH in
  *$HOME_ENVIRONMENT/profile/share/icons*) ;;
  *) export XCURSOR_PATH=$HOME_ENVIRONMENT/profile/share/icons:$XCURSOR_PATH ;;
esac

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
                (description "Set the environment variables.")))

(define (compute-on-first-login-script _ gexps)
  (gexp->script
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
                (description "Run gexps on first user login and can be
extended with one gexp.")))

(define (compute-activation-script init-gexp gexps)
  (gexp->script
   "activate"
   #~(begin
       ;; MAYBE: If cwd used, make sure that activation script was
       ;; called from -home store item
       (let* ((he-init-file (lambda (he) (string-append he "/setup-environment")))
              (he-path (string-append (getenv "HOME") "/.guix-home"))
              (new-home-env (getenv "GUIX_NEW_HOME"))
              (new-home (or new-home-env
                            ;; Path of the activation file if called interactively
                            (dirname (car (command-line)))))
              (old-home-env (getenv "GUIX_OLD_HOME"))
              (old-home (or old-home-env
                            (if (file-exists? (he-init-file he-path))
                                (readlink he-path)
                                #f))))
         (if (file-exists? (he-init-file new-home))
             (begin
               ;; Calling init-gexp before setenvs prevents accidential
               ;; GUIX_*_HOME set from setup-environment script
               #$init-gexp

               (setenv "GUIX_NEW_HOME" new-home)
               (setenv "GUIX_OLD_HOME" old-home)

               ;; Atomically make HOME current.

               #$@gexps

               ;; Do not unset env variable if it was set outside.
               (unless new-home-env (setenv "GUIX_NEW_HOME" #f))
               (unless old-home-env (setenv "GUIX_OLD_HOME" #f)))
             (format #t "\
Activation script was either called or loaded by file from this direcotry:
~a
It doesn't seem that home environment is somewhere around.
Make sure that you call ./activate by symlink from -home store item.\n"
                     new-home))))))

(define (activation-script-entry m-activation)
  "Return, as a monadic value, an entry for the activation script
in the home environment directory."
  (mlet %store-monad ((activation m-activation))
    (return `(("activate" ,activation)))))

(define home-activation-service-type
  (service-type (name 'home-activation)
                (extensions
                 (list (service-extension
			home-service-type
                        activation-script-entry)))
                (compose identity)
                (extend compute-activation-script)
		(default-value #f)
                (description "Run gexps to activate the current
generation of home environment and update the state of the home
directory.  @command{activate} script automatically called during
reconfiguration or generation switching.  This service can be extended
with one gexp, but many times, and all gexps must be idempotent.")))


;;;
;;; On-change.
;;;

(define (compute-on-change-gexp _ gexps)
  #~(begin
      (use-modules (srfi srfi-1)
                   (ice-9 popen)
                   (ice-9 match)
                   (rnrs io ports))

      (define (load-tree path)
        (if (file-exists? path)
            (call-with-input-file path
              (lambda (port)
                (read port)))
            #f))

      (define (butlast lst)
        (drop-right lst 1))

      (define rest cdr)

      ;; Copied from (guix import utils)
      ;; TODO: Make upstream move it to (guix build utils)?
      (define (flatten lst)
        "Return a list that recursively concatenates all sub-lists of LST."
        (fold-right
         (match-lambda*
           (((sub-list ...) memo)
            (append (flatten sub-list) memo))
           ((elem memo)
            (cons elem memo)))
         '() lst))

      (let* ((gexp-tuples '#$gexps)
             (tree-file-name "/.guix-home-file-tree")
             (config-home    (or (getenv "XDG_CONFIG_HOME")
                                 (string-append (getenv "HOME") "/.config")))
             (tree-file-path (string-append config-home tree-file-name))
             (current-generation (home-environment-directory))
             (previous-generation (getenv "GUIX_HOME_PREVIOUS_GENERATION"))
             (tree-file (load-tree tree-file-path)))
        (define tree-file-files
          (map (lambda (pair) (rest pair))
               (filter (lambda (pair) (equal? (car pair) 'file))
                       (flatten tree-file))))

        (define (check-file file)
          "Check Whether FILE for the current generation is identical
to the one for the previous generation identical.  If they aren't,
return FILE with the, otherwise, return @code{#f}."
          (let ((old-file-full-path (string-append previous-generation
                                                   "/files/" file))
                (new-file-full-path (string-append current-generation
                                                   "/files/" file)))
            (if (file-exists? old-file-full-path)
                (let* ((pipe (open-pipe*
                              OPEN_READ
                              (string-append #$diffutils "/bin/cmp")
                              old-file-full-path
                              new-file-full-path))
                       (status (eof-object? (read pipe))))
                  (close-pipe pipe)
                  (if status #f file))
                file)))

        (define changed-files
          (map (lambda (file) (check-file file)) tree-file-files))

        (define (needed-gexps gexp-tuples)
          (let loop ((acc '())
                     (gexp-tuples gexp-tuples))
            (cond
             ((null? gexp-tuples) acc)
             ((member (first (first gexp-tuples)) changed-files)
              (loop (cons (second (first gexp-tuples)) acc) (rest gexp-tuples)))
             (else (loop acc (rest gexp-tuples))))))

        (for-each primitive-eval
                  (needed-gexps gexp-tuples)))))

(define home-run-on-change-service-type
  (service-type (name 'home-run-on-change)
                (extensions
                 (list (service-extension
                        home-activation-service-type
                        identity)))
                (compose identity)
                (extend compute-on-change-gexp)
                (default-value '())
                (description "G-expression to run if the specified
file has changed since the last generation.  The G-expression should
be a list where the first element is the file that should be changed,
and the second element is the G-expression to the run.")))


;;;
;;; Provenance tracking.
;;;

;; TODO: Import all provenance functions from services.scm

(define (object->pretty-string obj)
  "Like 'object->string', but using 'pretty-print'."
  (call-with-output-string
    (lambda (port)
      (pretty-print obj port))))

(define (channel->code channel)
  "Return code to build CHANNEL, ready to be dropped in a 'channels.scm'
file."
  ;; Since the 'introduction' field is backward-incompatible, and since it's
  ;; optional when using the "official" 'guix channel, include it if and only
  ;; if we're referring to a different channel.
  (let ((intro (and (not (equal? (list channel) %default-channels))
                    (channel-introduction channel))))
    `(channel (name ',(channel-name channel))
              (url ,(channel-url channel))
              (branch ,(channel-branch channel))
              (commit ,(channel-commit channel))
              ,@(if intro
                    `((introduction
                       (make-channel-introduction
                        ,(channel-introduction-first-signed-commit intro)
                        (openpgp-fingerprint
                         ,(openpgp-format-fingerprint
                           (channel-introduction-first-commit-signer
                            intro))))))
                    '()))))

(define (channel->sexp channel)
  "Return an sexp describing CHANNEL.  The sexp is _not_ code and is meant to
be parsed by tools; it's potentially more future-proof than code."
  ;; TODO: Add CHANNEL's introduction.  Currently we can't do that because
  ;; older 'guix system describe' expect exactly name/url/branch/commit
  ;; without any additional fields.
  `(channel (name ,(channel-name channel))
            (url ,(channel-url channel))
            (branch ,(channel-branch channel))
            (commit ,(channel-commit channel))))

(define (sexp->channel sexp)
  "Return the channel corresponding to SEXP, an sexp as found in the
\"provenance\" file produced by 'provenance-service-type'."
  (match sexp
    (('channel ('name name)
               ('url url)
               ('branch branch)
               ('commit commit)
               rest ...)
     ;; XXX: In the future REST may include a channel introduction.
     (channel (name name) (url url)
              (branch branch) (commit commit)))))

(define (provenance-file channels config-file)
  "Return a 'provenance' file describing CHANNELS, a list of channels, and
CONFIG-FILE, which can be either #f or a <local-file> containing the OS
configuration being used."
  (scheme-file "provenance"
               #~(provenance
                  (version 0)
                  (channels #+@(if channels
                                   (map channel->sexp channels)
                                   '()))
                  (configuration-file #+config-file))))

(define (provenance-entry config-file)
  "Return system entries describing the operating system provenance: the
channels in use and CONFIG-FILE, if it is true."
  (define profile
    (current-profile))

  (define channels
    (and=> profile profile-channels))

  (mbegin %store-monad
    (let ((config-file (cond ((string? config-file)
                              ;; CONFIG-FILE has been passed typically via
                              ;; 'guix system reconfigure CONFIG-FILE' so we
                              ;; can assume it's valid: tell 'local-file' to
                              ;; not emit a warning.
                              (local-file (assume-valid-file-name config-file)
                                          "configuration.scm"))
                             ((not config-file)
                              #f)
                             (else
                              config-file))))
      (return `(("provenance" ,(provenance-file channels config-file))
                ,@(if channels
                      `(("channels.scm"
                         ,(plain-file "channels.scm"
                                      (object->pretty-string
                                       `(list
                                         ,@(map channel->code channels))))))
                      '())
                ,@(if config-file
                      `(("configuration.scm" ,config-file))
                      '()))))))


(define home-provenance-service-type
  (service-type (name 'home-provenance)
                (extensions
                 (list (service-extension home-service-type
                                          provenance-entry)))
                (default-value #f)                ;the HE config file
                (description
                 "Store provenance information about the
home environment in the home environment itself: the channels used
when building the home environment, and its configuration file, when
available.")))

(define (sexp->home-provenance sexp)
  "Parse SEXP, an s-expression read from ~/.guix-home/provenance or
similar, and return two values: the list of channels listed therein, and the
HE configuration file or #f."
  (match sexp
    (('provenance ('version 0)
                  ('channels channels ...)
                  ('configuration-file config-file))
     (values (map sexp->channel channels)
             config-file))
    (_
     (values '() #f))))

(define (home-provenance home-environment)
  "Given HOME-ENVIRONMENT, the file name of a system generation,
return two values: the list of channels HOME-ENVIRONMENT is built
from, and its configuration file.  If that information is missing,
return the empty list (for channels) and possibly #false (for the
configuration file)."
  (catch 'system-error
    (lambda ()
      (sexp->home-provenance
       (call-with-input-file (string-append home-environment "/provenance")
         read)))
    (lambda _
      (values '() #f))))



;;;
;;; Searching
;;;

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
