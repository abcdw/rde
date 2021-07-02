;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu home-services)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix profiles)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

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

;;; Comment:
;;;
;;; This module is similar to (gnu system services) module, but
;;; provides Home Services, which are supposed to be used for building
;;; home-environment.
;;;
;;; Home Services use the same extension as System Services.  Consult
;;; (gnu system services) module or manual for more information.
;;;
;;; Code:


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

;; TODO: Add a list of transformations for packages, it should be here
;; to prevent conflicts, when other packages relies on non-transformed
;; version of package.
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
		;; TODO: It's necessary to source ~/.guix-profile too
		;; on foreign distros
		"\
HOME_ENVIRONMENT=$HOME/.guix-home
GUIX_PROFILE=\"$HOME_ENVIRONMENT/profile\"
PROFILE_FILE=\"$HOME_ENVIRONMENT/profile/etc/profile\"
[ -f $PROFILE_FILE ] && . $PROFILE_FILE

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

"

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
   #~(let* ((he-init-file (lambda (he) (string-append he "/setup-environment")))
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
             (let* ((port   ((@@ (ice-9 popen) open-input-pipe)
		             (format #f "source ~a && env"
                                     (he-init-file new-home))))
	            (result ((@@ (ice-9 rdelim) read-delimited) "" port))
	            (vars (map (lambda (x)
                                 (let ((si (string-index x #\=)))
                                   (cons (string-take x si)
                                         (string-drop x (1+ si)))))
			       ((@@ (srfi srfi-1) remove)
			        string-null?
                                (string-split result #\newline)))))
	       (close-port port)
	       (map (lambda (x) (setenv (car x) (cdr x))) vars)

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
                     new-home)))))

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
                   (ice-9 ftw)
                   (rnrs io ports))

      (define (butlast lst)
        (drop-right lst 1))

      (define (flatten . lst)
        "Return a list that recursively concatenates all sub-lists of LST."
        (define (flatten1 head out)
          (if (list? head)
              (fold-right flatten1 out head)
              (cons head out)))
        (fold-right flatten1 '() lst))

      (let ((gexp-tuples '#$gexps)
            (new-generation (getenv "GUIX_NEW_HOME"))
            (old-generation (getenv "GUIX_OLD_HOME")))
        (define files-to-check (map car gexp-tuples))

        (define (symlink? file)
          (let ((file-info (lstat file)))
            (eq? (vector-ref file-info 13) 'symlink)))

        (define* (readlink* file #:optional (acc 1))
          "Like @code{readlink}, but recursive.  ACC is an accumulator, it is
used to keep track of the number of symlinks that have been followed,
this is to prevent infinite loops.

@example
/path/to/foo -> /path/to/bar
/path/to/bar -> /path/to/baz
/path/to/baz -> /path/to/foo
@end example"
          (cond
           ;; I think 10 is a reasonable number, might change in the future.
           ((or (= acc 10) (not (symlink? file))) file)
           (else (readlink* (readlink file) (+ acc 1)))))

        (define (check-directory dir)
          "Traverse DIR and check whether all the files in DIR are
identical to the ones for the old generation."
          ;; We have DIR, which is
          ;;
          ;; /gnu/store/...-home/some/path/to/dir
          ;;
          ;; and we want to extract /some/path/to/dir.  If DIR has in
          ;; fact changed we just want to return /some/path/to/dir and
          ;; not the full path DIR because /some/path/to/dir is what
          ;; is specified as the path in `gexp-tuples'.
          (define path-from-generation-dir
            (let ((dir-length (string-length new-generation)))
              ;; Also drop starting "/"
              (string-drop dir (+ dir-length 1))))

          (define (filter-file-tree-node node)
            (if (eq? (car node) 'dir)
                '()
                (cdr node)))

          (define (parent-or-current-dir dir)
            (or (string=? dir ".")
                (string=? dir "..")))

          (let ((children (map (lambda (dir)
                                 (string-append
                                  path-from-generation-dir "/" dir))
                               (filter (compose not parent-or-current-dir)
                                       (scandir dir)))))
            (if (any identity (flatten (map check-file children)))
                path-from-generation-dir
                #f)))

        (define (check-file file)
          "Check Whether FILE for the current generation is identical to the one
for the old generation.  If they aren't, return FILE with
the, otherwise, return @code{#f}.  This also works if the FILE is a
directory and the directory itself is a symlink to the store."
          (let ((new-file (string-append new-generation "/" file))
                (old-file (string-append old-generation "/" file)))
            (cond
             ;; If the files don't exist in either generation, don't
             ;; do anything.
             ((and (not (file-exists? new-file))
                   (not (file-exists? old-file)))
              #f)
             ;; If the file exists in one of the generations,
             ;; something has definitely changed.
             ((or (and (not (file-exists? old-file)) (file-exists? new-file))
                  (and (file-exists? old-file) (not (file-exists? new-file))))
              file)
             ;; If the file exists in both generations, check for
             ;; identity.
             ((symlink? new-file)
              (if (string=? (readlink* old-file)
                            (readlink* new-file))
                  #f
                  file))
             (else
              (check-directory new-file)))))

        (when (and old-generation (file-exists? old-generation))
          (let* ((changed-files
                  (map check-file files-to-check))
                 (needed-gexps
                  (lambda (gexp-tuples)
                    (let loop ((acc '())
                               (gexp-tuples gexp-tuples))
                      (cond
                       ((null? gexp-tuples) acc)
                       ((member (caar gexp-tuples) changed-files)
                        (loop (cons (cadar gexp-tuples) acc)
                              (cdr gexp-tuples)))
                       (else (loop acc (cdr gexp-tuples))))))))

            (for-each primitive-eval
                      (needed-gexps gexp-tuples)))))))

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

(define home-provenance-service-type
  (service-type
   (name 'home-provenance)
   (extensions
    (list (service-extension
           home-service-type
           (service-extension-compute
            (first (service-type-extensions provenance-service-type))))))
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
