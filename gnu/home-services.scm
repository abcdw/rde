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
  #:use-module (guix sets)
  #:use-module (guix ui)
  #:use-module (guix discovery)
  #:use-module (guix diagnostics)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-service-type
            home-profile-service-type
            home-environment-variables-service-type
            home-files-service-type
            home-run-on-first-login-service-type
            home-activation-service-type
            home-run-on-change-service-type
            home-provenance-service-type

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
;;; home-service-type is a root of home services DAG.
;;;
;;; home-profile-service-type is almost the same as profile-service-type, at least
;;; for now.
;;;
;;; home-environment-variables-service-type generates a @file{setup-environment}
;;; shell script, which is expected to be sourced by login shell or other program,
;;; which starts early and spawns all other processes.  Home services for shells
;;; automatically add code for sourcing this file, if person do not use those home
;;; services they have to source this script manually in their's shell *profile
;;; file (details described in the manual).
;;;
;;; home-files-service-type is similar to etc-service-type, but doesn't extend
;;; home-activation, because deploy mechanism for config files is pluggable and
;;; can be different for different home environments: The default one is called
;;; symlink-manager (will be introudced in a separate patch series), which creates
;;; links for various dotfiles (like $XDG_CONFIG_HOME/$APP/...) to store, but is
;;; possible to implement alternative approaches like read-only home from Julien's
;;; guix-home-manager.
;;;
;;; home-run-on-first-login-service-type provides an @file{on-first-login} guile
;;; script, which runs provided gexps once, when user makes first login.  It can
;;; be used to start user's Shepherd and maybe some other process.  It relies on
;;; assumption that /run/user/$UID will be created on login by some login
;;; manager (elogind for example).
;;;
;;; home-activation-service-type provides an @file{activate} guile script, which
;;; do three main things:
;;;
;;; - Sets environment variables to the values declared in
;;; @file{setup-environment} shell script.  It's necessary, because user can set
;;; for example XDG_CONFIG_HOME and it should be respected by activation gexp of
;;; symlink-manager.
;;;
;;; - Sets GUIX_NEW_HOME and possibly GUIX_OLD_HOME vars to paths in the store.
;;; Later those variables can be used by activation gexps, for example by
;;; symlink-manager or run-on-change services.
;;;
;;; - Run all activation gexps provided by other home services.
;;;
;;; home-run-on-change-service-type allows to trigger actions during
;;; activation if file or directory specified by pattern is changed.
;;;
;;; Code:


(define (home-derivation entries mextensions)
  "Return as a monadic value the derivation of the 'home'
directory containing the given entries."
  (mlet %store-monad ((extensions (mapm/accumulate-builds identity
                                                          mextensions)))
    (lower-object
     (file-union "home" (append entries (concatenate extensions))))))

(define home-service-type
  ;; This is the ultimate service type, the root of the home service
  ;; DAG.  The service of this type is extended by monadic name/item
  ;; pairs.  These items end up in the "home-environment directory" as
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
                                     (map identity
                                     ;;(options->transformation transformations)
                                     (delete-duplicates packages eq?))))))))))

;; MAYBE: Add a list of transformations for packages.  It's better to
;; place it in home-profile-service-type to affect all profile
;; packages and prevent conflicts, when other packages relies on
;; non-transformed version of package.
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
       ;; TODO: It's necessary to source ~/.guix-profile too
       ;; on foreign distros
       ,(apply mixed-text-file "setup-environment"
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

(define (files->files-directory files)
  "Return a @code{files} directory that contains FILES."
  (define (assert-no-duplicates files)
    (let loop ((files files)
               (seen (set)))
      (match files
        (() #t)
        (((file _) rest ...)
         (when (set-contains? seen file)
           (raise (formatted-message (G_ "duplicate '~a' entry for files/")
                                     file)))
         (loop rest (set-insert file seen))))))

  ;; Detect duplicates early instead of letting them through, eventually
  ;; leading to a build failure of "files.drv".
  (assert-no-duplicates files)

  (file-union "files" files))

(define (files-entry files)
  "Return an entry for the @file{~/.guix-home/files}
directory containing FILES."
  (with-monad %store-monad
    (return `(("files" ,(files->files-directory files))))))

(define home-files-service-type
  (service-type (name 'home-files)
                (extensions
                 (list (service-extension home-service-type
                                          files-entry)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description "Configuration files for programs that
will be put in @file{~/.guix-home/files}.")))

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
                (description "Run gexps on first user login.  Can be
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
           (let* ((port   ((@ (ice-9 popen) open-input-pipe)
                           (format #f "source ~a && env -0"
                                   (he-init-file new-home))))
                  (result ((@ (ice-9 rdelim) read-delimited) "" port))
                  (vars (map (lambda (x)
                               (let ((si (string-index x #\=)))
                                 (cons (string-take x si)
                                       (string-drop x (1+ si)))))
                             ((@ (srfi srfi-1) remove)
                              string-null?
                              (string-split result #\nul)))))
             (close-port port)
             (map (lambda (x) (setenv (car x) (cdr x))) vars)

             (setenv "GUIX_NEW_HOME" new-home)
             (setenv "GUIX_OLD_HOME" old-home)

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

(define (compute-on-change-gexp eval-gexps? pattern-gexp-tuples)
  #~(begin
      (define (equal-regulars? file1 file2)
        "Check if FILE1 and FILE2 are bit for bit identical."
        (let* ((cmp-binary #$(file-append
                              (@ (gnu packages base) diffutils) "/bin/cmp"))
               (stats1     (lstat file1))
               (stats2     (lstat file2)))
          (cond
           ((= (stat:ino stats1) (stat:ino stats2))         #t)
           ((not (= (stat:size stats1) (stat:size stats2))) #f)

           (else (= (system* cmp-binary file1 file2) 0)))))

      (define (equal-symlinks? symlink1 symlink2)
        "Check if SYMLINK1 and SYMLINK2 are pointing to the same target."
        (string=? (readlink symlink1) (readlink symlink2)))

      (define (equal-directories? dir1 dir2)
        "Check if DIR1 and DIR2 have the same content."
        (define (ordinary-file file)
          (not (or (string=? file ".")
                   (string=? file ".."))))
        (let* ((files1 (scandir dir1 ordinary-file))
               (files2 (scandir dir2 ordinary-file)))
          (if (equal? files1 files2)
              (map (lambda (file)
                     (equal-files?
                      (string-append dir1 "/" file)
                      (string-append dir2 "/" file)))
                   files1)
              #f)))

      (define (equal-files? file1 file2)
        "Compares files, symlinks or directories of the same type."
        (case (file-type file1)
          ((directory) (equal-directories? file1 file2))
          ((symlink) (equal-symlinks? file1 file2))
          ((regular) (equal-regulars? file1 file2))
          (else
           (display "The file type is unsupported by on-change service.\n")
           #f)))

      (define (file-type file)
        (stat:type (lstat file)))

      (define (something-changed? file1 file2)
        (cond
         ((and (not (file-exists? file1))
               (not (file-exists? file2))) #f)
         ((or  (not (file-exists? file1))
               (not (file-exists? file2))) #t)

         ((not (eq? (file-type file1) (file-type file2))) #t)

         (else
          (not (equal-files? file1 file2)))))

      (define expressions-to-eval
        (map
         (lambda (x)
           (let* ((file1 (string-append
                          (or (getenv "GUIX_OLD_HOME")
                              "/gnu/store/non-existing-generation")
                          "/" (car x)))
                  (file2 (string-append (getenv "GUIX_NEW_HOME") "/" (car x)))
                  (_ (format #t "Comparing ~a and\n~10t~a..." file1 file2))
                  (any-changes? (something-changed? file1 file2))
                  (_ (format #t " done (~a)\n"
                             (if any-changes? "changed" "same"))))
             (if any-changes? (cadr x) "")))
         '#$pattern-gexp-tuples))

      (if #$eval-gexps?
          (begin
            (display "Evaling on-change gexps.\n\n")
            (for-each primitive-eval expressions-to-eval)
            (display "On-change gexps evaluation finished.\n\n"))
          (display "\
On-change gexps won't be evaluated, disabled by service
configuration.\n"))))

(define home-run-on-change-service-type
  (service-type (name 'home-run-on-change)
                (extensions
                 (list (service-extension
                        home-activation-service-type
                        identity)))
                (compose concatenate)
                (extend compute-on-change-gexp)
                (default-value #t)
                (description "\
G-expressions to run if the specified files have changed since the
last generation.  The extension should be a list of lists where the
first element is the pattern for file or directory that expected to be
changed, and the second element is the G-expression to be evaluated.")))


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
   (description "\
Store provenance information about the home environment in the home
environment itself: the channels used when building the home
environment, and its configuration file, when available.")))

(define sexp->home-provenance sexp->system-provenance)
(define home-provenance system-provenance)


;;;
;;; Searching
;;;

(define (parent-directory directory)
  "Get the parent directory of DIRECTORY"
  (string-join (drop-right (string-split directory #\/) 1) "/"))

(define %guix-home-root-directory
  ;; Absolute file name of the module hierarchy.
  (parent-directory (dirname (search-path %load-path "gnu/home-services.scm"))))

(define %service-type-path
  ;; Search path for service types.
  (make-parameter `((,%guix-home-root-directory . "gnu/home-services"))))

(define (all-home-service-modules)
  "Return the default set of home-service modules."
  (cons (resolve-interface '(gnu home-services))
        (all-modules (%service-type-path)
                     #:warn warn-about-load-error)))

(define* (fold-home-service-types proc seed)
  (fold-service-types proc seed (all-home-service-modules)))
