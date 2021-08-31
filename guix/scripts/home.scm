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

(define-module (guix scripts home)
  #:use-module (gnu packages admin)
  #:use-module ((gnu services) #:hide (delete))
  #:use-module (gnu packages)
  #:use-module (gnu home)
  #:use-module (gnu home-services)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix ui)
  #:use-module (guix grafts)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix scripts package)
  #:use-module (guix scripts build)
  #:use-module (guix scripts system search)
  #:autoload   (guix scripts pull) (channel-commit-hyperlink)
  #:use-module (guix scripts home import)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (guix-home))


;;;
;;; Options.
;;;

(define %user-module
  (make-user-module '((gnu home))))

(define %guix-home
  (string-append %profile-directory "/guix-home"))

(define (show-help)
  (display (G_ "Usage: guix home [OPTION ...] ACTION [ARG ...] [FILE]
Build the home environment declared in FILE according to ACTION.
Some ACTIONS support additional ARGS.\n"))
    (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   search             search for existing service types\n"))
  (display (G_ "\
   reconfigure        switch to a new home environment configuration\n"))
  (display (G_ "\
   roll-back          switch to the previous home environment configuration\n"))
  (display (G_ "\
   describe           describe the current home environment\n"))
  (display (G_ "\
   list-generations   list the home environment generations\n"))
  (display (G_ "\
   switch-generation  switch to an existing home environment configuration\n"))
  (display (G_ "\
   delete-generations delete old home environment generations\n"))
  (display (G_ "\
   build              build the home environment without installing anything\n"))
  (display (G_ "\
   import             generates a home environment definition from dotfiles\n"))

  (show-build-options-help)
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (verbosity-level opts)
  "Return the verbosity level based on OPTS, the alist of parsed options."
  (or (assoc-ref opts 'verbosity)
      (if (eq? (assoc-ref opts 'action) 'build)
          2 1)))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix show")))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         %standard-build-options))

(define %default-options
  `((build-mode . ,(build-mode normal))
    (graft? . #t)
    (substitutes? . #t)
    (offload? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (verbosity . 3)
    (debug . 0)))


;;;
;;; Actions.
;;;

(define* (perform-action action he
                         #:key
                         dry-run?
                         derivations-only?
                         use-substitutes?)
  "Perform ACTION for home environment. "

  (define println
    (cut format #t "~a~%" <>))

  (mlet* %store-monad
      ((he-drv   (home-environment-derivation he))
       (drvs     (mapm/accumulate-builds lower-object (list he-drv)))
       (%        (if derivations-only?
                     (return
                      (for-each (compose println derivation-file-name) drvs))
                     (built-derivations drvs)))

       (he-out-path -> (derivation->output-path he-drv)))
    (if (or dry-run? derivations-only?)
        (return #f)
        (begin
          (for-each (compose println derivation->output-path) drvs)

          (case action
            ((reconfigure)
             (let* ((number (generation-number %guix-home))
                    (generation (generation-file-name
                                 %guix-home (+ 1 number))))

               (switch-symlinks generation he-out-path)
               (switch-symlinks %guix-home generation)
               (setenv "GUIX_NEW_HOME" he-out-path)
               (primitive-load (string-append he-out-path "/activate"))
               (setenv "GUIX_NEW_HOME" #f)
               (return he-out-path)))
            (else
             (newline)
             (return he-out-path)))))))

(define (process-action action args opts)
  "Process ACTION, a sub-command, with the arguments are listed in ARGS.
ACTION must be one of the sub-commands that takes a home environment
declaration as an argument (a file name.)  OPTS is the raw alist of options
resulting from command-line parsing."
  (define (ensure-home-environment file-or-exp obj)
    (unless (home-environment? obj)
      (leave (G_ "'~a' does not return a home environment ~%")
             file-or-exp))
    obj)

  (let* ((file   (match args
                   (() #f)
                   ((x . _) x)))
         (expr   (assoc-ref opts 'expression))
         (system (assoc-ref opts 'system))

         (transform   (lambda (obj)
                        (home-environment-with-provenance obj file)))

         (home-environment
          (transform
           (ensure-home-environment
            (or file expr)
            (cond
             ((and expr file)
              (leave
               (G_ "both file and expression cannot be specified~%")))
             (expr
              (read/eval expr))
             (file
              (load* file %user-module
                     #:on-error (assoc-ref opts 'on-error)))
             (else
              (leave (G_ "no configuration specified~%")))))))

         (dry?        (assoc-ref opts 'dry-run?)))

    (with-store store
      (set-build-options-from-command-line store opts)
      (with-build-handler (build-notifier #:use-substitutes?
                                          (assoc-ref opts 'substitutes?)
                                          #:verbosity
                                          (verbosity-level opts)
                                          #:dry-run?
                                          (assoc-ref opts 'dry-run?))

        (run-with-store store
          (mbegin %store-monad
            (set-guile-for-build (default-guile))

            (case action
              (else
               (perform-action action home-environment
                               #:dry-run? dry?
                               #:derivations-only? (assoc-ref opts 'derivations-only?)
                               #:use-substitutes? (assoc-ref opts 'substitutes?))
               ))))))
    (warn-about-disk-space)))


(define (process-command command args opts)
  "Process COMMAND, one of the 'guix home' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (define-syntax-rule (with-store* store exp ...)
    (with-store store
      (set-build-options-from-command-line store opts)
      exp ...))
  (case command
    ;; The following commands do not need to use the store, and they do not need
    ;; an home environment file.
    ((search)
     (apply search args))
    ((import)
     (let* ((profiles (delete-duplicates
                      (match (filter-map (match-lambda
                                           (('profile . p) p)
                                           (_              #f))
                                         opts)
                        (() (list %current-profile))
                        (lst (reverse lst)))))
           (manifest (concatenate-manifests
                      (map profile-manifest profiles))))
       (import-manifest manifest (current-output-port))))
    ((describe)
     (match (generation-number %guix-home)
       (0
        (error (G_ "no home environment generation, nothing to describe~%")))
       (generation
        (display-home-environment-generation generation))))
    ((list-generations)
     (let ((pattern (match args
                      (() #f)
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (list-generations pattern)))
    ((switch-generation)
     (let ((pattern (match args
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
                    (switch-to-home-environment-generation store pattern))))
    ((roll-back)
     (let ((pattern (match args
                      (() "")
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
                    (roll-back-home-environment store))))
    ((delete-generations)
     (let ((pattern (match args
                      (() #f)
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store*
        store
        (delete-matching-generations store %guix-home pattern))))
    (else (process-action command args opts))))

(define-command (guix-home . args)
  (synopsis "build and deploy home environments")

  (define (parse-sub-command arg result)
    ;; Parse sub-command ARG and augment RESULT accordingly.
    (if (assoc-ref result 'action)
        (alist-cons 'argument arg result)
        (let ((action (string->symbol arg)))
          (case action
            ((build
              reconfigure
              extension-graph shepherd-graph
              list-generations describe
              delete-generations roll-back
              switch-generation search
              import)
             (alist-cons 'action action result))
            (else (leave (G_ "~a: unknown action~%") action))))))

  (define (match-pair car)
    ;; Return a procedure that matches a pair with CAR.
    (match-lambda
      ((head . tail)
       (and (eq? car head) tail))
      (_ #f)))

  (define (option-arguments opts)
    ;; Extract the plain arguments from OPTS.
    (let* ((args   (reverse (filter-map (match-pair 'argument) opts)))
           (count  (length args))
           (action (assoc-ref opts 'action))
           (expr   (assoc-ref opts 'expression)))
      (define (fail)
        (leave (G_ "wrong number of arguments for action '~a'~%")
               action))

      (unless action
        (format (current-error-port)
                (G_ "guix home: missing command name~%"))
        (format (current-error-port)
                (G_ "Try 'guix home --help' for more information.~%"))
        (exit 1))

      (case action
        ((build reconfigure)
         (unless (or (= count 1)
                     (and expr (= count 0)))
           (fail)))
        ((init)
         (unless (= count 2)
           (fail))))
      args))

  (with-error-handling
    (let* ((opts     (parse-command-line args %options
                                         (list %default-options)
                                         #:argument-handler
                                         parse-sub-command))
           (args     (option-arguments opts))
           (command  (assoc-ref opts 'action)))
      (parameterize ((%graft? (assoc-ref opts 'graft?)))
        (with-status-verbosity (verbosity-level opts)
          (process-command command args opts))))))


;;;
;;; Searching.
;;;

(define service-type-name*
  (compose symbol->string service-type-name))

(define (service-type-description-string type)
  "Return the rendered and localised description of TYPE, a service type."
  (and=> (service-type-description type)
         (compose texi->plain-text P_)))

(define %service-type-metrics
  ;; Metrics used to estimate the relevance of a search result.
  `((,service-type-name* . 3)
    (,service-type-description-string . 2)
    (,(lambda (type)
        (match (and=> (service-type-location type) location-file)
          ((? string? file)
           (basename file ".scm"))
          (#f
           "")))
     . 1)))

(define (find-service-types regexps)
  "Return a list of service type/score pairs: service types whose name or
description matches REGEXPS sorted by relevance, and their score."
  (let ((matches (fold-home-service-types
                  (lambda (type result)
                    (match (relevance type regexps
                                      %service-type-metrics)
                      ((? zero?)
                       result)
                      (score
                       (cons (cons type score) result))))
                  '())))
    (sort matches
          (lambda (m1 m2)
            (match m1
              ((type1 . score1)
               (match m2
                 ((type2 . score2)
                  (if (= score1 score2)
                      (string>? (service-type-name* type1)
                                (service-type-name* type2))
                      (> score1 score2))))))))))

(define (search . args)
  (with-error-handling
    (let* ((regexps (map (cut make-regexp* <> regexp/icase) args))
           (matches (find-service-types regexps)))
      (leave-on-EPIPE
       (display-search-results matches (current-output-port)
                               #:print service-type->recutils
                               #:command "guix home search")))))


;;;
;;; Generations.
;;;

(define* (display-home-environment-generation
          number
          #:optional (profile %guix-home))
  "Display a summary of home-environment generation NUMBER in a
human-readable format."
  (define (display-channel channel)
    (format #t     "    ~a:~%" (channel-name channel))
    (format #t (G_ "      repository URL: ~a~%") (channel-url channel))
    (when (channel-branch channel)
      (format #t (G_ "      branch: ~a~%") (channel-branch channel)))
    (format #t (G_ "      commit: ~a~%")
            (if (supports-hyperlinks?)
                (channel-commit-hyperlink channel)
                (channel-commit channel))))

  (unless (zero? number)
    (let* ((generation  (generation-file-name profile number)))
      (define-values (channels config-file)
        ;; The function will work for home environments too, we just
        ;; need to keep provenance file.
        (system-provenance generation))

      (display-generation profile number)
      (format #t (G_ "  file name: ~a~%") generation)
      (format #t (G_ "  canonical file name: ~a~%") (readlink* generation))
      ;; TRANSLATORS: Please preserve the two-space indentation.

      (unless (null? channels)
        ;; TRANSLATORS: Here "channel" is the same terminology as used in
        ;; "guix describe" and "guix pull --channels".
        (format #t (G_ "  channels:~%"))
        (for-each display-channel channels))
      (when config-file
        (format #t (G_ "  configuration file: ~a~%")
                (if (supports-hyperlinks?)
                    (file-hyperlink config-file)
                    config-file))))))

(define* (list-generations pattern #:optional (profile %guix-home))
  "Display in a human-readable format all the home environment
generations matching PATTERN, a string.  When PATTERN is #f, display
all the home environment generations."
  (cond ((not (file-exists? profile))             ; XXX: race condition
         (raise (condition (&profile-not-found-error
                            (profile profile)))))
        ((not pattern)
         (for-each display-home-environment-generation (profile-generations profile)))
        ((matching-generations pattern profile)
         =>
         (lambda (numbers)
           (if (null-list? numbers)
               (exit 1)
               (leave-on-EPIPE
                (for-each display-home-environment-generation numbers)))))))


;;;
;;; Switch generations.
;;;

;; TODO: Make it public in (guix scripts system)
(define-syntax-rule (unless-file-not-found exp)
  (catch 'system-error
    (lambda ()
      exp)
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))

(define (switch-to-home-environment-generation store spec)
  "Switch the home-environment profile to the generation specified by
SPEC.  STORE is an open connection to the store."
  (let* ((number (relative-generation-spec->number %guix-home spec))
         (generation (generation-file-name %guix-home number))
         (activate (string-append generation "/activate")))
    (if number
        (begin
          (setenv "GUIX_NEW_HOME" (readlink generation))
          (switch-to-generation* %guix-home number)
          (unless-file-not-found (primitive-load activate))
          (setenv "GUIX_NEW_HOME" #f))
        (leave (G_ "cannot switch to home environment generation '~a'~%") spec))))


;;;
;;; Roll-back.
;;;

(define (roll-back-home-environment store)
  "Roll back the home-environment profile to its previous generation.
STORE is an open connection to the store."
  (switch-to-home-environment-generation store "-1"))
