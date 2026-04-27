;;; rde --- Reproducible development environment.
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (rde features ast-grep)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (rde serializers yaml)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages tree-sitter)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:export (feature-ast-grep
            ast-grep-grammar
            make-ast-grep-grammar
            ast-grep-ts-grammar))

;;; Commentary:

;; ast-grep is a structural code search/lint/rewrite tool built on
;; tree-sitter.  It ships with ~24 built-in languages but not Scheme or Elisp.
;;
;; This feature:
;; - Installs ast-grep and provides `rde-sg` wrapper so custom grammars work everywhere.
;; - Bakes sgconfig.yml with custom language definitions into the wrapper.
;; - The wrapper injects `-c <global-config>` only when no project-local
;;   sgconfig.yml is found and the user hasn't passed `-c` themselves.
;;
;; Usage example:
;;   (feature-ast-grep
;;     #:tree-sitter-grammars
;;     (list (ast-grep-ts-grammar
;;            #:grammar tree-sitter-scheme
;;            #:extensions '("scm" "ss" "sls" "sld"))))

(define %ast-grep-subcommands-with-config
  '("run" "scan" "lsp" "test" "new"))

;; Record to hold grammar configuration
(define-record-type* <ast-grep-grammar>
  ast-grep-grammar make-ast-grep-grammar
  ast-grep-grammar?
  (grammar       ast-grep-grammar-grammar)      ; file-like (tree-sitter package)
  (extensions    ast-grep-grammar-extensions)   ; list of strings
  (expando-char  ast-grep-grammar-expando-char  ; string, default "_"
                 (default "$")))

(define* (ast-grep-ts-grammar
          #:key grammar extensions (expando-char "$"))
  "Create an ast-grep grammar configuration.
GRAMMAR is a tree-sitter package (e.g. tree-sitter-scheme).
EXTENSIONS is a list of file extensions (e.g. '(\"scm\" \"ss\")).
EXPANDO-CHAR is the meta-variable character for ast-grep templates."
  (ast-grep-grammar
   (grammar grammar)
   (extensions extensions)
   (expando-char expando-char)))

(define (tree-sitter-grammar-name grammar)
  "Extract grammar name from a tree-sitter package.
E.g. tree-sitter-scheme -> scheme."
  (let ((name (package-name grammar)))
    (if (string-prefix? "tree-sitter-" name)
        (string-drop name (string-length "tree-sitter-"))
        name)))

(define* (feature-ast-grep
          #:key
          ast-grep
          (tree-sitter-grammars
           (list (ast-grep-ts-grammar
                  #:grammar tree-sitter-scheme
                  #:extensions '("scm" "ss" "sls" "sld")))))
  "Setup and configure ast-grep with tree-sitter grammar support.

AST-GREP is the ast-grep package (a file-like).
TREE-SITTER-GRAMMARS is a list of <ast-grep-grammar> records defining
custom languages.  Use MAKE-TREE-SITTER-GRAMMAR to construct entries.

The feature creates an @file{sgconfig.yml} with custom language registrations
and wraps @command{sg} to inject it when no project config is found."
  (ensure-pred file-like? ast-grep)
  (ensure-pred list? tree-sitter-grammars)
  (for-each
   (lambda (grammar)
     (ensure-pred ast-grep-grammar? grammar))
   tree-sitter-grammars)

  (define f-name 'ast-grep)

  (define grammar-entries
    (map
     (lambda (grammar)
       (let ((grammar-pkg (ast-grep-grammar-grammar grammar))
             (extensions (ast-grep-grammar-extensions grammar))
             (expando (ast-grep-grammar-expando-char grammar)))
         (cons (tree-sitter-grammar-name grammar-pkg)
               `((libraryPath . ,(file-append
                                  grammar-pkg
                                  "/lib/tree-sitter/libtree-sitter-"
                                  (tree-sitter-grammar-name grammar-pkg)
                                  ".so"))
                 (extensions . ,extensions)
                 (expandoChar . ,expando)))))
     tree-sitter-grammars))

  (define sgconfig
    (mixed-text-file
     "sgconfig.yml"
     #~(string-append
        "customLanguages:\n"
        #$@(map
            (lambda (entry)
              (let ((name (car entry))
                    (library (assoc-ref entry 'libraryPath))
                    (extensions (assoc-ref entry 'extensions))
                    (expando (assoc-ref entry 'expandoChar)))
                #~(string-append
                   "  " #$name ":\n"
                   "    libraryPath: " #$library "\n"
                   "    extensions: [" #$(string-join extensions ", ") "]\n"
                   "    expandoChar: \"" #$expando "\"\n")))
            grammar-entries))))

  (define rde-sg-cmd
    (program-file
     "rde-sg"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (ice-9 match))

           (define sg #$(file-append ast-grep "/bin/sg"))
           (define global-config #$sgconfig)

           (define (has-config-flag? args)
             (let loop ((args args))
               (match args
                 (() #f)
                 (((or "-c" "--config") . _) #t)
                 ((_ . rest) (loop rest)))))

           (define (find-project-config)
             (let loop ((dir (getcwd)))
               (cond
                ((file-exists? (string-append dir "/sgconfig.yml")) #t)
                ((string=? dir "/") #f)
                (else (loop (dirname dir))))))

           (define (subcommand-accepts-config? cmd)
             (member cmd '#$%ast-grep-subcommands-with-config))

           (let ((args (cdr (command-line))))
             (match args
               (((? subcommand-accepts-config? subcmd) . rest)
                (if (or (has-config-flag? rest)
                        (find-project-config))
                    (apply execl sg "sg" subcmd rest)
                    (apply execl sg "sg" subcmd "-c" global-config rest)))
               (_ (apply execl sg "sg" args))))))))

  (define rde-sg-script
    (computed-file
     "rde-sg-script"
     #~(begin
         (use-modules (guix build utils))
         (mkdir #$output)
         (mkdir-p (string-append #$output "/bin"))
         (symlink #$rde-sg-cmd (string-append #$output "/bin/rde-sg")))))

  (define (get-home-services config)
    (list
     (simple-service
      'ast-grep-packages
      home-profile-service-type
      (list ast-grep))
     (simple-service
      'ast-grep-wrapper
      home-files-service-type
      `((".local/bin/rde-sg" ,rde-sg-cmd)))))

  (feature
   (name f-name)
   (values `((,f-name . ,ast-grep)
             (rde-sg-script . ,rde-sg-script)))
   (home-services-getter get-home-services)))
