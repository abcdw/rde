;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde home services emacs)
  #:use-module (rde serializers elisp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services configuration)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 pretty-print)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix utils)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (home-emacs-service-type
            home-emacs-configuration
            home-emacs-extension
            elisp-configuration-package

            home-emacs-feature-loader-configuration
            home-emacs-feature-loader-service-type

            home-elisp-configuration
            home-elisp-extension
            make-home-elisp-service-type))

(define list-of-file-likes? (list-of file-like?))
(define list-of-symbols? (list-of symbol?))

(define (maybe-string? x)
  (or (string? x) (not x)))

(define serialize-boolean empty-serializer)
(define serialize-list empty-serializer)
(define serialize-list-of-file-likes empty-serializer)

;; TODO: Implement native compilation
;; https://git.sr.ht/~whereiseveryone/guixrus/tree/master/item/guixrus/home/services/emacs.scm#L1
(define-configuration home-emacs-configuration
  (emacs
   (file-like emacs)
   "Emacs package to use.")
  (elisp-packages
   (list-of-file-likes '())
   "List of Emacs Lisp packages to install.")
  (rebuild-elisp-packages?
   (boolean #f)
   "Rebuild Emacs Lisp packages with version of Emacs specified in
EMACS field.")
  (emacs-servers
   (list '(server))
   "List of emacs named servers.  Use can use @command{emacsclient -s
SERVER-NAME} to connect to the server (@pxref{Emacs Server,,,emacs.info}).
@code{server} is a default emacs server name, which can be used with
@command{emacsclient} without @command{-s} option.")
  (xdg-flavor?
   (boolean #t)
   "Whether to place all the configuration files in
@file{$XDG_CONFIG_HOME/emacs}.")
  (init-el
   (elisp-config '())
   "List of expressions, each expression can be a Sexp or Gexp.

Sexp is a Emacs Lisp form, preferably valid.  Be aware, if you include
values of Guile variables, they won't be automatically converted to
Elisp.  Strings doesn't require conversion, but for example booleans
do: @code{#t} -> @code{t}, @code{#f} -> @code{nil}.  Be careful here.

However, Sexp can contain file-like objects; String with path to a
corresponding file will appear in place of each such object.  See an
example below for more details.

Gexp should be string-valued.  The value of Gexp will be appended to
resulting Emacs Lisp file.

The list of expressions will be interposed with \\n and everything
will end up in @file{init.el}.

@example
(let ((guile-bool-value #f))
  (home-emacs-configuration
   (init-el
    `((setq rg-binary ,(file-append ripgrep \"/bin/rg\"))
      (load-file ,(local-file \"./emacs/test-init.el\"))
      \"just a string\"
      ;; Make sure you converted guile values to Elisp
      (setq tmp-boolean ,(if guile-bool-value 't 'nil))
      ,(if guile-bool-value '(setq v1 nil) '(setq v2 t))

      ,#~\"\\n;;; Section with gexps results:\"

      ,(slurp-file-gexp (local-file \"./emacs/test-init.el\"))
      ,#~(string-append \"(princ \" \"'hello)\")
      ,#~\"\\n\"
      ,#~\";; Another comment\"))))
@end example

would yield something like:

@example
(setq rg-binary
      \"/gnu/store/dw884p9d2jb83j4fqvdj2i10fn9xgwqd-ripgrep-12.1.1/bin/rg\")
(load-file
  \"/gnu/store/9b1s48crng5dy9xmxskcdnillw18bkg2-test-init.el\")
\"just a string\"
(setq tmp-boolean nil)
(setq v2 t)

;;; Section with gexps results:
;; Here is
\"a sample\"
;; content of test-init.el

(princ 'hello)


;; Another comment
@end example")
  (early-init-el
   (elisp-config '())
   "List of expressions, each expression can be a Sexp or Gexp.
Same as @code{init-el}, but result will go to @file{early-init.el}."))


(define ((update-emacs-argument-for-package target-emacs) p)
  "Set @code{#:emacs} to EMACS-PACKAGE for package P.  To build elisp
packages with proper GNU Emacs version."
  (if (equal?
       (package-build-system p)
       emacs-build-system)
      (package
        (inherit p)
        (arguments
         (substitute-keyword-arguments (package-arguments p)
           ((#:emacs e #f) target-emacs))))
      p))

(define (emacs-argument-updater target-emacs)
  "Recursively updates @code{#:emacs} argument for package and all the
inputs."
  ;; Alternative solution compilation with other emacs version
  ;; https://yhetil.org/20221021192458.4956-1-paren@disroot.org
  (package-mapping (update-emacs-argument-for-package target-emacs)
                   (lambda (p) #f)))

(define (updated-elisp-packages config)
  (let* ((emacs-package  (home-emacs-configuration-emacs config))
         (elisp-packages (home-emacs-configuration-elisp-packages config))

         (updated-elisp-packages
          (if (home-emacs-configuration-rebuild-elisp-packages? config)
              (map (emacs-argument-updater emacs-package)
                   elisp-packages)
              elisp-packages)))
    updated-elisp-packages))

(define (add-emacs-packages config)
  (append (updated-elisp-packages config)
          ;; It's important for packages to go first to override
          ;; built-in emacs packages in case of collisions
          (list (home-emacs-configuration-emacs config))))

(define (emacs-shepherd-service config name)
  (shepherd-service
   (documentation
    (format #f "Emacs server.  Use ~a to connect to it."
            (if (eq? 'server name)
                "@code{emacsclient}"
                (format #f "@code{emacsclient -s ~a}" name))))
   (provision `(,(symbol-append 'emacs- name)))
   (requirement '(emacs))
   (start #~(make-forkexec-constructor
             (list #$(file-append
                      (home-emacs-configuration-emacs config)
                      "/bin/emacs") #$(format #f "--fg-daemon=~a" name))
             #:log-file (string-append
                         (getenv "XDG_STATE_HOME") "/log"
                         "/emacs-" #$(symbol->string name) ".log")))
   (stop #~(make-kill-destructor))))

(define (add-emacs-shepherd-service config)
  (if (not (null? (home-emacs-configuration-emacs-servers config)))
      (cons
       (shepherd-service
        (documentation "\
Emacs metaservice.  Can be used to restart all emacs servers.")
        (provision '(emacs))
        (start #~(const #t))
        (stop #~(const #t)))
       (map (cut emacs-shepherd-service config <>)
            (home-emacs-configuration-emacs-servers config)))
      '()))

;; (define* (mixed-text-file name #:rest text)
;;   "Return an object representing store file NAME containing TEXT.  TEXT is a
;; sequence of strings and file-like objects, as in:

;;   (mixed-text-file \"profile\"
;;                    \"export PATH=\" coreutils \"/bin:\" grep \"/bin\")

;; This is the declarative counterpart of 'text-file*'."
;;   (define build
;;     (gexp (call-with-output-file (ungexp output "out")
;;             (lambda (port)
;;               ;; TODO: Upstream the fix?
;;               (set-port-encoding! port "UTF-8")
;;               (display (string-append (ungexp-splicing text)) port)))))

;;   (computed-file name build))

(define (get-emacs-configuration-files config)
  (let* ((xdg-flavor? (home-emacs-configuration-xdg-flavor? config)))
    (define prefix-file
      (cut string-append
        (if xdg-flavor?
            "emacs/"
            ".emacs.d/")
        <>))

    (define (filter-fields field)
      (filter-configuration-fields home-emacs-configuration-fields
                                   (list field)))

    (define (serialize-field field)
      (serialize-configuration
       config
       (filter-fields field)))

    (define (file-if-not-empty field)
      (let ((file-name (string-append
                        (string-drop-right (symbol->string field) 3)
                        ".el"))
            (field-obj (car (filter-fields field))))
        (if (not (null? ((configuration-field-getter field-obj) config)))
            `(,(prefix-file file-name)
              ,(mixed-text-file
                file-name
                (serialize-field field)))
            '())))

    (filter
     (compose not null?)
     (list
      (file-if-not-empty 'init-el)
      (file-if-not-empty 'early-init-el)))))

(define (add-emacs-dot-configuration config)
  (if (home-emacs-configuration-xdg-flavor? config)
      '()
      (get-emacs-configuration-files config)))

(define (add-emacs-xdg-configuration config)
  (if (home-emacs-configuration-xdg-flavor? config)
      (get-emacs-configuration-files config)
      '()))

(define-configuration home-emacs-extension
  (elisp-packages
   (list-of-file-likes '())
   "List of additional Emacs Lisp packages.")
  (emacs-servers
   (list '())
   "List of emacs servers to add to @code{emacs-servers}.  See
@code{home-emacs-service-type} for more information.")
  (init-el
   (elisp-config '())
   "List of expressions to add to @code{init-el}.  See
@code{home-emacs-service-type} for more information.")
  (early-init-el
   (elisp-config '())
   "List of expressions to add to @code{ealy-init-el}.  See
@code{home-emacs-service-type} for more information."))

(define (home-emacs-extensions original-config extensions)
  (let ((extensions (reverse extensions)))
    (home-emacs-configuration
     (inherit original-config)
     (elisp-packages
      (append (home-emacs-configuration-elisp-packages original-config)
              (append-map
               home-emacs-extension-elisp-packages extensions)))
     (emacs-servers
      (append (home-emacs-configuration-emacs-servers original-config)
              (append-map
               home-emacs-extension-emacs-servers extensions)))
     (init-el
      (append (home-emacs-configuration-init-el original-config)
              (append-map
               home-emacs-extension-init-el extensions)))
     (early-init-el
      (append (home-emacs-configuration-early-init-el original-config)
              (append-map
               home-emacs-extension-early-init-el extensions))))))


(define home-emacs-service-type
  (service-type (name 'home-emacs)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        add-emacs-packages)
                       (service-extension
                        home-shepherd-service-type
                        add-emacs-shepherd-service)
                       (service-extension
                        home-files-service-type
                        add-emacs-dot-configuration)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        add-emacs-xdg-configuration)))
                (compose identity)
                (extend home-emacs-extensions)
                (default-value (home-emacs-configuration))
                (description "Install and configure GNU Emacs, the
extensible, self-documenting editor.")))

(define (generate-home-emacs-documentation)
  (generate-documentation
   `((home-emacs-configuration
      ,home-emacs-configuration-fields))
   'home-emacs-configuration))


(define* (elisp-configuration-package
          package-name elisp-expressions
          #:key
          summary authors maintainers url keywords commentary
          (elisp-packages '())
          (autoloads? #f))
  "Takes a list of Elisp expressions, creates emacs-NAME package.
When autoloads? is @code{#t} adds @code{#~\";;;###autoload\"} before each
Elisp expression to make it evaluated on Emacs startup."

  (define (package->package-input pkg)
    (list ((@ (guix packages) package-name) pkg) pkg))

  (define (add-autoloads elisp-expressions)
    (fold-right
     (lambda (e acc)
       (if (list? e)
           (cons* #~";;;###autoload" e #~"" acc)
           (cons* e #~"" acc)))
     '() elisp-expressions))

  (package
   (name (string-append "emacs-" package-name))
   (version "1.0.0")
   (build-system emacs-build-system)
   (source
    (mixed-text-file
     (string-append package-name ".el")
     (serialize-elisp-config
      #f
      (append
       ;; <https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html>
       (list #~(format #f ";;; ~a.el --- ~a ~a\n" #$package-name
                       (or #$summary "No description provided")
                       "-*- lexical-binding:t -*-"))
       (if (and=> authors (compose not null?))
           (list #~(format #f ";; Author: ~a\n;;"
                           #$(string-join authors "\n;;         ")))
           '())
       (if (and=> maintainers (compose not null?))
           (list #~(format #f ";; Maintainer: ~a\n;;"
                           #$(string-join maintainers "\n;;             ")))
           '())
       (if url (list #~(format #f ";; URL: ~a" #$url)) '())
       (if (and=> keywords (compose not null?))
           (list #~#$(format #f ";; Keywords: ~a"
                             (string-join (map object->string keywords) ", ")))
           '())
       (if (and=> commentary (compose not null?))
           (list #~"\n;;; Commentary:\n"
                 #~#$(string-join
                      (map
                       (lambda (x)
                         (if (string-null? x)
                         x
                         (string-append ";; " x)))
                       (string-split commentary #\newline)) "\n"))
           '())
       (list #~"\n;;; Code:\n")
       ((if autoloads? add-autoloads identity) elisp-expressions)
       (list `(provide ',(string->symbol package-name))
             #~#$(format #f"\n;;; ~a.el ends here" package-name))))))
   (propagated-inputs (map package->package-input elisp-packages))
   (synopsis (or summary "Generated Emacs configuration package"))
   (description "Package generated by @code{elisp-configuration-package}.")
   (home-page "https://www.gnu.org/software/guix/")
   (license license:gpl3+)))

(define-configuration home-emacs-feature-loader-configuration
  (feature-entries
   (list '())
   "List of pairs (feature . ,list-of-packages).  All requires will be added
to a feature-loader package, which can be required in init.el or loaded some
other way.")
  (loader-feature-name
   (symbol 'feature-loader)
   "Name of emacs feature.")
  (autoloads?
   (boolean #f)
   "Add autoload cookies to requires in feature-loader package, which basically
makes the whole feature-loader loaded on startup, however the implementation
details can be changed later.")
  (add-to-init-el?
   (boolean #f)
   "Add require for loader feature to init.el.")
  (no-serialization))

(define (home-emacs-feature-loader-packages config)
  (let* ((add-to-init-el?
          (home-emacs-feature-loader-configuration-add-to-init-el? config))
         (autoloads?
          (home-emacs-feature-loader-configuration-autoloads? config))
         (loader-feature-name
          (home-emacs-feature-loader-configuration-loader-feature-name config))
         (feature-entries
          (home-emacs-feature-loader-configuration-feature-entries config))
         (loader-package (elisp-configuration-package
                          (symbol->string loader-feature-name)
                          (append
                           `(,#~";;;###autoload"
                             (defun ,loader-feature-name ()
                               (interactive)
                               ,@(map (lambda (x)
                                        `(require ',(car x)))
                                      feature-entries)
                               (message "Everything should be loaded now.")))
                           (if autoloads?
                               `(,#~";;;###autoload"
                                 (,loader-feature-name))
                               '()))
                          #:elisp-packages (append-map cdr feature-entries)
                          #:autoloads? #f
                          #:summary "Just loads all necessary features."
                          #:commentary "\
Depending on configuration it either loads features, when required or do it
automatically with autoload cookies.")))

    (when (and autoloads? add-to-init-el?)
      (raise (formatted-message
              (G_ "autoloads? and add-to-init-el? should NOT be used at the same \
time."))))
    (home-emacs-extension
     (init-el (if add-to-init-el?
                  `((require ',loader-feature-name))
                  '()))
     (elisp-packages
      (list loader-package)))))

(define (home-emacs-feature-loader-extensions config extensions)
  (home-emacs-feature-loader-configuration
   (inherit config)
   (feature-entries
    (append
     (home-emacs-feature-loader-configuration-feature-entries config)
     (reverse extensions)))))

(define home-emacs-feature-loader-service-type
  (service-type (name 'home-emacs-feature-loader)
                (extensions
                 (list (service-extension
                        home-emacs-service-type
                        home-emacs-feature-loader-packages)))
                (compose identity)
                (extend home-emacs-feature-loader-extensions)
                (default-value (home-emacs-feature-loader-configuration))
                (description "Extends emacs with feature-loader elisp package,
which have specified packages as propagated inputs, loads specified emacs
feaures and optionally adds a require of itself to init-el.")))


;;;
;;; Elisp configuration.
;;;

(define-configuration/no-serialization home-elisp-configuration
  (name
   (symbol #f)
   "A name of the configuration package.")
  (config
   (elisp-config '())
   "List of expressions.  See
@code{home-emacs-service-type} for more information.")
  (early-init
   (elisp-config '())
   "List of expressions.  See
@code{home-emacs-service-type} for more information.")
  (elisp-packages
   (list-of-file-likes '())
   "List of additional Emacs Lisp packages.")
  (autoloads?
   (boolean #f)
   "Add autoload cookies to all items in config.  Usually not needed as
feature-loader take care of it.")
  (summary
   (maybe-string #f)
   "A brief summary of the configuration package.")
  (commentary
   (maybe-string #f)
   "A brief description of the configuration package.")
  (keywords
   (list-of-symbols '())
   "A list of keyword for the configuration package.")
  (url
   (maybe-string "https://trop.in/rde")
   "A url of the configuration package.")
  (authors
   (list-of-strings '("Andrew Tropin <andrew@trop.in>"))
   "A list of authors of configuration package."))

(define-configuration home-elisp-extension
  (config
   (elisp-config '())
   "List of expressions.  See
@code{home-emacs-service-type} for more information.")
  (elisp-packages
   (list-of-file-likes '())
   "List of additional Emacs Lisp packages."))

(define (home-elisp-feature config)
  (let ((name (home-elisp-configuration-name config)))
    (cons
     name
     (list
      (elisp-configuration-package
       (symbol->string name)
       (home-elisp-configuration-config config)
       #:elisp-packages (home-elisp-configuration-elisp-packages config)
       #:autoloads? (home-elisp-configuration-autoloads? config)
       #:summary (home-elisp-configuration-summary config)
       #:commentary (home-elisp-configuration-commentary config)
       #:keywords (home-elisp-configuration-keywords config)
       #:url (home-elisp-configuration-url config)
       #:authors (home-elisp-configuration-authors config))))))

(define (home-elisp-emacs-extension config)
  (home-emacs-extension
   (early-init-el (home-elisp-configuration-early-init config))
   ;; It's necessary to explicitly add elisp-packages here, because
   ;; we want to overwrite builtin emacs packages.  Propagated
   ;; inputs have lowest priority on collisions, that's why we have
   ;; to list those package here in addition to propagated-inputs.
   (elisp-packages (home-elisp-configuration-elisp-packages config))))

(define (home-elisp-extensions original-config extensions)
  (let ((extensions (reverse extensions)))
    (home-elisp-configuration
     (inherit original-config)
     (elisp-packages
      (append (home-elisp-configuration-elisp-packages original-config)
              (append-map
               home-elisp-extension-elisp-packages extensions)))
     (config
      (append (home-elisp-configuration-config original-config)
              (append-map
               home-elisp-extension-config extensions))))))

(define (make-home-elisp-service-type name)
  (service-type (name name)
                (extensions
                 (list (service-extension
                        home-emacs-feature-loader-service-type
                        home-elisp-feature)
                       (service-extension
                        home-emacs-service-type
                        home-elisp-emacs-extension)))
                (compose identity)
                (extend home-elisp-extensions)
                (default-value #f)
                (description (format #f "\
Creates emacs-~a configuration package, extends emacs and feature-loader to
make provided configuration available/loaded at startup time.  Can be extended
with home-elisp-extension."
                                     name))))
