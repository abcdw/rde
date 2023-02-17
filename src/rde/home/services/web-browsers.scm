;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 conses <contact@conses.eu>
;;;
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

(define-module (rde home services web-browsers)
  #:use-module (rde serializers lisp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (home-nyxt-configuration
            home-nyxt-extension
            home-nyxt-service-type
            home-nyxt-lisp-configuration
            home-nyxt-lisp-extension
            make-nyxt-service-type))

(define file-likes? (list-of file-like?))
(define serialize-packages empty-serializer)
(define serialize-file-likes empty-serializer)

(define-configuration home-nyxt-configuration
  (nyxt
   (file-like nyxt)
   "The nyxt package to use.")
  (lisp-packages
   (file-likes '())
   "List of Lisp packages to install alongside the configuration.")
  (config-lisp
   (lisp-config '())
   "List of expressions, where each expression can be a Sexp or a Gexp.
Sexp is a Lisp form.  Strings don't require any conversion, but booleans do.
Sexps can contain file-like objects, which are paths to corresponding files in
the store that will be serialized as strings.  Gexps should be string-valued
and their value will be appended to the resulting Lisp file.

See @code{serialize-lisp-config} in the @code{(rde serializers lisp)} module
for more details on the Lisp serialization.

The list of expressions will be interposed with \n and everything will end up
in the @file{config.lisp}.")
  (auto-rules-lisp
   (lisp-config '())
   "List of @code{auto-rules} to toggle modes for URLs matching specific
criteria.  Some examples are shown below:
@lisp
((match-domain \"eff.org\") :included ((dark-mode :visible-in-status-p nil)))
((match-host \"guix.gnu.org\") :excluded (emacs-mode))
@end lisp

Rules consist of a condition for rule activation, which can be one of
@code{match-domain}, @code{match-host}, @code{match-regex}, or @code{match-port},
a user-defined condition, or a quoted URL.

Included modes is a list of mode symbols, or a list of lists in the form of
(MODE-SYMBOL INIT-ARGS), where init-args are passed to the mode when instantiated.

See the @uref{nyxt:manual#auto-rules, auto-rules section} in the Nyxt manual for
more information on how to write these rules."))

(define-configuration home-nyxt-extension
  (lisp-packages
   (file-likes '())
   "List of additional Lisp packages to install alongside the service
extension.")
  (config-lisp
   (lisp-config '())
   "List of expressions to add to @file{config.lisp}.  See
@code{home-nyxt-configuration-config-lisp} for more information.")
  (auto-rules-lisp
   (lisp-config '())
   "List of additional auto rules to add.  See
@code{home-nyxt-configuration-auto-rules-lisp} for more information."))

(define (home-nyxt-files-service config)
  (define (filter-fields field)
    (filter-configuration-fields home-nyxt-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (append
   (if (null? (home-nyxt-configuration-config-lisp config))
       '()
       (list
        `(".config/nyxt/config.lisp"
          ,(mixed-text-file
            "config.lisp"
            (serialize-field 'config-lisp)))))
   (if (null? (home-nyxt-configuration-auto-rules-lisp config))
       '()
       (list
        `(".local/share/nyxt/auto-rules.lisp"
          ,(mixed-text-file
            "auto-rules.lisp"
            (serialize-lisp-config
             'auto-rules-lisp
             `(,#~"("
               ,@(home-nyxt-configuration-auto-rules-lisp config)
               ,#~")"))))))))

(define (home-nyxt-extensions original-config extension-configs)
  (let ((extensions (reverse extension-configs)))
    (home-nyxt-configuration
     (inherit original-config)
     (lisp-packages
      (append (home-nyxt-configuration-lisp-packages original-config)
              (append-map
               home-nyxt-extension-lisp-packages extensions)))
     (config-lisp
      (append (home-nyxt-configuration-config-lisp original-config)
              (append-map
               home-nyxt-extension-config-lisp extensions)))
     (auto-rules-lisp
      (append (home-nyxt-configuration-auto-rules-lisp original-config)
              (append-map
               home-nyxt-extension-auto-rules-lisp extensions))))))

(define (home-nyxt-profile-service config)
  (list (home-nyxt-configuration-nyxt config)))

(define home-nyxt-service-type
  (service-type
   (name 'home-nyxt)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-nyxt-profile-service)
     (service-extension
      home-files-service-type
      home-nyxt-files-service)))
   (compose identity)
   (extend home-nyxt-extensions)
   (default-value (home-nyxt-configuration))
   (description "Install and configure Nyxt, the hacker's power-browser.")))

(define (generate-home-nyxt-documentation)
  (generate-documentation
   `((home-nyxt-configuration
      ,home-nyxt-configuration-fields))
   'home-nyxt-configuration))


;;;
;;; Nyxt configuration
;;;

(define-configuration/no-serialization home-nyxt-lisp-configuration
  (name
   (symbol #f)
   "The name of the configuration package.")
  (config
   (lisp-config '())
   "List of Lisp expressions.  See
@code{home-nyxt-service-type} for more information.")
  (lisp-packages
   (file-likes '())
   "List of additional Lisp packages to install alongside the configuration
package.")
  (auto-rules
   (lisp-config '())
   "List of auto rules.  See
@code{home-nyxt-service-type} for more information."))

(define-configuration home-nyxt-lisp-extension
  (config
   (lisp-config '())
   "List of expressions.  See
@code{home-nyxt-service-type} for more information.")
  (lisp-packages
   (file-likes '())
   "List of additional Lisp packages.")
  (auto-rules
   (lisp-config '())
   "List of auto rules.  See
@code{home-nyxt-service-type} for more information."))

(define (home-nyxt-lisp-extensions original-config extensions)
  (let ((extensions (reverse extensions)))
    (home-nyxt-lisp-configuration
     (inherit original-config)
     (lisp-packages
      (append (home-nyxt-lisp-configuration-lisp-packages original-config)
              (append-map
               home-nyxt-lisp-extension-lisp-packages extensions)))
     (config
      (append (home-nyxt-lisp-configuration-config original-config)
              (append-map
               home-nyxt-lisp-extension-config extensions)))
     (auto-rules
      (append (home-nyxt-lisp-configuration-auto-rules original-config)
              (append-map
               home-nyxt-lisp-extension-auto-rules extensions))))))

(define add-nyxt-lisp-configuration
  (match-lambda
    (($ <home-nyxt-lisp-configuration> name config lisp-packages auto-rules)
     (let* ((file-name (symbol->string name))
            (conf-file (mixed-text-file
                        (string-append file-name ".lisp")
                        (serialize-lisp-config
                         #f
                         `((in-package :nyxt-user)
                           ,@config))))
            (dirname #~(string-append (dirname #$conf-file) "/"))
            (filename #~(basename #$conf-file)))
       (home-nyxt-extension
        (config-lisp
         `(,#~(string-trim-right
               (with-output-to-string
                 (lambda ()
                   ((@ (ice-9 pretty-print) pretty-print)
                    `(define-nyxt-user-system-and-load
                       #$(string->symbol (format #f "nyxt-user/~a" name))
                       #$@(if (null? lisp-packages)
                              '()
                              `(:depends-on (,@lisp-packages)))
                       :config-directory ,#$dirname
                       :components (,#$filename)))))
               #\newline)))
        (lisp-packages lisp-packages)
        (auto-rules-lisp auto-rules))))))

(define (make-nyxt-service-type name)
  (service-type (name name)
                (extensions
                 (list (service-extension
                        home-nyxt-service-type
                        add-nyxt-lisp-configuration)))
                (compose identity)
                (extend home-nyxt-lisp-extensions)
                (default-value #f)
                (description (format #f "\
Create nyxt-~a configuration file which extends Nyxt and ensures the provided
configuration is available/loaded at startup time.  Can be extended with
@code{home-nyxt-lisp-extension}." name))))
