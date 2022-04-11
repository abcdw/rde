(define-module (gnu home-services xorg)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages xorg)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix diagnostics)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)

  #:export (home-xresources-service-type
            home-xresources-configuration
            xresources-file))

;;; Commentary:
;;;
;;; Services related to Xorg.
;;;
;;; Code:

;;;
;;; X resources.
;;;
;;; (home-xresources-configuration
;;;  (config
;;;   `((include . ,(xresources-file "urxvt-xresources"
;;;                                  `((font.size . 34))))
;;;     (*headingFont . "DejaVu Sans Mono")
;;;     (Xcursor.size . 13))))
;;;

(define-configuration/no-serialization home-xresources-configuration
  (package
    (package xrdb)
    "Package to use for setting
@uref{https://en.wikipedia.org/wiki/X_resources,X resources}.")
  (config
   (alist '())
   "Association list of key and value pairs for the Xresources file.
The value can be a string, G-expression, boolean, number or list
strings.

Below is an example configuraiton:

@example
(home-xresources-configuration
 (config
  `((Xft.antialias . #t)
    (XTerm.termName . \"xterm-256color\")
    (URxvt.secondaryScroll . 0)
    (define . (black . \"#000000\"))
    (include . ,(local-file \"/path/to/legacy/config\"))
    (include . ,(xresources-file \"urxvt-config\"
                                 `((URxvt*scrollTtyKeypress . #t))')))))
@end example

It would result in:

@example
Xft.antialias: true
XTerm.termName: xterm-256color
URxvt.secondaryScroll: 0
#define black #00000
#include \"/gnu/store/...-legacy-config\"
#include \"/gnu/store/...-urxvt-config\"
@end example"))

(define (xresources-file filename config)
  (apply mixed-text-file
         filename
         (serialize-xresources-config config)))

(define (serialize-xresources-config config)
  (define (format-config key val)
    (let* ((val (cond
                 ((pair? val)           ;for #define directives
                  (string-append (maybe-object->string (car val))
                                 " "
                                 (maybe-object->string (cdr val))))
                 ((computed-file? val)  ;from `xresources-file'
                  #~(string-append "\"" #$val "\""))
                 ((list? val)
                  ;; TODO: Support list of gexps.
                  ;;  (with-imported-modules (source-module-closure
                  ;;                          '((gnu home-services-utils)))
                  ;;    #~(begin
                  ;;        (use-modules (gnu home-services-utils))
                  ;;        (string-join (map maybe-object->string #$val) ","))))
                  (string-join (map maybe-object->string val) ","))
                 ((boolean? val)
                  (if val "true" "false"))
                 ((or (symbol? val) (number? val))
                  (maybe-object->string val))
                 (else val)))           ;gexps most likely
           (key (cond
                 ((or (equal? key 'include) (equal? key 'define))
                  (format #f "#~a" key))
                 (else (string-append (maybe-object->string key) ":")))))
      (list key " " val "\n")))

  (define (check-duplicates alist)
    "Check if there are any duplicate keys in the alist pairs."
    (let loop ((alist alist)
               (acc '()))
      (cond
       ((null? alist) acc)
       (else (let* ((head (first alist))
                    (tail (cdr alist))
                    (key (first head))
                    (duplicate? (assoc key acc)))
               (if duplicate?
                   (raise (formatted-message
                           (G_ "\
`home-xresources-configuration' contains duplicate value for `~a'")
                           key))
                   (loop tail (cons head acc))))))))

  (generic-serialize-alist append format-config (check-duplicates config)))

(define (home-xresources-files-service config)
  `(("Xresources"
     ,(apply mixed-text-file
             "Xresources"
             (serialize-xresources-config
              (home-xresources-configuration-config config))))))

(define (home-xresources-profile-service config)
  (list (home-xresources-configuration-package config)))

(define (home-xresources-run-on-change-service config)
  `(("files/.Xresources"
     ,#~(begin
          (display "Reloading Xresources\n")
          (system* #$(file-append (home-xresources-configuration-package config)
                                  "/bin/xrdb")
                   "-load"
                   (string-append (getenv "GUIX_NEW_HOME")
                                  "/files/.Xresources"))))))

(define (home-xresources-extension old-config extension-configs)
  (match old-config
    (($ <home-xresources-configuration> _ package* config*)
     (home-xresources-configuration
      (package package*)
      (config (append config*
                      (append-map home-xresources-configuration-config
                                  extension-configs)))))))

(define home-xresources-service-type
  (service-type (name 'home-xresources)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-xresources-files-service)
                       (service-extension
                        home-profile-service-type
                        home-xresources-profile-service)
                       (service-extension
                        home-run-on-change-service-type
                        home-xresources-run-on-change-service)))
                (compose concatenate)
                (extend home-xresources-extension)
                (default-value (home-xresources-configuration))
                (description "Configure X resources.")))

(define (generate-home-xresources-documentation)
  (generate-documentation
   `((home-xresources-configuration
      ,home-xresources-configuration-fields))
   'home-xresources-configuration))
