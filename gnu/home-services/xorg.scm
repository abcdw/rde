(define-module (gnu home-services xorg)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages xorg)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
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
            home-xresources-configuration))

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
;;;   `((*headingFont . "-*-fixed-bold-r-*-*-*-100-*-*-*-*-iso8859-1")
;;;     (?.Dialog.headingFont . "-*-fixed-bold-r-*-*-*-100-*-*-*-*-iso8859-1")
;;;     (Xcursor.size . 3)
;;;     (XTerm.vt100.utf8 . #t))))

(define (serialize-alist field-name val) "")

(define-configuration home-xresources-configuration
  (package
    (package xrdb)
    "Package to use for setting
@uref{https://en.wikipedia.org/wiki/X_resources,X resources}.")
  (config
   (alist '())
   "Association list of key and value pairs for the Xresources file.
The value can be a string, G-expression, boolean, number or list
strings.  The following example:

@example
(home-xresources-configuration
 (config
  '((Xft.antialias . #t)
    (XTerm.termName . \"xterm-256color\")
    (URxvt.secondaryScroll . 0))))
@end example

would yield:

@example
Xft.antialias: true
XTerm.termName: xterm-256color
URxvt.secondaryScroll: 0
@end example"))

(define (serialize-xresources-config config)
  (define (format-config key val)
    (let ((val (cond
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
                (else val)))            ;gexps most likely
          (key (maybe-object->string key)))
      (list key ": " val "\n")))

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

  (check-duplicates '((key1 . val1) (key2 . val2)))

  (generic-serialize-alist append format-config (check-duplicates config)))

(define (home-xresources-files-service config)
  `(("Xresources"
     ,(apply mixed-text-file
       "Xresources"
       (serialize-xresources-config
        (home-xresources-configuration-config config))))))

(define (home-xresources-profile-service config)
  (list (home-xresources-configuration-package config)))

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
                        home-xresources-profile-service)))
                (compose concatenate)
                (extend home-xresources-extension)
                (default-value (home-xresources-configuration))
                (description "Configure X resources.")))

(define (generate-home-xresources-documentation)
  (generate-documentation
   `((home-xresources-configuration
      ,home-xresources-configuration-fields))
   'home-xresources-configuration))
