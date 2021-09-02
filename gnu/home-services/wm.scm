(define-module (gnu home-services wm)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages wm)
  #:use-module (gnu services configuration)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-sway-service-type
	    home-sway-configuration

            home-xmonad-service-type
            home-xmonad-configuration

            sway-config?))

;;; Commentary:
;;;
;;; This module contains services for window managers.
;;;
;;; Code:

;;;
;;; Sway.
;;;

(define sway-config? list?)
(define (serialize-sway-config field-name val)
  (define (aligner nestness)
    (apply string-append
	   (map (const "    ") (iota nestness))))

  (define (serialize-sway-term term)
    ;; (format #t "finval. ~a\n" term)
    (match term
      (#t "yes")
      (#f "no")
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? string? e) e)
      ((lst ...)
       (raise (formatted-message
               (G_ "Sway term should be a non-list value (string, \
boolean, number, symbol, or gexp). Provided term is:\n ~a") lst)))
      (e e)))

  (define* (serialize-sway-expression
	    expr #:optional (nestness 0))
    ;; (format #t "expres. ~a\n" expr)
    (match expr
      ;; subconfig has the same structure as config,
      ;; the only difference: it's not a top-level form
      ;; can be found at the end of expression.
      ;; (term subconfig)
      ((term ((expressions ...) ...))
       ;; (format #t "subtop. ~a . ~a\n" term expressions)
       (append
	(list (serialize-sway-term term) " {\n")
	(serialize-sway-subconfig expressions (1+ nestness))
	`(,(aligner nestness)
          "}\n")))

      ;; subexpression:
      ;; (term . rest)
      ((term rest ..1)
       ;; (format #t "inside. ~a . ~a\n" term rest)
       (cons* (serialize-sway-term term) " "
	      (serialize-sway-expression rest)))

      ;; last element of subexpression
      ((term)
       ;; (format #t "term.   ~a\n" term)
       (list (serialize-sway-term term) "\n"))

      (e
       (raise (formatted-message
               (G_ "Sway expression should be a list of terms \
optionally ending with subconfigs, but provided expression is:\n ~a")
               e)))))

  (define* (serialize-sway-subconfig
	    subconfig #:optional (nestness 0))
    (match subconfig
      ;; config:
      ;; ((expr1) (expr2) (expr3))
      (((expressions ...) ...)
       ;; (format #t "top.    ~a\n" expressions)
       (append-map
	;; It was a quantum behavior, when serialize-sway-expression
	;; was used directly, without wrapping it into lambda. Be aware.
	;; <https://paste.sr.ht/~abcdw/8b4909fbcd0f5fece985afa5913120fd4c5e787c>
	;; <https://logs.guix.gnu.org/guix/2021-04-23.log#093933>
	(lambda (e)
	  (append (list (aligner nestness))
	          (serialize-sway-expression e nestness)))
	expressions))
      (e
       (raise (formatted-message
	       (G_ "Sway (sub)config should be a list of expressions, \
where each expression is also a list, but provided value is:\n ~a") e))) ))

  #~(apply string-append
	   '#$(serialize-sway-subconfig val)))

(define-configuration home-sway-configuration
  (package
    (package sway)
    "Sway package to use.")
  (config
   ;;; TODO: make the default value thunked and use a sway package
   ;;; from package field. Requires patching define-configuration
   ;; `((include ,(file-append
   ;; 		 (home-sway-configuration-package this-home-sway-configuration)
   ;; 		 "/etc/sway/config")))
   (sway-config
    `((include ,(file-append sway "/etc/sway/config"))))
   "List of expressions.  Each @dfn{expression} is a list of terms,
optionally ending in a list of expressions.  A @dfn{term} is a non-list
value: string, boolean, number, symbol, or gexp.

There is no special syntax for CRITERIA (See @command{man 5 sway}),
because sway's doc states it's a string, so use a string for that.

For gradual migration, the file with old config can be included as
shown in the example below.  After that, you can start translate lines
to scheme one by one.

@example
`((include ,(local-file \"./sway/config\"))
  (bindsym $mod+Ctrl+Shift+a exec emacsclient -c --eval \"'(eshell)'\")
  (bindsym $mod+Ctrl+Shift+o \"[class=\\\"IceCat\\\"]\" kill)
  (input * ((xkb_layout us,ru)
            (xkb_variant dvorak,))))
@end example

would yield something like:

@example
include /gnu/store/408jwvh6wxxn1j85lj95fniih05gx5xj-config
bindsym $mod+Ctrl+Shift+a exec emacsclient -c --eval '(eshell)'
bindsym $mod+Ctrl+Shift+o [class=\"IceCat\"] kill
input * {
    xkb_layout us,ru
    xkb_variant dvorak,
}
@end example"))

(define (add-sway-packages config)
  (list (home-sway-configuration-package config)))

(define (add-sway-configuration config)
  `(("config/sway/config"
     ,(mixed-text-file
       "sway-config"
       (serialize-configuration config home-sway-configuration-fields)))))

(define (home-sway-extensions cfg extensions)
  (home-sway-configuration
   (inherit cfg)
   (config
    (append (home-sway-configuration-config cfg)
            (append-map identity (reverse extensions))))))

(define home-sway-service-type
  (service-type (name 'home-sway)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-sway-packages)
		       (service-extension
                        home-files-service-type
                        add-sway-configuration)))
		(compose identity)
		(extend home-sway-extensions)
                (default-value (home-sway-configuration))
                (description "\
Install and configure Sway, a Wayland compositor compatible with i3.")))

(define (generate-home-sway-documentation)
  (generate-documentation
   `((home-sway-configuration
      ,home-sway-configuration-fields))
   'home-sway-configuration))


;;;
;;; XMonad.
;;;

(define-configuration/no-serialization home-xmonad-configuration
  (package
    (package xmonad)
    "The XMonad package to use.")
  (xmonad-contrib?
   (boolean #f)
   "Whether to install the @code{ghc-xmonad-contrib} package, which
contains many third-party extensions for XMonad.")
  (xdg-flavor?
   (boolean #t)
   "Whether to respect XDG base directory, this will set the relevant
environment variables if enabled.")
  (config
   (text-config '())
   "List of strings or gexps containing the XMonad configuration, see
the @uref{https://xmonad.org/documentation.html, official XMonad
documentation} for how to configure it."))

(define xmonad-profile-service
  (match-lambda
    (($ <home-xmonad-configuration> _ package xmonad-contrib?)
     (if xmonad-contrib?
         (list package ghc-xmonad-contrib)
         (list package)))))

(define xmonad-files-service
  (match-lambda
    (($ <home-xmonad-configuration> _ package xmonad-contrib?
                                    xdg-flavor? config)
     (if (null? config)
         '()
         `((,(string-append (if xdg-flavor? "config/" "") "xmonad/xmonad.hs")
            ,(mixed-text-file "xmonad-xmonad.hs"
                              (serialize-text-config #f config))))))))

(define xmonad-run-on-change-service
  (match-lambda
    (($ <home-xmonad-configuration> _ package xmonad-contrib?
                                    xdg-flavor? config)
     `((,(if xdg-flavor? "files/config/xmonad/xmonad.hs" "files/xmonad/xmonad.hs")
        ,#~(let ((executable #$(file-append package "/bin/xmonad")))
             (system* executable"--recompile")
             (system* executable "--restart")))))))

(define home-xmonad-service-type
  (service-type (name 'home-xmonad)
                (extensions
                 ;; TODO: Extend `on-change' service.
                 (list (service-extension
                        home-profile-service-type
                        xmonad-profile-service)
                       (service-extension
                        home-run-on-change-service-type
                        xmonad-run-on-change-service)
                       (service-extension
                        home-files-service-type
                        xmonad-files-service)))
                (description "\
Install and configure XMonad, a window manager written in Haskell.")))

(define (generate-home-xmonad-documentation)
  (generate-documentation
   `((home-xmonad-configuration
      ,home-xmonad-configuration-fields))
   'home-xmonad-configuration))
