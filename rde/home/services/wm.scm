;;; rde --- Reproducible development environment
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (rde home services wm)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu services configuration)
  #:use-module (rde serializers json)
  #:use-module (rde serializers css)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)

  #:export (home-sway-service-type
	    home-sway-configuration

            home-swayidle-service-type
	    home-swayidle-configuration

            home-swaylock-service-type
	    home-swaylock-configuration

            home-waybar-service-type
	    home-waybar-configuration
            home-waybar-extension

            sway-config?))


;;;
;;; sway.
;;;

(define sway-config? list?)
(define (serialize-sway-config val)
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
      ;; TODO: Change it to ((? string? e) (format #f "~s" e))
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
       (append-map
	(lambda (e)
	  (append (list (aligner nestness))
	          (serialize-sway-expression e nestness)))
	expressions))
      (e
       (raise (formatted-message
	       (G_ "Sway (sub)config should be a list of expressions, \
where each expression is also a list, but provided value is:\n ~a") e))) ))

  (serialize-sway-subconfig val))

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

@lisp
`((include ,(local-file \"./sway/config\"))
  (bindsym $mod+Ctrl+Shift+a exec emacsclient -c --eval \"'(eshell)'\")
  (bindsym $mod+Ctrl+Shift+o \"[class=\\\"IceCat\\\"]\" kill)
  (input * ((xkb_layout us,ru)
            (xkb_variant dvorak,))))
@end lisp

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
     ,(apply
       mixed-text-file
       "sway-config"
       (serialize-sway-config (home-sway-configuration-config config))))))

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


;;;
;;; swayidle.
;;;

(define-configuration home-swayidle-configuration
  (swayidle
    (package swayidle)
    "swayidle package to use.")
  (config
   (sway-config
    `())
   "This field has the same format as sway's config field, but in reality
swayidle supports only a subset of sway config, also, make sure
<command> is quoted with single quotes if it has arguments, otherwise
only first part will be interpreted as a command by swayidle.  To get
the complete list of available options see @code{man swayidle}.

The example configuration:

@lisp
((lock \"'swaylock -c 303030'\")
 (before-sleep \"'swaylock -c 303030'\")
 (timeout 300 \"'swaylock -c 303030'\"))
@end lisp"))

(define (add-swayidle-packages config)
  (list (home-swayidle-configuration-swayidle config)))

(define (add-swayidle-configuration config)
  `(("config/swayidle/config"
     ,(apply
       mixed-text-file
       "swayidle-config"
       (serialize-sway-config (home-swayidle-configuration-config config))))))

(define (home-swayidle-extensions cfg extensions)
  (home-swayidle-configuration
   (inherit cfg)
   (config
    (append (home-swayidle-configuration-config cfg)
            (append-map identity (reverse extensions))))))

(define home-swayidle-service-type
  (service-type (name 'home-swayidle)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-swayidle-packages)
		       (service-extension
                        home-files-service-type
                        add-swayidle-configuration)))
		(compose identity)
		(extend home-swayidle-extensions)
                (default-value (home-swayidle-configuration))
                (description "\
Install and configure swayidle, sway's idle management daemon")))


;;;
;;; swaylock.
;;;

(define swaylock-config? list?)

(define (serialize-swaylock-config config)
  (define (serialize-swaylock-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? string? e) e)
      (e
       (raise (formatted-message
               (G_ "Swaylock term should be a non-list value (string, \
symbol). Provided term is:\n ~a") e)))))

  (define (serialize-swaylock-option lst)
    (unless (or (pair? lst) (gexp? lst))
      (raise (formatted-message
              (G_ "Swaylock option must be a gexp or pair, second \
element can be a boolean. Provided term is:\n ~a") lst)))
    (match lst
      ((x . #t) (list (serialize-swaylock-term x) "\n"))
      ((x . #f) (list))
      ((x)      (list (serialize-swaylock-term x) "\n"))
      ((x . y)  (list (serialize-swaylock-term x) "="
                      (serialize-swaylock-term y) "\n"))
      (e (list e "\n"))))
  (append-map serialize-swaylock-option config))

(define-configuration home-swaylock-configuration
  (swaylock
    (package swaylock)
    "swaylock package to use.")
  (config
   (swaylock-config
    '())
   "A list of pairs, each pair represents an option described in
@code{man swaylock}.  The second element of the pair can be a string,
symbol, boolean or omitted.  If second element is omitted or @code{#t}
option is enabled, if @code{#f} disabled, and set to the value
otherwise."))

(define (add-swaylock-packages config)
  (list (home-swaylock-configuration-swaylock config)))

(define (add-swaylock-configuration config)
  `(("config/swaylock/config"
     ,(apply
       mixed-text-file
       "swaylock-config"
       (serialize-swaylock-config
        (home-swaylock-configuration-config config))))))

(define (home-swaylock-extensions cfg extensions)
  (home-swaylock-configuration
   (inherit cfg)
   (config
    (append (home-swaylock-configuration-config cfg)
            (append-map identity (reverse extensions))))))

(define home-swaylock-service-type
  (service-type (name 'home-swaylock)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-swaylock-packages)
		       (service-extension
                        home-files-service-type
                        add-swaylock-configuration)))
		(compose identity)
		(extend home-swaylock-extensions)
                (default-value (home-swaylock-configuration))
                (description "\
Install and configure swaylock, screen locker for Wayland.")))


;;;
;;; waybar.
;;;

;; (print-json
;;  '((ipc . #t)
;;    (ipc . #f)
;;    (battery#bat2 . ((hehe . val)
;;                     (hehe2 . key)))
;;    (modules-left . #("sway/workspaces" module/another))
;;    (modules-left . #("sway/mode"))))

(define waybar-keys-to-merge
  '(modules-left modules-center modules-right))

(define (waybar-key-to-merge? x)


  ;; (define (json-equal? v1 v2)
  ;;   (equal? (->symbol v1) (->symbol v2)))

  (member
   x
   (append
    waybar-keys-to-merge
    (map symbol->string waybar-keys-to-merge))))

(define (->symbol v)
  (if (symbol? v) v (string->symbol v)))

(define (->string v)
  (if (string? v) v (symbol->string v)))

(define (json-assoc-ref alist key)
  (or
   (assoc-ref alist (->symbol key))
   (assoc-ref alist (->string key))))

(define (waybar-merge-configs c1 c2)
  "Appends C2 alist to the end of C1, if key satisfies
waybar-key-to-merge?, merge the values of those keys."


  (unless (equal? (json-assoc-ref c1 'name) (json-assoc-ref c2 'name))
    (throw 'waybar-merge-name-mismatch (list c1 c2)))

  (define updated-c1
    (fold-right
     (lambda (x acc)
       (let* ((key (car x))
              (v1 (cdr x))
              (v2 (json-assoc-ref c2 key)))
         (if (and v2 (waybar-key-to-merge? key))
             (cons (cons key (vector-append v1 v2)) acc)
             (cons x acc))))
     '() c1))

  (define updated-c2
    (fold-right
     (lambda (x acc)
       (let* ((key (car x))
              (v (json-assoc-ref updated-c1 key)))
         (if (and v (waybar-key-to-merge? key))
             acc
             (cons x acc))))
     '() c2))

  (append updated-c1 updated-c2))

;; (print-json
;;  (waybar-merge-configs
;;   '((position . "top")
;;       (name . main)
;;       (modules-left . #(sway/workspaces)))
;;   '((ipc . #t)
;;     (height . 34)
;;     (name . main)
;;     (modules-left . #(sway/mode)))))

(define (waybar-collapse-configs configs)
  "Merge bar configs with the same name."
  (define (index-by-name name vec)
    (vector-index
     (lambda (x)
       (equal? (json-assoc-ref x 'name) name)) vec))

  (vector-fold
   (lambda (i acc x)
     (let* ((name (json-assoc-ref x 'name))
            (ind (index-by-name name acc)))
       (if ind
           (begin
             (vector-set! acc ind (waybar-merge-configs (vector-ref acc ind) x))
             acc)
           (vector-append acc (vector x)))))
   #() configs))

;; (print-json
;;  (waybar-collapse-configs
;;   #(((position . "top")
;;      (name . main)
;;      (modules-left . #(sway/workspaces)))
;;     ((ipc . #t)
;;      (height . 34)
;;      (name . main)
;;      (modules-left . #(sway/mode))))))

(define (waybar-config? config)
  (vector? config))

(define (serialize-waybar-config config)
  (serialize-json config))

(define-configuration home-waybar-configuration
  (waybar
    (file-like waybar)
    "waybar package to use.")
  (config
   (waybar-config
    #())
   "A vector of alists, each alists represent a separate bar
configuration.  Configurations for bars provided by extensions will be
merged by the value of name key, *-modules get appended, the rest just
added to the end of original configuration of the bar.")
  (style-css
   (css-config
    '())
   "Style definitions for waybar look and feel customization."))

(define-configuration home-waybar-extension
  (config
   (waybar-config
    #())
   "A vector of alists, each alists represent a separate bar
configuration.  Configurations for bars provided by extensions will be
merged by the value of name key, *-modules get appended, the rest just
added to the end of original configuration of the bar.")
  (style-css
   (css-config
    '())
   "Additional style definitions for waybar look and feel customization."))

(define (add-waybar-packages config)
  (list (home-waybar-configuration-waybar config)))

(define (home-waybar-extensions cfg extensions)
  (let ((extensions (reverse extensions)))
    (home-waybar-configuration
     (inherit cfg)
     (config
      (waybar-collapse-configs
       (vector-append
        (home-waybar-configuration-config cfg)
        (vector-concatenate
         (map home-waybar-extension-config extensions)))))
     (style-css
      (append
       (home-waybar-configuration-style-css cfg)
       (append-map home-waybar-extension-style-css extensions))))))

(define (add-waybar-configuration config)
  `(("config/waybar/style.css"
     ,(apply
       mixed-text-file
       "waybar-style-css"
       (serialize-css-config (home-waybar-configuration-style-css config))))
    ("config/waybar/config"
     ,(apply
       mixed-text-file
       "waybar-config"
       (serialize-waybar-config (home-waybar-configuration-config config))))))

(define home-waybar-service-type
  (service-type (name 'home-waybar)
                (extensions
                 (list (service-extension
			home-profile-service-type
			add-waybar-packages)
		       (service-extension
                        home-files-service-type
                        add-waybar-configuration)))
		(compose append)
		(extend home-waybar-extensions)
                (default-value (home-waybar-configuration))
                (description "\
Install and configure waybar, a highly customizible wayland bar for
wlroots based compositors.")))
