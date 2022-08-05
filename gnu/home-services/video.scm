(define-module (gnu home-services video)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages video)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (mpv-profile
            home-mpv-configuration
            home-mpv-service-type)
  #:re-export (alist?))

;;; Commentary:
;;;
;;; This module contains services related to video playback and
;;; editing.
;;;
;;; Code:


;;;
;;; mpv.
;;;

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (if (string-suffix? "?" str)
        (string-drop-right str 1)
        str)))

(define (serialize-name field-name val)
  (format #f "\n[~a]\n" val))

(define (serialize-field field-name val)
  (cond
   ((boolean? val) (serialize-boolean field-name val))
   ((symbol? val) (serialize-symbol field-name val))
   (else #~(format #f "~a=~a\n" #$(uglify-field-name field-name) #$val))))

(define (serialize-boolean field-name val)
  (serialize-field field-name (boolean->yes-or-no val)))

(define (serialize-symbol field-name val)
  (serialize-field field-name (symbol->string val)))

(define (serialize-alist field-name val)
  #~(string-append #$@(map (match-lambda
                             ((key . val) (serialize-field key val)))
                           val)))

(define-configuration mpv-profile
  (name
   (string)
   "The name of the mpv profile."
   serialize-name)
  (options
   (alist '())
   "An association list of options to set in the mpv profile.  The format
is the same as the @code{options} field in
@code{home-mpv-configuration}."))

(define (serialize-list-of-mpv-profiles field-name val)
  #~(string-append #$@(map (lambda (config)
                             (serialize-configuration
                              config mpv-profile-fields))
                           val)))

(define (serialize-bindings field-name val)
  #~(string-append #$@(map (match-lambda
                             ((key . val) #~(string-append #$key " " #$val "\n")))
                           val)))

(define list-of-mpv-profiles?
  (list-of mpv-profile?))

(define-configuration home-mpv-configuration
  (package
   (package mpv)
   "The mpv package to use.")
  (default-options
    (alist '())
    "An assocation list of top-level configuration options to set in the
@file{$XDG_CONFIG_HOME/mpv/mpv.conf} file.")
  (profiles
   (list-of-mpv-profiles '())
   "A list of @code{mpv-profile} records for configuring mpv profiles.")
  (bindings
   (alist '())
   "An association list of keybindings to set for mpv."
   serialize-bindings))

(define (mpv-files-service config)
  `(("mpv/mpv.conf"
     ,(mixed-text-file
       "mpv-mpv.conf"
       (serialize-configuration
        config (filter-configuration-fields
                home-mpv-configuration-fields '(bindings) #t))))
    ("mpv/input.conf"
     ,(mixed-text-file
       "mpv-input.conf"
       (serialize-configuration
        config (filter-configuration-fields
                home-mpv-configuration-fields '(bindings)))))))

(define (mpv-profile-service config)
  (list (home-mpv-configuration-package config)))

(define home-mpv-service-type
  (service-type (name 'home-mpv)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        mpv-files-service)
                       (service-extension
                        home-profile-service-type
                        mpv-profile-service)))
                (description "Install and configure mpv")))

(define (generate-home-mpv-documentation)
  (generate-documentation
   `((home-mpv-configuration
      ,home-mpv-configuration-fields
      (mpv-profile home-mpv-profile-configuration))
     (mpv-profile
      ,mpv-profile-fields))
   'home-mpv-configuration))
