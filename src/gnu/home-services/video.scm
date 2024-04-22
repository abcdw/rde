(define-module (gnu home-services video)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages video)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
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

(define-deprecated/alias mpv-profile #f)
(define-deprecated/alias home-mpv-configuration
  (@ (rde home services video) home-mpv-configuration))
(define-deprecated/alias home-mpv-service-type
  (@ (rde home services video) home-mpv-service-type))
