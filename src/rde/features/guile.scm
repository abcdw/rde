;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (rde features guile)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu packages guile)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix gexp)
  #:export (feature-guile))

(define* (feature-guile
          #:key
          (guile guile-next)
          (emacs-arei emacs-arei)
          (guile-ares-rs guile-ares-rs))
  "Configure tooling and environment for GNU Guile."

  (define f-name 'guile)

  (define (get-home-services config)
    "Return home services related to Guile."
    (list
     (simple-service
      'add-guile-package
      home-profile-service-type
      (list guile guile-ares-rs))
     (simple-service
      'guile-xdg-base-dirs-specification
      home-environment-variables-service-type
      '(("GUILE_HISTORY" . "$XDG_STATE_HOME/guile_history")))
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'info-look
         (info-lookup-add-help
          :mode 'scheme-mode
          :regexp "[^()`',\"        \n]+"
          :ignore-case t
          :doc-spec '(("(r5rs)Index" nil "^[ 	]+-+ [^:]+:[ 	]*" "\\b")
                      ;; TODO: Check what rest nil arguments do
                      ("(Guile)Procedure Index" nil nil nil)
                      ("(Guile)Variable Index" nil nil nil)
                      ("(Guix)Programming Index" nil nil nil))))
        (with-eval-after-load 'minions
          (setopt minions-prominent-modes '(arei-mode)))
        (require 'arei))

      #:elisp-packages (list emacs-arei)
      #:keywords '(guile)
      #:summary "Configure Guile-related packages"
      #:commentary "\
Provide interactive and functional programming environment for Guile.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
