;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023, 2025 Nicolas Graves <ngraves@ngraves.fr>
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
  #:use-module (rde predicates)
  #:use-module (rde packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages guile)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde packages guile-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (substitute-keyword-arguments))
  #:export (feature-guile
            feature-shepherd))

(define* (feature-guile
          #:key
          (guile guile-next)
          (emacs-arei emacs-arei-latest)
          (guile-ares-rs guile-ares-rs-latest))
  "Configure tooling and environment for GNU Guile."

  (define f-name 'guile)

  (define (get-home-services config)
    "Return home services related to Guile."
    (list
     (simple-service
      'add-guile-package
      home-profile-service-type
      (list (get-value 'guile config)
            (get-value 'guile-ares-rs config)))
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

      #:elisp-packages (list (get-value 'emacs-arei config))
      #:keywords '(guile)
      #:summary "Configure Guile-related packages"
      #:commentary "\
Provide interactive and functional programming environment for Guile.")))

  (feature
   (name f-name)
   (values `((guile . ,guile)
             (guile-ares-rs . ,guile-ares-rs)
             (emacs-arei . ,emacs-arei)))
   (home-services-getter get-home-services)))

(define (rde-patch-shepherd shepherd)
  (package
    (inherit shepherd)
    (source
     (origin
       (inherit (package-source shepherd))
       (patches
        (parameterize
            ((%patch-path %rde-patch-path))
          (search-patches
           "shepherd-set-user-log-dir-to-XDG_STATE_HOME-log.patch")))))
    ;; See #77795 guix upstream bug.
    (arguments
     (substitute-keyword-arguments (package-arguments shepherd)
       ((#:tests? enabled? #f)
        #f)))))

(define* (feature-shepherd
          #:key
          (shepherd (rde-patch-shepherd shepherd-1.0)))
  "Configure tooling and environment for GNU Shepherd."
  (ensure-pred file-like? shepherd)

  (define f-name 'shepherd)

  (define (get-home-services config)
    "Return home services related to Guile."
    (list
     (service home-shepherd-service-type
              (home-shepherd-configuration
               (shepherd
                ((package-input-rewriting/spec
                  `(("guile" . ,(const (get-value 'guile config guile-3.0)))))
                 (get-value 'shepherd config)))
               (auto-start? #f)
               (daemonize? #f)))

     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'info-look
          (info-lookup-add-help
           :mode 'scheme-mode
           :regexp "[^()`',\"        \n]+"
           :ignore-case t
           :doc-spec '(("(shepherd) Procedure and Macro Index" nil nil nil)
                       ("(shepherd) Variable Index" nil nil nil)))))
      #:keywords '(guile)
      #:summary "Configure Shepherd-related packages"
      #:commentary "\
Provide interactive and functional programming environment for Shepherd.")))

  (feature
   (name f-name)
   (values `((,f-name . ,shepherd)
             (shepherd-launch
              . ,(program-file
                  "launch-shepherd"
                  #~(let* ((state-dir (or (getenv "XDG_STATE_HOME")
                                          (format #f "~a/.local/state"
                                                  (getenv "HOME"))))
                           (log-dir (string-append state-dir "/log")))
                      ((@ (guix build utils) mkdir-p) log-dir)
                      (system*
                       #$(file-append shepherd "/bin/shepherd")
                       "--logfile"
                       (string-append log-dir "/shepherd.log")))))))
   (home-services-getter get-home-services)))
