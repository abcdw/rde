;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (rde home services emacs-xyz)
  #:use-module (rde serializers elisp)
  #:use-module (rde home services emacs)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services configuration)

  #:use-module (srfi srfi-1)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)

  #:export (home-emacs-tempel-service-type
            home-emacs-tempel-configuration))

(define-configuration/no-serialization home-emacs-tempel-configuration
  (emacs-tempel
   (file-like emacs-tempel)
   "emacs-tempel package to use.")
  (templates-file-name
   (string "templates")
   "File path in @code{user-emacs-directory}.")
  (templates
   (elisp-config '())
   "Templates, as described (@pxref{,,,Tempel})."))

(define (emacs-tempel-templates-file elisp-config)
  (mixed-text-file "tempel-templates"
                   #~"\
;; Local Variables:
;; mode: lisp-data
;; outline-regexp: \"[a-z]\"
;; End:\n\n"
                   (elisp-serialize elisp-config)))

(define (add-emacs-tempel-templates-file config)
  `((,(string-append
       "emacs/" (home-emacs-tempel-configuration-templates-file-name config))
     ,(emacs-tempel-templates-file
       (home-emacs-tempel-configuration-templates config)))))

(define (home-emacs-tempel-extensions config extensions)
  (let ((templates (home-emacs-tempel-configuration-templates config))
        (extensions (interpose (reverse extensions) (list #~""))))
    (home-emacs-tempel-configuration
     (inherit config)
     (templates
      (apply
       append
       templates
       (if (null? templates) '() (list #~""))
       extensions)))))

(define home-emacs-tempel-service-type
  (service-type (name 'home-emacs-tempel)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        add-emacs-tempel-templates-file)))
                (compose identity)
                (extend home-emacs-tempel-extensions)
                (default-value (home-emacs-tempel-configuration))
                (description "TempEL service.")))
