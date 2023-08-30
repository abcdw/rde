;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features uml)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (rde gexp)

  #:export (feature-plantuml))

(define* (feature-plantuml
          #:key
          (emacs-plantuml-mode emacs-plantuml-mode))
  "Configure plantuml and related emacs packages."
  (ensure-pred file-like? emacs-plantuml-mode)

  (define f-name 'plantuml)

  (define (get-home-services config)
    "Returns home services related to Bash."
    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'ob
          (require 'plantuml-mode)
          (org-babel-do-load-languages
           'org-babel-load-languages
           '((plantuml . t)))
          (setq org-plantuml-exec-mode 'plantuml)
          (setq org-plantuml-executable-path plantuml-executable-path))

        (eval-when-compile
         (require 'plantuml-mode))
        (with-eval-after-load 'plantuml-mode
          ;; Needed to make variable definition available compile time
          (plantuml-init-once)
          ;; Don't indent activation/deactivation statements
          (setq plantuml-indent-regexp-start
                (remove plantuml-indent-regexp-activate-start
                        plantuml-indent-regexp-start))
          (setq plantuml-indent-regexp-end
                (remove plantuml-indent-regexp-activate-end
                        plantuml-indent-regexp-end))))
      #:summary "\
Setup plantuml related settings"
      #:commentary "\
Adjustments and tweaks."
      #:keywords '(convenience)
      #:elisp-packages (list emacs-plantuml-mode))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
