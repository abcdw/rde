;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024 Demis Balbach <db@minikn.xyz>
;;;
;;; This file is a part of rde.
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

(define-module (contrib features json)
  #:use-module (rde features)
  #:use-module (rde features emacs)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (gnu packages web)
  #:use-module (gnu packages tree-sitter)

  #:use-module (rde packages emacs-xyz)

  #:use-module (guix gexp)

  #:export (feature-json))

(define* (feature-json
          #:key
          (jq jq)
          (tree-sitter-json tree-sitter-json))
  "Setup and configure environment for JSON support."
  (ensure-pred file-like? jq)
  (ensure-pred file-like? tree-sitter-json)

  (define (get-home-services config)
    (define emacs-f-name 'json)
    (define feature-javascript (get-value 'javascript config))

    (list
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        ;; Remapping json-mode to json-ts-mode
        `((require 'treesit)
          (when (and (treesit-available-p)
                     (treesit-language-available-p 'json))
            (progn
             (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
             ;; Additionally remapping js-json-mode if `feature-javascript' is
             ;; enabled
             ,@(if feature-javascript
                   '((add-to-list 'major-mode-remap-alist
                                  '(js-json-mode . json-ts-mode)))
                   '())))

          ;; Activating json flymake checker upon enabling json-ts-mode
          (add-hook 'json-ts-mode-hook
	            (lambda ()
	              ;; load json flymake checker
	              (load-library "json-simple-flymake")
	              (json-simple-setup-flymake-backend)

	              ;; Add flymake diagnostics to mode bar
	              (add-to-list 'mode-line-misc-info
			           `(flymake-mode
                                     (" " flymake-mode-line-counters " ")))
	              ;; Enable flymake
	              (flymake-mode t))))
        #:authors '("Demis Balbach <db@minikn.xyz>")
        #:elisp-packages (list emacs-json-simple-flymake)
        #:summary "Preconfigured JSON support"
        #:commentary "\
Enables tree-sitter modes for JSON if Emacs was compiled with
tree-sitter support. Also remaps `js-json-mode' if `feature-javascript'
was enabled. In addition, it also adds linting and flymake
integration for json files.
"
        #:keywords '(convenience editing languages)))
     (simple-service
      'json-add-packages
      home-profile-service-type
      (list jq tree-sitter-json))))
  (feature
   (name 'json)
   (values `((json . #t)))
   (home-services-getter get-home-services)))
