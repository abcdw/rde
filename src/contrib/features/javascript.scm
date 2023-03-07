;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (contrib features javascript)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages node)
  #:use-module (contrib packages node-xyz)

  #:use-module (guix gexp)

  #:export (feature-javascript))

(define* (feature-javascript
          #:key
          (node node)
          (node-typescript node-typescript)
          (node-typescript-language-server node-typescript-language-server)
          (eglot-stay-out-of '()))
  "Setup and configure environment for JavaScript."
  (ensure-pred file-like? node-typescript)
  (ensure-pred file-like? node-typescript-language-server)

  (define (get-home-services config)
    (define emacs-f-name 'javascript)
    (define tsserver-library
      (if (any-package? node-typescript)
          (file-append node-typescript "/lib/node_modules/typescript/lib")
          node-typescript))
    (define ts-lsp-executable
      (if (any-package? node-typescript-language-server)
          (file-append node-typescript-language-server
                       "/bin/typescript-language-server")
          node-typescript-language-server))
    (list
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((defun rde--javascript-disable-eglot-parts ()
            (setq-local eglot-stay-out-of ',eglot-stay-out-of))

          (defun rde--javascript-setup-electric-pairs-for-jsx-tsx ()
            (electric-pair-local-mode)
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs
                                '((60 . 62)))) ;; <, >
            (setq-local electric-pair-text-pairs electric-pair-pairs))

          (dolist
           (hook
            '(js-mode-hook
              typescript-mode-hook
              typescript-tsx-mode-hook))
           (add-hook hook
                     (lambda ()
                       (eglot-ensure)
                       (rde--javascript-setup-electric-pairs-for-jsx-tsx)
                       (rde--javascript-disable-eglot-parts)
                       (js2-minor-mode)
                       (npm-mode))))

          ;; js2-mode
          (with-eval-after-load
           'js2-mode
           (setq js-chain-indent t
                 js2-basic-offset 2
                 js2-skip-preprocessor-directives t
                 js2-mode-show-parse-errors nil
                 js2-mode-show-strict-warnings nil
                 js2-strict-missing-semi-warning nil
                 js2-highlight-level 3
                 js2-idle-timer-delay 0.15))

          ;; typescript-mode
          (with-eval-after-load
           'typescript-mode
           (add-hook 'typescript-mode-hook 'npm-mode)
           (setq typescript-indent-level 2))

          ;; typescript-tsx-mode
          (when (fboundp 'web-mode)
            (define-derived-mode typescript-tsx-mode web-mode "TypeScript[TSX]")
            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))
          (with-eval-after-load
           'web-mode
           (setq web-mode-markup-indent-offset 2
                 web-mode-css-indent-offset 2
                 web-mode-code-indent-offset 2)
           (add-hook 'typescript-tsx-mode-hook
                     (lambda ()
                       (rde--javascript-setup-electric-pairs-for-jsx-tsx)
                       (rde--javascript-diable-eglot-parts)
                       (npm-mode))))

          ;; npm-mode
          (with-eval-after-load
           'npm-mode
           (fset 'npm-mode-command-keymap npm-mode-command-keymap)
           (define-key npm-mode-keymap (kbd "C-c n")
             '("npm" . npm-mode-command-keymap)))

          ;; eglot
          (with-eval-after-load
           'eglot

           (add-to-list
            'eglot-server-programs
            '((js-mode
               typescript-mode
               typescript-tsx-mode) . (,ts-lsp-executable
                                       "--tsserver-path" ,tsserver-library
                                       "--stdio")))))
        #:authors
        '("Demis Balbach <db@minikn.xyz>"
          "Andrew Tropin <andrew@trop.in>")
        #:elisp-packages
        (list emacs-js2-mode
              emacs-npm-mode
              emacs-typescript-mode
              emacs-web-mode)))
     (simple-service
      'javascript-add-packages
      home-profile-service-type
      (list
       node
       node-typescript))))
  (feature
   (name 'javascript)
   (values `((javascript . #t)))
   (home-services-getter get-home-services)))
