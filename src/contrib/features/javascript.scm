;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2022, 2024 Andrew Tropin <andrew@trop.in>
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

  #:use-module (rde packages emacs-xyz)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tree-sitter)

  #:use-module (contrib packages node-xyz)

  #:use-module (guix gexp)

  #:export (feature-javascript))

(define* (feature-javascript
          #:key
          (node node)
          (node-typescript node-typescript)
          (node-eslint node-eslint-8.17.0)
          (node-typescript-language-server node-typescript-language-server)
          (node-vscode-js-debug node-vscode-js-debug)
          (eglot-stay-out-of '(flymake))
          (format-buffer-on-save? #f))
  "Setup and configure environment for JavaScript."
  (ensure-pred file-like? node-typescript)
  (ensure-pred file-like? node-typescript-language-server)
  (ensure-pred file-like? node-eslint)
  (ensure-pred file-like? node-vscode-js-debug)
  (ensure-pred boolean? format-buffer-on-save?)

  (define emacs-f-name 'javascript)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)

    (define tsserver-library
      (file-append node-typescript "/lib/node_modules/typescript/lib"))
    (define ts-lsp-executable
      (file-append node-typescript-language-server
                   "/bin/typescript-language-server"))
    (define node-executable (file-append node "/bin/node"))
    (define eslint-executable (file-append node-eslint "/bin/eslint"))
    (define vscode-js-debug-executable
      (file-append node-vscode-js-debug "/bin/dapDebugServer"))

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

          (defun rde--javascript-setup-keymaps (mode)
            (defvar nodejs-repl-mode-command-map
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "e") 'nodejs-repl-send-last-expression)
                (define-key map (kbd "j") 'nodejs-repl-send-line)
                (define-key map (kbd "r") 'nodejs-repl-send-region)
                (define-key map (kbd "C-c") 'nodejs-repl-send-buffer)
                (define-key map (kbd "C-l") 'nodejs-repl-load-file)
                (define-key map (kbd "C-z") 'nodejs-repl-switch-to-repl)
                map))
            (fset 'nodejs-repl-mode-command-map nodejs-repl-mode-command-map)
            (define-key
              (symbol-value (intern (format "%s-map" (car mode))))
              (kbd "C-c r")
              '("repl" . nodejs-repl-mode-command-map))
            (define-key
              (symbol-value (intern (format "%s-map" (car mode))))
              (kbd "C-c f")
              '("Format buffer" . eslint-fix)))

          (with-eval-after-load
              'web-mode
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2))

          (require 'treesit)

          ;;; We want to make sure that eglot sends the correct language-ids
          ;;; to the LSP server for the respective files. In short:
          ;;; .js -> javascript
          ;;; .jsx -> javascriptreact
          ;;; .ts -> typescript
          ;;; .tsx -> typescriptreact
          ;;; But due to  the way javascript / typescript major modes are
          ;;; designed in emacs, this doesn't work with current versions
          ;;; of emacs 29/30.
          ;;; See: https://github.com/joaotavora/eglot/issues/1384
          ;;; However, commit c79a509384d33 in emacs master fixes this. I
          ;;; locally tested this by building latest emacs. Unfortunately,
          ;;; this commit is not yet packaged in Guix. However let's leave
          ;;; the code in place, once the package is updated this should work
          ;;; ootb. In the meantime, this code doesn't have any negative
          ;;; effect.
          (let* ((ts-js? (treesit-ready-p 'javascript))
                 (ts-ts? (treesit-ready-p 'typescript))
                 (mode-list (if (and ts-js? ts-ts?)
                                ;; (mode . language-id)
                                '((jsx-ts-mode . "javascriptreact")
                                  (js-ts-mode . "javascript")
                                  (tsx-ts-mode . "typescriptreact")
                                  (typescript-ts-mode . "typescript"))
                                '((js-jsx-mode . "javascriptreact")
                                  (js-mode . "javascript")
                                  (typescript-tsx-mode . "typescriptreact")
                                  (typescript-mode . "typescript")))))

            ;; Javascript setup
            (cond (ts-js?
                   ;; for tree-sitter
                   (add-to-list 'major-mode-remap-alist `(javascript-mode . js-ts-mode))

                   ;; We need to create a derived mode from tsx-ts-mode
                   (define-derived-mode jsx-ts-mode tsx-ts-mode "JavaScript[JSX]")

                   ;; Manually adding js-ts-mode to the end of the list, so that it doesn't
                   ;; get added again (to the top) once we invoke it
                   (add-to-list 'auto-mode-alist '("\\(\\.js[mx]\\|\\.har\\)\\'" . js-ts-mode) t)
                   (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-ts-mode)))
                  (t
                   ;; for non-tree-sitter
                   (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))))

            ;; Typescript setup
            (cond (ts-ts?
                   ;; tree-sitter
                   (add-to-list 'major-mode-remap-alist `(typescript-mode . typescript-ts-mode))
                   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))
                  (t
                   ;; for non-tree-sitter
                   (define-derived-mode typescript-tsx-mode web-mode "TypeScript[TSX]")
                   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
                   (with-eval-after-load 'typescript-mode
                     (setq typescript-indent-level 2))))

            ;; Setting up each mode dynamically
            (dolist (mode mode-list)
                    (add-hook (intern (format "%s-hook" (car mode)))
                              (lambda ()
                                (rde--javascript-disable-eglot-parts)

                                ;; set up flymake
                                (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
                                (flymake-eslint-enable)

                                ;; Run eslint on save if specified
                                ,@(if format-buffer-on-save?
                                      '((add-hook 'after-save-hook 'eslint-fix nil t))
                                      '())

                                ;; eglot
                                (eglot-ensure)

                                ;; general stuff
                                (setq indent-tabs-mode nil)
                                (rde--javascript-setup-electric-pairs-for-jsx-tsx)
                                (rde--javascript-setup-keymaps mode)
                                (js2-minor-mode)
                                (js2-imenu-extras-mode t)
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

            ;;eglot
            (let ((ts-exec ,ts-lsp-executable)
                  (ts-lib ,tsserver-library)
                  (modes (mapcar
                          (lambda (x) `(,(car x) :language-id ,(cdr x)))
                          mode-list)))
              (with-eval-after-load
                  'eglot
                (add-to-list
                 'eglot-server-programs
                 `(,modes . (,ts-exec "--stdio"
                                      :initializationOptions
                                      (:tsserver (:path ,ts-lib))))))
              (add-hook
               'eglot-managed-mode-hook
               (lambda ()
                 (flymake-mode t)
                 ;; Add flymake diagnostics to mode bar
                 (add-to-list 'mode-line-misc-info
                              `(flymake-mode (" " flymake-mode-line-counters " "))))))

            ;; npm-mode
            (with-eval-after-load
                'npm-mode
              (fset 'npm-mode-command-keymap npm-mode-command-keymap)
              (define-key npm-mode-keymap (kbd "C-c n")
                '("npm" . npm-mode-command-keymap)))

            ;; flymake-eslint
            (with-eval-after-load
                'flymake-eslint
              (setq flymake-eslint-executable-name ,eslint-executable))

            (with-eval-after-load
                'eslint-fix
              (setq eslint-fix-executable ,eslint-executable))

            ;; repl
            (with-eval-after-load
                'nodejs-repl
              (setopt nodejs-repl-command ,node-executable))

            ;; dape
            ,@(if (get-value 'emacs-dape config #f)
                  `((with-eval-after-load
                        'dape
                      (let ((vscode-js-debug ,vscode-js-debug-executable)
                            (js-target-mode (car (rassoc "javascript" mode-list)))
                            (ts-target-mode (car (rassoc "typescript" mode-list))))
                        (setq dape-configs
                              (append
                               ;; NOTE [Demis Balbach, 24-05-15] All configurations
                               ;; apart from `frontend' are untested. I just copied
                               ;; the default dape config and adapted them to work
                               ;; with our version of vscode-js-debug
                               `((backend
                                  modes (,js-target-mode ,ts-target-mode)
                                  command ,vscode-js-debug
                                  port 8123
                                  :name "Backend"
                                  :type "pwa-node"
                                  :cwd dape-cwd
                                  :program dape-buffer-default
                                  :console "internalConsole")
                                 (backend-ts
                                  modes (,ts-target-mode)
                                  command ,vscode-js-debug
                                  port 8123
                                  :name "Backend (TypeScript)"
                                  :type "pwa-node"
                                  :runtimeExecutable "ts-node"
                                  :cwd dape-cwd
                                  :program dape-buffer-default
                                  :console "internalConsole")
                                 (backend-attach
                                  modes (,js-target-mode ,ts-target-mode)
                                  command ,vscode-js-debug
                                  port 8123
                                  :name "Backend (Attach)"
                                  :request "attach"
                                  :port 9229)
                                 (frontend
                                  modes (,js-target-mode ,ts-target-mode)
                                  command ,vscode-js-debug
                                  port 8123
                                  :name "Frontend"
                                  :type "pwa-chrome"
                                  :userDataDir nil
                                  :url ,(lambda () (read-string
                                                    "Url: "
                                                    "http://localhost:3000"))
                                  :webRoot ,(lambda () (read-string
                                                        "Root: "
                                                        (funcall dape-cwd-fn)))))
                               dape-configs)))))
                  '())))
        #:authors
        '("Demis Balbach <db@minikn.xyz>"
          "Andrew Tropin <andrew@trop.in>")
        #:elisp-packages
        (list emacs-js2-mode
              emacs-npm-mode
              emacs-typescript-mode
              emacs-web-mode
              emacs-nodejs-repl
              emacs-markdown-mode
              emacs-flymake-eslint
              emacs-eslint-fix)))
     (simple-service
      'javascript-add-packages
      home-profile-service-type
      (list
       node
       tree-sitter-typescript
       tree-sitter-javascript))))
  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
