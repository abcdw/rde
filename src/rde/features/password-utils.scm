;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
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

(define-module (rde features password-utils)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services state)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu services)

  #:use-module (guix gexp)

  #:export (feature-password-store))

(define* (feature-password-store
          #:key
          (password-store password-store)
          (pass-key "p")
          (consult-pass-key "M-g P")
          (remote-password-store-url #f)
          (password-store-directory "$HOME/.local/var/lib/password-store")
          (default-pass-prompt? #t))
  "Setup and configure password manager."
  ;; (ensure-pred maybe-url? remote-password-store-url)
  (ensure-pred file-like? password-store)
  (ensure-pred boolean? default-pass-prompt?)
  (ensure-pred path? password-store-directory)
  (ensure-pred string? pass-key)
  (ensure-pred string? consult-pass-key)

  (define emacs-f-name 'pass)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (emacs-pass-prompt config)
    (require-value 'emacs config)
    ((get-value-eval 'emacs-minibuffer-program config)
     "pass-prompt" "pass" 'rde-consult-pass
     #:height (get-value 'standalone-minibuffer-height config 10)))

  (define (password-store-home-services config)
    "Returns home services related to password-store."
    (require-value 'gpg-primary-key config)
    (require-value 'home-directory config)
    (list (service home-password-store-service-type
                   (home-password-store-configuration
                    (package password-store)
                    (directory password-store-directory)))
          (simple-service
           'password-store-add-git-state
           home-state-service-type
           (list
            (state-git
             ;; TODO: Update the implementation of rde states
             ;; FIXME: It's a workaround, don't reuse it anywhere
             ((@ (ice-9 string-fun) string-replace-substring)
              password-store-directory
              "$HOME" (get-value 'home-directory config))
             remote-password-store-url)))
          (simple-service
           'add-password-store-extensions
           home-profile-service-type
           (list pass-otp))

          (when (get-value 'emacs config #f)
            (emacs-xdg-service
             'pass
             "Emacs (Client) [pass]"
             (emacs-pass-prompt config)))

          (when (get-value 'emacs config #f)
            (let ((emacs-embark (get-value 'emacs-embark config))
                  (emacs-consult (get-value 'emacs-consult config #f))
                  (wtype (get-value 'wtype config wtype)))
              (rde-elisp-configuration-service
               emacs-f-name
               config
               `((eval-when-compile (require 'pass))
                 ;; TODO: Fix tree view in pass.el
                 ,@(if (get-value 'emacs-advanced-user? config)
                       '((setq pass-show-keybindings nil))
                       '())
                 (add-hook 'pass-mode-hook (lambda () (setq truncate-lines t)))

                 (with-eval-after-load 'rde-keymaps
                   (define-key rde-app-map (kbd ,pass-key) 'pass))

                 (with-eval-after-load 'auth-source
                   (auth-source-pass-enable))

                 ;; Source:
                 ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/tools/pass/autoload/consult.el
                 ,@(if emacs-consult
                       `((autoload 'password-store-dir "password-store")
                         (autoload 'password-store-list "password-store")
                         (autoload 'consult--read "consult")

                         (defun rde-consult-pass (arg pass)
                           "Interactively search the password store."
                           (interactive
                            (list current-prefix-arg
                                  (consult--read (password-store-list)
                                                 :prompt "Pass entry: "
                                                 :sort nil
                                                 :require-match nil
                                                 :category 'pass)))
                           (funcall (if arg
                                        'password-store-url
                                        'password-store-copy)
                                    pass))

                         (define-key global-map (kbd ,consult-pass-key)
                           'rde-consult-pass))
                       '())

                 (with-eval-after-load 'password-store
                   (defun rde-password-store-autotype (entry)
                     "\
Find `username' and `secret' of particular pass entry and types it with a Tab
in the middle and Return at the end."
                     (let* ((wtype-bin ,(file-append wtype "/bin/wtype"))
                            (entry-alist (password-store-parse-entry entry))
                            (wtype-args
                             (list
                              wtype-bin
                              "-s" "10"
                              (alist-get "username" entry-alist nil nil 'equal)
                              "-k" "Tab"
                              (alist-get 'secret entry-alist nil nil 'equal)
                              "-k" "Return")))

                       (async-shell-command
                        (mapconcat 'shell-quote-argument wtype-args " ")))))
                 ,@(if emacs-embark
                       `((with-eval-after-load 'password-store
                           (defun rde-password-store-generate (entry)
                             "Create and entry and generate a secret.  Warn
 and do nothing, if the entry with this name already exists."
                             (interactive "sNew entry: ")
                             (if (password-store-get entry)
                                 (message "Warning: Entry '%s' already exists. Not generating." entry)
                               (password-store-generate entry)))

                           (defvar pass-embark-actions
                             (let ((map (make-sparse-keymap)))
                               (define-key map "f" 'password-store-copy-field)
                               (define-key map "b" 'password-store-url)
                               (define-key map "e" 'password-store-edit)
                               (define-key map "g" 'rde-password-store-generate)
                               (define-key map "r" 'password-store-rename)
                               (define-key map "d" 'password-store-remove)
                               (define-key map "i" 'password-store-insert)
                               (define-key map "a" 'rde-password-store-autotype)
                               map)
                             "Keymap for actions for pass entries."))

                         (with-eval-after-load
                             'embark
                           (require 'password-store)
                           (add-to-list 'embark-keymap-alist
                                        '(pass . pass-embark-actions))))
                       '()))
               #:summary "\
Password store emacs interfaces"
               #:commentary "\
Keybinding for `rde-consult-pass' and embark actions for it."
               #:keywords '(convenience)
               #:elisp-packages
               (append
                (list emacs-pass emacs-password-store emacs-password-store-otp)
                (if emacs-embark (list emacs-embark) '())
                (if emacs-consult (list emacs-consult) '())))))))

  (feature
   (name 'password-store)
   (values `((pass . #t)
             (password-store . ,password-store)
             ,@(if default-pass-prompt?
                   `((default-pass-prompt-fn . ,emacs-pass-prompt))
                   '())))
   (home-services-getter password-store-home-services)))
