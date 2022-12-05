;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Demis Balbach <db@minikn.xyz>
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
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services state)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu services)

  #:use-module (guix gexp)

  #:export (feature-password-store))


(define* (feature-password-store
          #:key
          (password-store password-store)
          (remote-password-store-url #f)
          (default-pass-prompt? #t))
  "Setup and configure password manager."
  ;; (ensure-pred maybe-url? remote-password-store-url)
  (ensure-pred file-like? password-store)
  (ensure-pred boolean? default-pass-prompt?)

  (define emacs-f-name 'pass)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (emacs-pass-prompt config)
    (require-value 'emacs config)
    (emacs-minibuffer-program
     (get-value 'emacs-client-create-frame config)
     "pass-prompt" "pass" 'rde-consult-pass
     #:height (get-value 'standalone-minibuffer-height config 10)))

  (define (password-store-home-services config)
    "Returns home services related to password-store."
    (require-value 'gpg-primary-key config)
    (require-value 'home-directory config)
    (list (service home-password-store-service-type
                   (home-password-store-configuration
                    (package password-store)))
          (simple-service
           'add-password-store-git-state
           home-state-service-type
           (list
            (state-git
            ;;; TODO: Rewrite it to xdg-state-home or rework states.
             (string-append
              (get-value 'home-directory config)
              "/.local/var/lib/password-store")
             remote-password-store-url)))

          (when (get-value 'emacs config)
            (emacs-xdg-service
             'pass
             "Emacs (Client) [pass]"
             (emacs-pass-prompt config)))

          (when (get-value 'emacs config)
            (let ((emacs-embark (get-value 'emacs-embark config))
                  (emacs-consult (get-value 'emacs-consult config)))
              (rde-elisp-configuration-service
               emacs-f-name
               config
               `((eval-when-compile (require 'pass))
                 ;; TODO: Fix tree view in pass.el
                 ,@(if (get-value 'emacs-advanced-user? config)
                       '((setq pass-show-keybindings nil))
                       '())
                 (add-hook 'pass-mode-hook (lambda () (setq truncate-lines t)))

                 (require 'configure-rde-keymaps)
                 (define-key rde-app-map (kbd "p") 'pass)

                 (with-eval-after-load
                  'auth-source
                  (require 'auth-source-pass)
                  (add-to-list 'auth-sources 'password-store))

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

                         (define-key global-map (kbd "M-g p") 'rde-consult-pass))
                       '())

                 ,@(if emacs-embark
                       `((eval-when-compile
                          (require 'embark))

                         (with-eval-after-load
                          'embark
                          (require 'password-store)
                          (embark-define-keymap
                           embark-pass-actions
                           "Keymap for actions for pass entries."
                           ("f" password-store-copy-field)
                           ("b" password-store-url)
                           ("e" password-store-edit)
                           ("g" password-store-generate)
                           ("r" password-store-rename)
                           ("d" password-store-remove))

                          (add-to-list 'embark-keymap-alist
                                       '(pass . embark-pass-actions))))
                       '()))
               #:summary "\
Password store emacs interfaces"
               #:commentary "\
Keybinding for `rde-consult-pass' and embark actions for it."
               #:keywords '(convenience)
               #:elisp-packages
               (append
                (list emacs-pass emacs-password-store emacs-password-store-otp
                      (get-value 'emacs-configure-rde-keymaps config))
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
