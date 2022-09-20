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
          (remote-password-store-url #f))
  "Setup and configure password manager."
  ;; (ensure-pred maybe-url? remote-password-store-url)

  (define emacs-f-name 'pass)
  (define f-name (symbol-append 'emacs- emacs-f-name))

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
             #~(system* #$(get-value 'emacs-client-create-frame config)
;; https://git.sr.ht/~bruun/home/tree/master/item/.config/emacs/bruun/bruun-emacs-menu.el
                        "--eval" "(progn \
(set-frame-name \"pass - Emacs Client\") \
(let ((current-frame (selected-frame))) \
  (unwind-protect \
      (command-execute 'rde-consult-pass) \
    (delete-frame current-frame))))"
                        "-F"
                        "((minibuffer . only) (width . 120) (height . 11))")))

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
             (password-store . ,password-store)))
   (home-services-getter password-store-home-services)))
