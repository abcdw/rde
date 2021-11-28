(define-module (rde features password-utils)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services state)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)

  #:export (feature-password-store))


(define* (feature-password-store
	  #:key
	  (remote-password-store-url #f))
  "Setup and configure password manager."
  ;; (ensure-pred maybe-url? remote-password-store-url)

  (define emacs-f-name 'pass)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (password-store-home-services config)
    "Returns home services related to password-store."
    (require-value 'gpg-primary-key config)
    (require-value 'home-directory config)
    (list (service home-password-store-service-type)
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

          ;; Configure emacs to use pass.el if feature-emacs is enabled.
          (when (get-value 'emacs config)
            (elisp-configuration-service
             emacs-f-name
             #:elisp-packages (list emacs-pass
                                    emacs-password-store
                                    emacs-password-store-otp)))

          ;; Configure consult to search pass-store if feature-completion
          ;; is enabled.
          (when (get-value 'emacs-completion config)
            (elisp-configuration-service
             (symbol-append emacs-f-name '-completion)
             `((define-key global-map (kbd "M-g p") 'rde-consult-pass)
               (autoload 'password-store-dir "password-store")
               (autoload 'password-store-list "password-store")

               ;; Source:
               ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/tools/pass/autoload/consult.el
               (defun rde-consult-pass (arg pass)
                 "Interactively search the password store."
                 (interactive
                  (list current-prefix-arg
                        (progn
                         (require 'consult)
                         (consult--read (password-store-list)
                                        :prompt "Pass: "
                                        :sort nil
                                        :require-match t
                                        :category 'pass))))
                 (funcall (if arg
                              'password-store-url
                              'password-store-copy)
                          pass)))))))

  (feature
   (name 'password-store)
   (values '((pass . #t)
	     (password-store . #t)))
   (home-services-getter password-store-home-services)))
