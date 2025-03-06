;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024-2025 jgart <jgart@dismail.de>

(define-module (rde features agda)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages agda)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (feature-agda))

(define* (feature-agda
          #:key
          (agda agda)
          (emacs-agda2-mode emacs-agda2-mode)
          (deactivate-input-method? #f))
  "Configure Agda for Emacs. If deactivate-input-method? is #t, do not use
Agda's input method for writing various symbols like ∀≥ℕ→π⟦⟧."
  (ensure-pred file-like? agda)
  (ensure-pred file-like? emacs-agda2-mode)
  (ensure-pred boolean? deactivate-input-method?)

  (define f-name 'agda)
  (define emacs-f-name (symbol-append 'emacs- f-name))

  (define (get-home-services config)
    (list
     (simple-service 'add-agda-home-package
                     home-profile-service-type
                     (list agda))
     (when (get-value 'emacs config #f)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `(,@(if deactivate-input-method?
                '((with-eval-after-load 'agda2-mode
                    (add-hook 'agda2-mode-hook
                              '(lambda ()
                                 (deactivate-input-method)))))
                '()))
        #:elisp-packages (list emacs-agda2-mode)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
