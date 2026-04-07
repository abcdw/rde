;;; rde --- Reproducible development environment.
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (rde features jujutsu)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde serializers ini)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix gexp)
  #:export (feature-jujutsu))

(define* (feature-jujutsu
          #:key
          (jujutsu jujutsu)
          (emacs-majutsu emacs-majutsu)
          (extra-config '()))
  "Setup and configure Jujutsu version control system with Emacs integration.

EXTRA-CONFIG is an alist of additional INI/TOML sections appended to the
generated @file{~/.config/jj/config.toml}."
  (ensure-pred file-like? jujutsu)
  (ensure-pred file-like? emacs-majutsu)
  (ensure-pred list? extra-config)

  (define f-name 'jujutsu)

  (define (get-home-services config)
    (define full-name (get-value 'full-name config))
    (define email    (get-value 'email config))
    (define emacs-client (get-value 'emacs-client config #f))

    (define jj-config
      `((user
         ((name  . ,full-name)
          (email . ,email)))
        ,@extra-config))

    (list
     (simple-service
      'add-jujutsu-home-package
      home-profile-service-type
      (list jujutsu))
     (simple-service
      'jujutsu-config
      home-xdg-configuration-files-service-type
      `(("jj/config.toml"
         ,(apply mixed-text-file
                 "jj-config.toml"
                 (ini-serialize
                  jj-config
                  #:equal-string " = ")))))
     (rde-elisp-configuration-service
      f-name
      config
      `((autoload 'majutsu "majutsu" nil t)
        (with-eval-after-load 'majutsu-jj
          (setopt majutsu-jj-executable
                  ,(file-append jujutsu "/bin/jj"))))
      #:summary "Jujutsu version control integration"
      #:commentary "Set jj executable path and autoload majutsu entry point."
      #:keywords '(convenience tools)
      #:elisp-packages (list emacs-majutsu))))

  (feature
   (name f-name)
   (values `((,f-name . ,jujutsu)
             (emacs-majutsu . ,emacs-majutsu)))
   (home-services-getter get-home-services)))
