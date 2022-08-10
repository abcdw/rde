;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (gnu home services)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (guix gexp)
  #:use-module (rde gexp)
  #:use-module (guix packages)

  #:export (feature-emacs-tempel))

(define* (feature-emacs-tempel
          #:key
          (emacs-tempel emacs-tempel)
          (tempel-capf-hooks '(prog-mode-hook
                               text-mode-hook
                               conf-mode-hook
                               fundamental-mode))
          (default-templates? #t)
          (templates '())
          (tempel-trigger-prefix "<"))
  "Configure TempEL for emacs.  To extend a list of templates from other
features use `home-emacs-tempel-service-type'."
  (ensure-pred file-like? emacs-tempel)
  (ensure-pred string? tempel-trigger-prefix)
  (ensure-pred list? tempel-capf-hooks)

  (define emacs-f-name 'tempel)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (service home-emacs-tempel-service-type
              (home-emacs-tempel-configuration
               (templates
                (if default-templates?
                    `(,#~"fundamental-mode ;; Available everywhere\n"
                      (today (format-time-string "%Y-%m-%d"))
                      (copyright
                       (if (derived-mode-p 'lisp-data-mode 'clojure-mode 'scheme-mode)
                           ";;;"
                           comment-start)
                       (if (string-suffix-p " " comment-start) "" " ")
                       "Copyright © " (format-time-string "%Y") " "
                       (format "%s <%s>" user-full-name user-mail-address)
                       comment-end))
                    '()))))

     (simple-service
      'emacs-tempel-user-templates
      home-emacs-tempel-service-type
      templates)

     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'tempel))
        (with-eval-after-load
         'tempel
         (setq tempel-trigger-prefix ,tempel-trigger-prefix)
         (defun rde-tempel-setup-capf ()
           "Prepends `tempel-complete' to `completion-at-point-functions'."
           (setq-local completion-at-point-functions
                       (cons 'tempel-complete
                             completion-at-point-functions)))

         (mapcar
          (lambda (mode)
            (add-hook mode 'rde-tempel-setup-capf))
          ',tempel-capf-hooks))

        (define-key global-map (kbd "M-+") 'tempel-insert)

        (if after-init-time
             (global-tempel-abbrev-mode 1)
             (add-hook 'after-init-hook 'global-tempel-abbrev-mode)))
      #:elisp-packages (list emacs-tempel)
      #:summary "\
Simple templates based on tempo syntax."
      #:commentary "\
Integrates well with CAPF and abbrev.  Use `expand-abbrev', `tempel-insert' or
just start typing `tempel-trigger-prefix' (default is \"<\") and use
`completion-at-point'.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-tempel)))
   (home-services-getter get-home-services)))

;;; emacs-xyz.scm end here
