;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 jgart <jgart@dismail.de>
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

(define-module (rde features machine-learning)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (rde features emacs)

  #:use-module (gnu home services)
  #:use-module (rde home services emacs-xyz)
  #:use-module (rde home services emacs)

  #:use-module (rde packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde serializers elisp)

  #:use-module (guix gexp)
  #:use-module (rde gexp)
  #:use-module (guix packages)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)

  #:export (feature-emacs-gptel))

(define* (feature-emacs-gptel
          #:key
          (emacs-gptel emacs-gptel)
          (emacs-gptel-quick emacs-gptel-quick)
          (gptel-api-key (list "pass" "show" "gptel-api-key"))
          (default-mode 'org-mode))
  "Configure Gptel, a simple and unintrusive LLM client for Emacs.
GPTEL-API-KEY is a list of program and arguments that are called by Emacs and
that returns a string API key (safer defaults than having it as a string
on-disk).  By default, it tries to load the `gptel-api-key' from the
password-store."
  (ensure-pred file-like? emacs-gptel)
  (ensure-pred file-like? emacs-gptel-quick)
  (ensure-pred list-of-strings? gptel-api-key)
  (ensure-pred (cut member <> '(markdown-mode org-mode text-mode))
               default-mode)

  (define emacs-f-name 'gptel)
  (define f-name (symbol-append 'emacs emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Gptel."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'gptel
          (defun rde-gptel-get-api-key ()
            "Get the API key for gptel."
            (string-trim-right
             (with-output-to-string
               (let ((exit (call-process
                            ,(if (string-prefix? "pass" (car gptel-api-key))
                                 (file-append
                                  (get-value 'password-store config)
                                  "/bin/" (car gptel-api-key))
                                 (car gptel-api-key))
                            nil " *string-output*" nil
                            ,@(cdr gptel-api-key))))
                 (or (zerop exit)
                     (error "Failed to get gptel-api-key with %s"
                            (with-current-buffer " *string-output*"
                                                 (buffer-string))))))))
          (setq gptel-api-key 'rde-gptel-get-api-key)
          ,@(if (get-value 'emacs-embark config)
                '((with-eval-after-load 'embark
                    (keymap-set embark-general-map "?" 'gptel-quick)))
                '())
          (setq gptel-default-mode ',default-mode)))
      #:elisp-packages (list emacs-gptel
                             emacs-gptel-quick))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-gptel . ,emacs-gptel)))
   (home-services-getter get-home-services)))
