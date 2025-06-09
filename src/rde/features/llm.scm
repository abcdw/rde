;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features llm)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26)

  #:export (feature-emacs-ellama feature-emacs-gptel))

;; https://github.com/armindarvish/consult-web
;; https://github.com/karthink/gptel
;; https://github.com/s-kostyaev/ellama
;; https://tabby.tabbyml.com/docs/welcome/
;; https://github.com/TabbyML/tabby ; local copilot

(define* (feature-emacs-ellama
          #:key
          (emacs-ellama emacs-ellama))
  "Maps keys randomly"
  (ensure-pred file-like? emacs-ellama)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      'ellama
      config
      `((setopt ellama-keymap-prefix "C-c e")
        (with-eval-after-load 'ellama
          ;; (setopt llm-warn-on-nonfree nil)
          (require 'llm-openai)
          (setopt llm-warn-on-nonfree nil)
          (setopt ellama-long-lines-length 80)
          (setopt
           ellama-provider
           (make-llm-openai
            :key (auth-source-pick-first-password :host "openai.com")
            :chat-model "gpt-4o-mini"))
          (setopt
           ellama-providers
           (list
            (cons
             "mini"
             (make-llm-openai
              :key (auth-source-pick-first-password :host "openai.com")
              :chat-model "gpt-4o-mini"))
            (cons
             "full"
             (make-llm-openai
              :key (auth-source-pick-first-password :host "openai.com")
              :chat-model "gpt-4o"))))))
      #:summary "\
LLMs interaction interface"
      #:commentary "\
Can be used for variety of tasks: translation, refactoring, rubber duck
discussions, prettifying and spelling correction."
      #:keywords '(convenience llm)
      #:elisp-packages (list emacs-ellama))))

  (feature
   (name 'ellama)
   (values `((ellama . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-gptel
          #:key
          (emacs-gptel emacs-gptel)
          (emacs-gptel-quick emacs-gptel-quick)
          (emacs-gptel-api-key (list "pass" "show" "gptel-api-key"))
          (emacs-gptel-default-mode 'org-mode))
  "Configure Gptel, a simple and unintrusive LLM client for Emacs.
EMACS-GPTEL-API-KEY is a list of program and arguments that are called by
Emacs and that returns a string API key (safer defaults than having it as a
string on-disk).  By default, it tries to load the `emacs-gptel-api-key' from
the password-store."
  (ensure-pred file-like? emacs-gptel)
  (ensure-pred file-like? emacs-gptel-quick)
  (ensure-pred list-of-strings? emacs-gptel-api-key)
  (ensure-pred (cut member <> '(markdown-mode org-mode text-mode))
               emacs-gptel-default-mode)

  (define emacs-f-name 'gptel)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Gptel."
    (define gptel-api-key (get-value 'emacs-gptel-api-key config))
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
          (setopt gptel-api-key 'rde-gptel-get-api-key)
          ,@(if (get-value 'emacs-embark config)
                '((with-eval-after-load 'embark
                    (keymap-set embark-general-map "?" 'gptel-quick)))
                '())
          (setopt gptel-default-mode
                  ',(get-value 'emacs-gptel-default-mode config))))
      #:elisp-packages (list (get-value 'emacs-gptel config)
                             (get-value 'emacs-gptel-quick config)))))

  (feature
   (name f-name)
   (values `((emacs-gptel . ,emacs-gptel)
             (emacs-gptel-quick . ,emacs-gptel-quick)
             (emacs-gptel-api-key . ,emacs-gptel-api-key)
             (emacs-gptel-default-mode . ,emacs-gptel-default-mode)))
   (home-services-getter get-home-services)))
