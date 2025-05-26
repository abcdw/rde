;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-ellama))

;; https://github.com/armindarvish/consult-web
;; https://github.com/karthink/gptel
;; https://github.com/s-kostyaev/ellama
;; https://tabby.tabbyml.com/docs/welcome/
;; https://github.com/TabbyML/tabby ; local copilot

(define* (feature-ellama
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
