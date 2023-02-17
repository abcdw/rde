;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 conses <contact@conses.eu>
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

(define-module (rde features clojure)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages java)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-clojure))

;; https://github.com/jpe90/emacs-clj-deps-new
;; https://practical.li/spacemacs/ :: some emacs clojure tips

(define* (feature-clojure
          #:key
          (clojure-tools clojure-tools)
          (clojure-lsp #f)
          (eglot-stay-out-of '())
          (jdk (list openjdk17 "jdk")))
  (ensure-pred file-like? clojure-tools)
  (ensure-pred maybe-file-like? clojure-lsp)
  ;; (ensure-pred file-like? jdk)
  (ensure-pred list? eglot-stay-out-of)

  (define (get-home-services config)
    (define emacs-f-name 'clojure)
    (define clojure-lsp-binary
      (if (any-package? clojure-lsp)
          (file-append clojure-lsp "/bin/clojure-lsp")
          clojure-lsp))

    (append
      (list
       (simple-service
        'add-clojure-packages
        home-profile-service-type
         (list
          ;; for go-to-definition
          ;; MAYBE: Add as a dependency to cider?
          (@ (gnu packages compression) unzip)
          clojure-tools
          jdk))))
          (list
      ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-clojure-syntax-highlighting
      (if (get-value 'emacs config)
          (list
           (rde-elisp-configuration-service
            emacs-f-name
            config
            `((defun rde--clojure-disable-eglot-parts-in-favor-of-cider ()
                (setq-local eglot-stay-out-of ',eglot-stay-out-of))
              (add-hook 'clojure-mode-hook
                        'rde--clojure-disable-eglot-parts-in-favor-of-cider)

              ;; TODO: Check if this hack is still needed
              (with-eval-after-load 'consult-imenu
                (add-to-list
                 'consult-imenu-config
                 ;; https://github.com/clojure-lsp/clojure-lsp/blob/13487d1eb0d06596565f76b8f6d76d00b7e9f03b/cli/integration-test/integration/initialize_test.clj#L62
                 '(clojure-mode
                   :toplevel "Variable"
                   :types ((?f "Function"  font-lock-function-name-face)
                           (?m "Macro"     font-lock-function-name-face)
                           (?M "Method"    font-lock-function-name-face)
                           (?e "Event"     font-lock-function-name-face)
                           (?n "Namespace" font-lock-constant-face)
                           (?k "Keyword"   font-lock-keyword-face)
                           (?c "Class"     font-lock-type-face)
                           (?t "Type"      font-lock-type-face)
                           (?v "Variable"  font-lock-variable-name-face)))))

              (with-eval-after-load 'orderless
                (defun rde-orderless-clojure (component)
                  "Match ns and symbol separately by changing `/' to `.*'."
                  (orderless--separated-by
                      '(zero-or-more nonl)
                    (split-string component "/")))

                (defun rde--setup-clojure-orderless-matching-style ()
                  (make-local-variable 'orderless-matching-styles)
                  (add-hook 'orderless-matching-styles
                            'rde-orderless-clojure 0 t))

                (add-hook 'cider-mode-hook
                          'rde--setup-clojure-orderless-matching-style))

              ,@(if clojure-lsp-binary
                    `((with-eval-after-load 'eglot
                        (add-to-list
                         'eglot-server-programs
                         '(((clojure-mode :language-id "clojure")
                            (clojurec-mode :language-id "clojure")
                            (clojurescript-mode :language-id "clojurescript"))
                           . (,clojure-lsp-binary)))))
                    '())

              ;; MAYBE: Move it to configure-rde-emacs?  It's very generic and
              ;; can be useful in many other situations.
              (add-hook 'after-init-hook 'jarchive-setup)

              (with-eval-after-load 'cider
                (setq cider-allow-jack-in-without-project t)
                (setq cider-words-of-inspiration '("")))

              (with-eval-after-load 'cider-mode
                ;; Make cider-completion work together with orderless and eglot
                ;; https://github.com/clojure-emacs/cider/issues/3019
                ;; https://github.com/oantolin/orderless/issues/89
                (require 'cider-completion)
                (defun rde--cider-complete-at-point ()
                  "Complete the symbol at point."
                  (require 'thingatpt)
                  (when (and (cider-connected-p)
                             (not (cider-in-string-p)))
                    (let* ((bounds (or (bounds-of-thing-at-point 'symbol)
                                       ;; It may be too expensive to calculate it every time
                                       (cons (point) (point))))
                           (beg (car bounds))
                           (end (cdr bounds))
                           (completion
                            (append
                             (cider-complete
                              ;; Use only namespace as a prefix for nrepl completions,
                              ;; the rest will be filtered with orderless
                              (replace-regexp-in-string
                               "/.*" "/" (buffer-substring beg end)))
                             (get-text-property (point) 'cider-locals))))
                      (list beg end (completion-table-dynamic
                                     (lambda (_) completion))
                            :annotation-function 'cider-annotate-symbol))))

                (advice-add 'cider-complete-at-point
                            :override 'rde--cider-complete-at-point)

                (setq cider-use-xref nil) ;; eglot will handle it
                (setq cider-auto-select-error-buffer nil)
                (setq cider-inspector-auto-select-buffer nil)
                (setq cider-auto-select-test-report-buffer nil)
                (setq cider-print-options '(("right-margin" 70) ("length" 50)))
                (setq cider-doc-auto-select-buffer nil))

              (with-eval-after-load 'cider-repl
                (define-key cider-repl-mode-map (kbd "C-M-q") 'indent-sexp)
                (setq cider-repl-pop-to-buffer-on-connect nil)
                ,@(if (get-value 'emacs-advanced-user? config)
                      '((setq cider-repl-display-help-banner nil))
                      '()))
              ,@(if (get-value 'emacs-org config)
                    '((with-eval-after-load 'org
                        (add-to-list 'org-structure-template-alist
                                     '("clj" . "src clojure"))
                        (require 'ob-clojure)
                        (require 'ob-java))
                      (with-eval-after-load 'ob-core
                        (setq org-babel-default-header-args:clojure
                              '((:results . "scalar")
                                (:session . ""))))
                      (with-eval-after-load 'ob-clojure
                        (setq org-babel-clojure-backend 'cider)))
                    '())

              (with-eval-after-load 'clojure-mode
                (setq clojure-align-forms-automatically t)))
            #:summary "\
Clojure(Script) code style, CIDER, LSP, imenu and other tweaks"
            #:commentary "\
Configure eglot, imenu, CIDER, flymake and other packages.
"

        #:keywords '(convenience clojure)
        #:elisp-packages
        (list emacs-cider emacs-clojure-mode
              emacs-jarchive emacs-html-to-hiccup)))))
          '())

  (feature
   (name 'clojure)
   (values `((clojure . #t)))
   (home-services-getter get-home-services)))
