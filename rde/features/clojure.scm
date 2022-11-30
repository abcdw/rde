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
          (clojure-tools-cli clojure-tools-cli)
          (clojure-lsp #f)
          (cljr-clojure-test-declaration
           "[clojure.test :refer [deftest are is testing]]")
          (jdk (list openjdk17 "jdk")))
  "Setup and configure environment for Clojure. "
  (ensure-pred file-like? clojure-tools-cli)
  (ensure-pred file-like? clojure-lsp)

  (define (get-home-services config)
    (define emacs-f-name 'clojure)
    (define clojure-lsp-binary
      (if (any-package? clojure-lsp)
          (file-append clojure-lsp "/bin/clojure-lsp")
          clojure-lsp))
    (list
     (unless (get-value 'openjdk config)
         (simple-service
          'clojure-add-packages
          home-profile-service-type
          (list
           ;; for go-to-definition
           ;; MAYBE: Add as a dependency to cider?
           (@ (gnu packages compression) unzip)
           clojure-tools
           jdk)))

     ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-clojure-syntax-highlighting
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((defun rde--clojure-disable-eglot-parts-in-favor-of-cider ()
            (setq-local eglot-stay-out-of '(eldoc flymake)))
          (add-hook 'clojure-mode-hook
                    'rde--clojure-disable-eglot-parts-in-favor-of-cider)

          ;; TODO: Check if this hack is still needed
          (with-eval-after-load
           'consult-imenu
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
          (with-eval-after-load
           'eglot
           (add-to-list
            'eglot-server-programs
            '(((clojure-mode :language-id "clojure")
               (clojurec-mode :language-id "clojure")
               (clojurescript-mode :language-id "clojurescript"))
              . (,clojure-lsp-binary))))

          ;; MAYBE: Move it to configure-rde-emacs?  It's very generic and
          ;; can be useful in many other situations.
          (add-hook 'after-init-hook 'jarchive-setup)

          (with-eval-after-load
           'cider-mode
           ;; Make cider-completion work together with orderless and eglot
           ;; https://github.com/clojure-emacs/cider/issues/3019
           ;; https://github.com/oantolin/orderless/issues/89
           (require 'cider-completion)
           (defun rde--cider-complete-at-point ()
             "Complete the symbol at point."
             (require 'thingatpt)
             (when (and (cider-connected-p)
                        (not (cider-in-string-p)))
               (let*
                ((bounds
                  (or (bounds-of-thing-at-point 'symbol)
                      ;; It may be too expensive to calculate it every time
                      `(cons ,(point) ,(point))))
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
                (message "=> %s %s" (buffer-substring beg end) completion)
                (list beg end (completion-table-dynamic (lambda (_) completion))
                 :annotation-function 'cider-annotate-symbol))))

           (advice-add 'cider-complete-at-point
                       :override 'rde--cider-complete-at-point)

           (with-eval-after-load 'orderless
             (defun rde-orderless-clojure (component)
               "Match namespace and symbol separately by changing `/' to `.*'."
               (orderless--separated-by
                '(zero-or-more nonl)
                (split-string component "/")))

             (defun rde--setup-clojure-orderless-matching-style ()
               "hehe"
               (make-local-variable 'orderless-matching-styles)
               (add-hook 'orderless-matching-styles 'rde-orderless-clojure 0 t))

             (add-hook 'cider-mode-hook
                       'rde--setup-clojure-orderless-matching-style))

           (setq cider-doc-auto-select-buffer nil)
           (setq cider-auto-select-error-buffer nil)
           (setq cider-inspector-auto-select-buffer nil)
           (setq cider-auto-select-test-report-buffer nil)
           (setq cider-print-options '(("right-margin" 70) ("length" 50))))

          (with-eval-after-load
           'clj-refactor
           (setq cljr-clojure-test-declaration ,cljr-clojure-test-declaration))
          (with-eval-after-load
           'clojure-mode
           (setq clojure-align-forms-automatically t)))
        #:summary "\
Clojure(Script) code style, CIDER, LSP, imenu and other tweaks"
        #:commentary "\
Configure eglot, imenu, CIDER, flymake and other packages.
"
        #:keywords '(convenience clojure)
        #:elisp-packages
        (list emacs-cider emacs-clojure-mode
              emacs-jarchive emacs-clj-refactor
              (get-value 'emacs-eglot config emacs-eglot)
              emacs-flymake-kondor emacs-html-to-hiccup)))))

  (feature
   (name 'clojure)
   (values `((clojure . #t)))
   (home-services-getter get-home-services)))
