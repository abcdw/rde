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

          (add-hook 'clojure-mode-hook 'jarchive-setup)

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
