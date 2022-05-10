(define-module (rde features markup)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-markdown))

(define* (feature-markdown
          #:key
          (emacs-markdown-mode emacs-markdown-mode))
  (define emacs-f-name 'markdown)
  (define f-name emacs-f-name)

  (define (get-home-services config)
    (let ((pandoc (get-value 'pandoc config pandoc)))
      (list
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `(;; The package updates auto-mode-list automatically via autoloads
          (with-eval-after-load
           'markdown-mode
           ;; TODO: Package js/css for prettier previews
           (setq markdown-command ,(file-append pandoc "/bin/pandoc"))
           (setq markdown-fontify-code-blocks-natively t)))
        #:elisp-packages (list emacs-markdown-mode)))))

  (feature
   (name f-name)
   (values (make-feature-values emacs-markdown-mode))
   (home-services-getter get-home-services)))
