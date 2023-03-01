(define-module (rde features markup)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde serializers elisp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages tex)
  #:use-module (guix gexp)

  #:export (feature-markdown
            feature-tex))

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
        #:summary "\
Markdown tweaks"
        #:commentary "\
Integration with pandoc, better code blocks rendering."
        #:keywords '(convenience)
        #:elisp-packages (list emacs-markdown-mode)))))

  (feature
   (name f-name)
   (values (make-feature-values emacs-markdown-mode))
   (home-services-getter get-home-services)))


;;;
;;; feature-tex
;;;

(define* (feature-tex
          #:key
          (extra-tex-packages '())
          (biblatex? #t)
          (listings? #t)
          (listings-options '()))
  "Configure the TeX typesetting system.
LISTINGS-OPTIONS is an association list of key-value pairs for the
tex-listings package that typesets source code."
  (ensure-pred list-of-file-likes? extra-tex-packages)
  (ensure-pred boolean? biblatex?)
  (ensure-pred boolean? listings?)
  (ensure-pred elisp-config? listings-options)

  (define f-name 'tex)

  (define (get-home-services config)
    "Return home services related to TeX."
    (append
     (list
      (simple-service
       'add-tex-home-packages
       home-profile-service-type
       (append
        (list texlive-base)
        (if listings?
            (list texlive-listings)
            '())
        (if biblatex?
            (list texlive-biblatex biber)
            '())
        extra-tex-packages))
      (simple-service
       'add-tex-home-envs
       home-environment-variables-service-type
       '(("TEX_MFHOME" . "$XDG_DATA_HOME/texmf")
         ("TEX_MFVAR" . "$XDG_CACHE_HOME/texlive/texmf-var")
         ("TEX_MFCONFIG" . "$XDG_CONFIG_HOME/texlive/texmf-config"))))
     (if (get-value 'emacs config)
         (list
          (rde-elisp-configuration-service
           f-name
           config
           `((with-eval-after-load 'ox-latex
               ,@(if listings?
                     `((setq org-latex-listings t)
                       (setq org-latex-listings-options ',listings-options))
                     '()))
             (setq bibtex-user-optional-fields
                   '(("keywords" "Keywords to describe the entry" "")
                     ("file" "Link to document file" ":")))
             (setq bibtex-align-at-equal-sign t)
             ,@(if biblatex?
                   '((setq bibtex-dialect 'biblatex))
                   '()))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (tex-biblatex . ,biblatex?)))
   (home-services-getter get-home-services)))
