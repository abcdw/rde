(define-module (rde features markup)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)
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


;;;
;;; feature-markdown
;;;

(define* (feature-markdown
          #:key
          (emacs-markdown-mode emacs-markdown-mode)
          (headings-scaling? #f))
  "Configure rendering of Markdown files."
  (ensure-pred file-like? emacs-markdown-mode)
  (ensure-pred boolean? headings-scaling?)

  (define emacs-f-name 'markdown)
  (define f-name emacs-f-name)

  (define (get-home-services config)
    (let ((pandoc (get-value 'pandoc config pandoc)))
      (list
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `(;; The package updates auto-mode-list automatically via autoloads
          (with-eval-after-load 'markdown-mode
            ,@(if headings-scaling?
                  '((setq markdown-header-scaling t)
                    (setq markdown-header-scaling-values
                          '(1.2 1.1 1.1 1.0 1.0 0.9)))
                  '())
            (setq markdown-hide-urls t)
            (setq markdown-hide-markup t)
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
          (texlive-scheme-basic texlive-scheme-basic)
          (texlive-listings texlive-listings)
          (texlive-biber texlive-biber)
          (texlive-biblatex texlive-biblatex)
          (extra-tex-packages '())
          (biblatex? #t)
          (listings? #t)
          (listings-options '()))
  "Configure the TeX typesetting system.
LISTINGS-OPTIONS is an association list of key-value pairs for the
tex-listings package that typesets source code."
  (ensure-pred file-like? texlive-scheme-basic)
  (ensure-pred file-like? texlive-listings)
  (ensure-pred file-like? texlive-biber)
  (ensure-pred file-like? texlive-biblatex)
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
        (list texlive-scheme-basic)
        (if listings?
            (list texlive-listings)
            '())
        (if biblatex?
            (list texlive-biblatex texlive-biber)
            '())
        extra-tex-packages))
      (simple-service
       'add-tex-home-envs
       home-environment-variables-service-type
       '(("TEX_MFHOME" . "$XDG_DATA_HOME/texmf")
         ("TEX_MFVAR" . "$XDG_CACHE_HOME/texlive/texmf-var")
         ("TEX_MFCONFIG" . "$XDG_CONFIG_HOME/texlive/texmf-config"))))
     (if (get-value 'emacs config #f)
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
