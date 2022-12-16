(define-module (abcdw emacs)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features irc)
  #:use-module (rde features mail))

(define-public %emacs-features
  (list
   (feature-emacs
    #:default-application-launcher? #t)

   (feature-emacs-appearance
    #:extra-elisp
    `((setq modus-themes-syntax '(faint))
      ;; (setq modus-themes-region '(bg-only))
      ;; (setq modus-themes-paren-match '(underline))
      (setq modus-themes-org-blocks 'tinted-background)))
   (feature-emacs-faces)

   (feature-emacs-completion
    #:mini-frame? #f
    #:marginalia-align 'right)
   (feature-emacs-corfu
    #:corfu-doc-auto #f)
   (feature-emacs-vertico)

   (feature-emacs-tramp)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)

   (feature-emacs-message)
   (feature-emacs-erc
    #:erc-log? #t)
   (feature-emacs-telega)
   (feature-emacs-elpher)

   (feature-emacs-pdf-tools)
   (feature-emacs-nov-el)
   (feature-emacs-org-protocol)
   (feature-emacs-citar)

   (feature-emacs-smartparens
    #:show-smartparens? #t)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-eglot)))
