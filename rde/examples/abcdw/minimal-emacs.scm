(define-module (rde examples abcdw minimal-emacs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features version-control)
  #:use-module (rde features terminals)
  #:use-module (rde features fontutils)

  #:use-module (gnu packages emacs)

  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define minimal-emacs-config
  (rde-config
   (features
    (list
     (feature-user-info
      #:user-name "bob"
      #:full-name "Andrew Tropin"
      #:email "andrew@trop.in"
      #:emacs-advanced-user? #t)
     (feature-emacs-portable)
     (feature-fonts)
     (feature-emacs-keycast #:turn-on? #t)
     (feature-emacs-which-key)
     (feature-emacs-vertico)
     (feature-emacs-completion)
     (feature-emacs-eshell)
     (feature-vterm)
     (feature-git #:sign-commits? #f)
     (feature-emacs-git)
     (feature-emacs-project)
     (feature-emacs-org)
     (feature-emacs-org-roam
      #:org-roam-directory "~/work/abcdw/notes/notes")
     (feature-emacs-org-agenda
      #:org-agenda-files '("~/work/abcdw/private/todo.org"
                           "~/work/abcdw/rde/TODO"))
     (feature-emacs-appearance)
     (feature-emacs-faces)
     ))))

(package
 (name "minimal-emacs-env")
 (version "0.0.1")
 (source (local-file "."))
 (build-system trivial-build-system)
 (native-inputs
  (append
   (rde-config-home-packages minimal-emacs-config)
   ;; (list emacs-next-pgtk)
   ))
 (home-page "")
 (synopsis "")
 (description "")
 (license #f))
