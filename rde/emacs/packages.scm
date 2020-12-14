(define-module (rde emacs packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix store)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build emacs-utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-rde
  (package
    (name "emacs-rde")
    (version "0.2.0")
    (build-system emacs-build-system)
    (source (local-file "emacs-rde.el"))
    (propagated-inputs `(("which-key" ,emacs-which-key)
			 ("use-package" ,emacs-use-package)))
    (synopsis "test")
    (description "test")
    (home-page "https://github.com/abcdw/rde")
    (license license:gpl3+)))


;; (symlink "./emacs-rde.el" "/home/abcdw/tmp-file.el")

(define rde-emacs-packages
  '(emacs-use-package))

(define (update-package-emacs p)
  (pretty-print p)
  (pretty-print  (equal?
		  (package-build-system p)
		  emacs-build-system))
  (if (equal?
       (package-build-system p)
       emacs-build-system)
      (package (inherit p)
	       (arguments
		(substitute-keyword-arguments
		    (package-arguments p)
		  ((#:emacs e #:emacs) rde-emacs))))
      p))

(define rde-emacs-instead-of-emacs
  (package-mapping update-package-emacs
		   (lambda (p) #f)))

(packages->manifest
 ;; (append (map rde-emacs-instead-of-emacs rde-emacs-packages)
 ;; 	 '(rde-emacs))
 (list
  emacs
  ;; emacs-next-pgtk
  ;; (rde-emacs-instead-of-emacs
  ;;  )
  emacs-use-package
  emacs-which-key
  emacs-magit
  emacs-rde
  ;; emacs-ivy
  ))

;; (specifications->manifest
;;  '("emacs"))
