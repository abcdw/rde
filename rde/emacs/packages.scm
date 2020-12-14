(define-module (rde emacs packages)
  #:use-modules (guix build-system emacs)
  #:use-modules (guix store)
  #:use-modules (guix git-download)
  #:use-modules (guix packages)
  #:use-modules (gnu packages autotools)
  #:use-modules (gnu packages fontutils)
  #:use-modules (gnu packages webkit)
  #:use-modules (gnu packages gnome)
  #:use-modules (gnu packages emacs-xyz)
  #:use-modules (guix build emacs-utils)
  #:use-modules (ice-9 pretty-print)
  #:use-modules (guix utils)
  #:use-modules ((guix licenses) #:prefix license:))

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
