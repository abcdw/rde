(define-module (rde emacs packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build emacs-utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (%rde-emacs-packages
	    emacs-rde-core))

(define-public emacs-rde-core
  (package
    (name "emacs-rde-core")
    (version "0.2.0")
    (build-system emacs-build-system)
    (source (local-file "rde-core.el"))
    (propagated-inputs `(("use-package" ,emacs-use-package)))
    (synopsis "use-package initialization")
    (description "use-package initialization")
    (home-page "https://github.com/abcdw/rde")
    (license license:gpl3+)))

(define-public emacs-rde-early-init
  (package
    (name "emacs-rde-early-init")
    (version "0.2.0")
    (build-system emacs-build-system)
    (source (local-file "early-init.el"))
    (synopsis "Different tweaks for faster startup")
    (description "In addition to tweaks, disables GUI elements")
    (home-page "https://github.com/abcdw/rde")
    (license license:gpl3+)))

;; (symlink "./emacs-rde.el" "/home/abcdw/tmp-file.el")

;; (define rde-emacs-packages
;;   '(emacs-use-package))

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

;; (packages->manifest
;;  ;; (append (map rde-emacs-instead-of-emacs rde-emacs-packages)
;;  ;; 	 '(rde-emacs))
;; )

(define-public %rde-emacs-packages
  (list emacs-next-pgtk emacs-rde-early-init emacs-rde-core))
;; (specifications->manifest
;;  '("emacs"))
