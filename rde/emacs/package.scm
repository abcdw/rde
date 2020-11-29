(use-package-modules emacs)
(use-modules (guix build-system emacs))
(use-modules (guix store))
(use-modules (guix git-download))
(use-modules (guix packages))
(use-modules (gnu packages autotools))
(use-modules (gnu packages fontutils))
(use-modules (gnu packages webkit))
(use-modules (gnu packages gnome))
(use-modules (gnu packages emacs-xyz))
(use-modules (guix build emacs-utils))
(use-modules (ice-9 pretty-print))
(use-modules (guix utils))
(use-modules ((guix licenses) #:prefix license:))

(define-public emacs-rde
  (package
    (name "emacs-rde")
    (version "0.1.0")
    (build-system emacs-build-system)
    (source (local-file "emacs-rde.el"))
    (synopsis "test")
    (description "test")
    (home-page "https://github.com/abcdw/rde")
    (license license:gpl3+)))


(define-public emacs-rde-use-package
  (package/inherit emacs-use-package
    (name "emacs-next-use-package")
    (version "2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jwiegley/use-package")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "088kl3bml0rs5bkfymgzr15ram9qvy66h1kaisrbkynh0yxvf8g9"))))
    (propagated-inputs '())))

(define-public emacs-next-pgtk
  (let ((commit "3df4ca451d41a5f1036713277ef55ca9734c6fa7")
        (revision "0"))
    (package/inherit emacs-next
      (name "emacs-next-pgtk")
      (version (git-version "28.0.50" revision commit))
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url "https://github.com/masm11/emacs")
	       (commit commit)))
	 (sha256
          (base32
           "1c677c97b9avhlzysg09cvf6bd69iz41ggppnszw8chhphk3knfc"))))
      (arguments
       (substitute-keyword-arguments
	   (package-arguments emacs-next)
	 ((#:configure-flags flags ''())
	  `(cons* "--with-pgtk" "--with-xwidgets" ,flags))))
      (propagated-inputs `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
			   ("glib-networking" ,glib-networking)))
      (inputs
       `(("webkitgtk" ,webkitgtk)
	 ,@(package-inputs emacs-next)))

      (home-page "https://github.com/masm11/emacs")
      (synopsis "The fork of extensible, customizable,
self-documenting text editor (with pgtk toolkit and xwidgets
support)")
      (description "This is an unofficial Emacs fork build with
pure-gtk to work natively on Wayland. In addition to that xwidgets
also enabled and work without glitches even on X server."))))

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
  emacs-next-pgtk
  ;; (rde-emacs-instead-of-emacs
  ;;  )
  emacs-rde-use-package
  emacs-magit
  ;; emacs-ivy
  ))

;; (specifications->manifest
;;  '("emacs"))
