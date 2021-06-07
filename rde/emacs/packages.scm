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
  #:use-module (gnu packages sqlite)
  #:use-module (guix build emacs-utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (%rde-emacs-all-packages))

(define-public emacs-next-pgtk-latest
  (let ((commit "ce7a78d0a8e033f04978b0c0762378ba13d3fa64")
        (revision "2"))
    (package/inherit emacs-next
      (name "emacs-next-pgtk-latest")
      (version (git-version "28.0.50" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/emacs.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "06psn92zrv6j3w5daj4w4iz652w5xd6zjg6a5y4j7az5s3v39ivc"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-next)
         ((#:configure-flags flags ''())
          `(cons* "--with-pgtk" "--with-xwidgets" ,flags))))
      (propagated-inputs
       `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
         ("glib-networking" ,glib-networking)))
      (inputs
       `(("webkitgtk" ,webkitgtk)
         ,@(package-inputs emacs-next)))
      (home-page "https://github.com/masm11/emacs")
      (synopsis "Emacs text editor with @code{pgtk} and @code{xwidgets} support")
      (description "This is an unofficial Emacs fork build with a pure-GTK
graphical toolkit to work natively on Wayland.  In addition to that, xwidgets
also enabled and works without glitches even on X server."))))

(define-public emacs-embark-next
  (package
    (inherit emacs-embark)
    (name "emacs-embark-next")
    (version "0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oantolin/embark")
             (commit "2c3ac885252379044afb20353214115eb06a51ae")))
       (sha256
        (base32 "08l3i6zb2wbqiq2qn18qdsgqqf0iibx09bk2d1x89pw3j7r6xgf7"))
       (file-name (git-file-name name version))))))

(define-public emacs-reverse-im
  (package
    (name "emacs-reverse-im")
    (version "0.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a13/reverse-im.el")
             (commit (string-append "v" version))))
       (sha256
        (base32 "05k2zp2hldzq5h6nl8gx79dd8lvfn507ad4x3naichdqgn2013nn"))
       (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (home-page "https://github.com/a13/reverse-im.el")
    (synopsis "Make hotkeys keep working on other input-methods")
    (description "Make hotkeys keep working on other input-methods.")
    (license license:gpl3+)))

(define-public emacs-cyrillic-dvorak-im
  (package
    (name "emacs-cyrillic-dvorak-im")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xFA25E/cyrillic-dvorak-im")
             (commit version)))
       (sha256
        (base32 "12adszd4p9i9glx2chasgq68i6cnxcrwbf5c268jjb5dw4q7ci0n"))
       (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (home-page "https://github.com/xFA25E/cyrillic-dvorak-im")
    (synopsis "Cyrillic input method for dvorak layout")
    (description "Cyrillic input method for dvorak layout.")
    (license license:gpl3+)))


(define %rde-emacs emacs-next-pgtk-latest)

(define %rde-emacs-runtime-packages
  (list ))

(define-public %rde-additional-emacs-packages
  (list emacs-guix
	emacs-telega
	emacs-magit
	emacs-pdf-tools
	emacs-yasnippet))

(define %rde-emacs-all-packages
  (append
   (list %rde-emacs)
   %rde-additional-emacs-packages
   %rde-emacs-runtime-packages))

;; (pretty-print "test")
;; (specifications->manifest
;;  '("emacs"))
