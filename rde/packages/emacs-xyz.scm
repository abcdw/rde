(define-module (rde packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-justify-kp
 (let ((commit "385e6b8b909ae0f570f30101cec3677e21c9e0a0"))
  (package
   (name "emacs-justify-kp")
   (version "20171119")
   (home-page "https://github.com/qzdl/justify-kp")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "13fylx4mvw7cgzd2mq060x43b1x7g5vdf16jm49c31f6b3jj1qi0"))))
   (build-system emacs-build-system)
   (inputs (list emacs-dash emacs-s))
   (synopsis "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (description
    "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (license license:gpl3+))))


