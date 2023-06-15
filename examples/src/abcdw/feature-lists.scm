(define-module (abcdw feature-lists)
  #:use-module (rde features docker)
  #:use-module (rde features virtualization)
  #:use-module (rde features markup)

  #:use-module (rde features presets)

  #:use-module (guix gexp))

(define-public %dev-features
  (list
   (feature-markdown)))

(define-public %virtualization-features
  (list
   (feature-docker)
   (feature-qemu)))

(define-public %general-features
  (append
   rde-base
   rde-desktop
   rde-mail
   rde-cli
   rde-emacs))

(define-public %all-features
  (append
   %virtualization-features
   %dev-features
   %general-features))
