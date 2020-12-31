(define-module (rde home manifest)
  #:use-module (rde packages)
  #:use-module (guix profiles))

(packages->manifest
 %rde-all-packages)
