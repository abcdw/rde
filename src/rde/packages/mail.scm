(define-module (rde packages mail)
  #:use-module (gnu packages mail)
  #:use-module (guix deprecation)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-deprecated/public-alias goimapnotify-next goimapnotify)
