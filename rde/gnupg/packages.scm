(define-module (rde gnupg packages)
  #:use-module (gnu packages))

(define-public %rde-gnupg-packages
  (map specification->package+output
       '("gnupg" "pinentry-gnome3" "pinentry-qt")))
