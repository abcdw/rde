(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (rde emacs packages)
  #:use-module (rde gnupg packages)
  #:use-module (rde obs))


(define-public %rde-base-packages
  (map specification->package+output
       '()))

(define-public %rde-all-packages
  (list))
