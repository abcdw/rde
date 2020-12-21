(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (rde emacs packages))


(define-public %rde-base-packages
  (map specification->package+output
       '("tmux")))

(define-public %rde-all-packages
  (append
   %rde-base-packages
   %rde-emacs-all-packages))
