(define-module (rde packages)
  #:use-module (gnu packages))
  
(define-public %rde-base-packages
  (map specification->package+output
		   '("tmux")))
