(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (rde emacs packages)
  #:use-module (rde gnupg packages))

(use-modules
 (gnu packages)
 (rde emacs packages)
 (rde gnupg packages))

(define-public %rde-base-packages
  (map specification->package+output
       '("tmux" "openssh" "git" "make"
	 "pavucontrol"
	 "ungoogled-chromium-wayland"
	 "mpv" "imv")))

(define-public %rde-all-packages
  (append
   %rde-base-packages
   %rde-gnupg-packages
   %rde-emacs-all-packages))
