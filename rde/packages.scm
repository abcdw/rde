(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (rde emacs packages)
  #:use-module (rde gnupg packages)
  #:use-module (rde obs))


(define-public %rde-base-packages
  (map specification->package+output
       '("tmux" "openssh" "git" "make"
	 "xdg-utils"
	 "ungoogled-chromium-wayland"
	 "pavucontrol"
	 ;; "obs-next" "obs-wlrobs"
	 "mpv" "imv" "ffmpeg")))

(define-public %rde-all-packages
  (append
   ;; '(obs-next)
   %rde-base-packages
   %rde-gnupg-packages
   %rde-emacs-all-packages))
