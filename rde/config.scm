(define-module (rde config)
  #:use-module (gnu home)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services-utils)
  #:use-module (guix records)
  #:use-module (srfi srfi-1))

(define-record-type* <rde-config> rde-config
  make-rde-config
  rde-config?
  this-rde-config

  (user-name rde-config-user-name)
  (full-name rde-config-full-name)
  (email rde-config-email)
  (mails rde-config-mails
	 (default '()))

  (gpg-sign-key rde-config-gpg-sign-key
		(default #f))

  (keyboard-layout rde-config-keyboard-layout
		   (default #f))
  ;; (shell rde-config-shell
  ;; 	 (default "bash"))

  (home-directory
   rde-config-home-directory
   (thunked)
   (default (string-append
	     "/home/"
             (rde-config-user-name this-rde-config))))

  (features rde-config-features
	    (default '()))

  ;; Not intended for manual setting, needed for internal
  ;; machinery. User have to use features instead.
  (services
   rde-config-services
   (thunked)
   (default
     (get-rde-services
      this-rde-config
      (rde-config-features this-rde-config)))))

(define (get-rde-services config features)
  (append-map (lambda (item) (item config)) features))

(define* (get-home-environment rde-config #:key (additional-services '()))
  (home-environment
   (keyboard-layout (rde-config-keyboard-layout rde-config))
   (home-directory (rde-config-home-directory rde-config))
   (services
    (append
     additional-services
     (rde-config-services rde-config)))))


(use-modules (gnu system keyboard))

(define dvorak-jcuken
  (keyboard-layout "us,ru" "dvorak,"
		   #:options '("grp:win_space_toggle" "ctrl:nocaps")))

(use-modules (gnu home-services xdg))
(use-modules (gnu packages freedesktop))
(use-modules (guix gexp))
(define (rde-xdg rde-config)
  (list
   (service
    home-xdg-mime-applications-service-type
    (home-xdg-mime-applications-configuration
     (added '((x-scheme-handler/magnet . torrent.desktop)))
     (default '((x-scheme-handler/mailto . emacs-mailto.desktop)
		(inode/directory . file.desktop)))
     (removed '((inode/directory . thunar.desktop)))
     (desktop-entries
      (list
       (xdg-desktop-entry
	(file "emacs-mailto")
	(name "Handler for mailto:")
	(type 'application)
	(config
	 `((exec . ,(program-file
		     "emacs-mailto"
		     #~(system
			(string-append
			 "emacsclient -c --eval '(browse-url-mail \""
			 (car (cdr (command-line))) "\")'")))))))
       (xdg-desktop-entry
	(file "file")
	(name "File manager")
	(type 'application)
	(config
	 '((exec . "emacsclient -c -a emacs %u"))))
       (xdg-desktop-entry
	(file "text")
	(name "Text editor")
	(type 'application)
	(config
	 '((exec . "emacsclient -c -a emacs %u"))))))))

   (home-generic-service
    'home-xdg-packages
    #:packages
    (list xdg-utils xdg-user-dirs desktop-file-utils))
   (service home-xdg-user-directories-service-type
	    (home-xdg-user-directories-configuration
	     (music "$HOME/music")
	     (videos "$HOME/vids")
	     (pictures "$HOME/pics")
	     (documents "$HOME/docs")
	     (download "$HOME/dl")
	     (desktop "$HOME")
	     (publicshare "$HOME")
	     (templates "$HOME")))))

(use-modules (rde emacs packages))
(use-modules (gnu home-services emacs))
(use-modules (gnu packages emacs))
(use-modules (gnu packages emacs-xyz))
(use-modules (guix gexp))

(define (rde-emacs rde-config)
  (list
   ;; (simple-service
   ;;  'add-emacs-package
   ;;  home-emacs-service-type
   ;;  (home-emacs-extension
   ;;   (elisp-packages (list emacs-treemacs))))

   (service home-emacs-service-type
	    (home-emacs-configuration
	     (package emacs-next-pgtk)
	     (elisp-packages (cons*
			      emacs-rde-default-init
			      emacs-yaml-mode
			      %rde-additional-emacs-packages))
	     (server-mode? #t)
	     (xdg-flavor? #f)
	     (init-el
	      `((load-file ,(local-file "./emacs/test-init.el"))
		,(slurp-file-gexp (local-file "./emacs/init.el"))))
	     (early-init-el
	      `(,(slurp-file-gexp (local-file "./emacs/early-init.el"))))
	     ;; (rebuild-elisp-packages? #t)
	     ))))

(define (rde-other-packages rde-config)
  (list
   (simple-service
    'xdg-friendly-env-vars
    home-environment-vars-service-type
    '(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")))
   (home-generic-service
    'rde-other-packages
    #:packages
    (map specification->package+output
	 '("make"
	   "hicolor-icon-theme" "adwaita-icon-theme"
	   "alsa-utils"
	   "ripgrep"
	   "youtube-dl"
	   "mpv" "imv" "ffmpeg"
	   "obs" "obs-wlrobs"
	   "curl")))))

(use-modules (gnu home-services shells))
(use-modules (gnu home-services shellutils))
(use-modules (gnu packages shells))
(use-modules (gnu packages shellutils))
(use-modules (guix gexp))

;; TODO: Maybe make C-m/C-j in isearch accept current candidate
;; instead of just closing isearch
(define (rde-zsh rde-config)
  (list
   (simple-service 'set-default-shell-to-zsh
		   home-environment-vars-service-type
		   `(("SHELL" . ,(file-append zsh "/bin/zsh"))))

   ;; zsh-autosuggestions is very cool plugin, but a little
   ;; distractive, I find it a little against Attention-friendly
   ;; principle
   (service home-zsh-autosuggestions-service-type)

   ;; https://github.com/purcell/envrc
   ;; home-zsh-direnv-service
   (service home-zsh-service-type
	    (home-zsh-configuration
	     (xdg-flavor? #t)
	     (zshrc (list
		     (slurp-file-gexp (local-file "./zsh/zshrc"))))))))

(define (rde-bash rde-config)
  (list
   (service home-bash-service-type
	    (home-bash-configuration
	     (guix-defaults? #f)
	     (bash-profile '("\
export HISTFILE=\"$XDG_CACHE_HOME\"/.bash_history"))))))

(use-modules (gnu home-services gnupg))
(define (rde-gnupg rde-config)
  (list
   (service home-gnupg-service-type
	    (home-gnupg-configuration
	     (gpg-config
	      (home-gpg-configuration
	       (extra-config
		'((keyid-format . long)
		  (with-subkey-fingerprint . #t)
		  (keyserver . "hkps://keys.openpgp.org")))))
	     (gpg-agent-config
	      (home-gpg-agent-configuration
	       (ssh-agent? #t)
	       (pinentry-flavor 'qt)))))))

(use-modules (gnu home-services ssh))
(define (rde-ssh rde-config)
  (list
   (service home-ssh-service-type
	    (home-ssh-configuration))))

(use-modules (gnu home-services version-control))
(define (rde-git rde-config)
  (list
   (service home-git-service-type
	    (home-git-configuration
	     (ignore
             '("*~"
               "*.\\#\\*"
               "*.\\#*\\#"))
	     (config
	      `((user
		 ((name . ,(rde-config-full-name rde-config))
		  (email . ,(rde-config-email rde-config))
		  ,@(if (rde-config-gpg-sign-key rde-config)
			`((signingkey . ,(rde-config-gpg-sign-key rde-config)))
			'())))
		(commit
		 (,@(if (rde-config-gpg-sign-key rde-config)
			'((gpgsign . #t))
			'())))
		(sendmail
		 ((annotate . #t)))))))))

(use-modules (gnu packages wm))
(use-modules (gnu packages terminals))
(use-modules (gnu packages xdisorg))
(use-modules (gnu home-services shells))

(define (rde-sway rde-config)
  (list
   (simple-service
    'run-sway-on-tty2
    home-bash-service-type
    (home-bash-extension
     (bash-profile '("[[ $(tty) = /dev/tty2 ]] && exec sway"))))
   (home-generic-service
    'home-sway
    #:files `(("config/sway/config"
	       ,(mixed-text-file
		 "sway-config"
		 #~(format #f "output * bg ~a/share/backgrounds/\
sway/Sway_Wallpaper_Blue_1920x1080.png fill\n" #$sway)
		 (slurp-file-gexp (local-file "./sway/config")))))
    #:packages (list sway wofi))
   (home-generic-service
    'home-alacritty
    #:files `(("config/alacritty/alacritty.yml"
	       ,(local-file "../stale/dotfiles/.config/alacritty/alacritty.yml")))
    #:packages (list alacritty))
   (simple-service 'set-wayland-specific-env-vars
		   home-environment-vars-service-type
		   '(("_JAVA_AWT_WM_NONREPARENTING" . "1")))))

(use-modules (gnu packages tmux))
(define (rde-tmux rde-config)
  (list
   (home-generic-service
    'home-tmux
    #:files `(("config/tmux/tmux.conf"
	       ,(local-file "../stale/dotfiles/.tmux.conf" "tmux.conf")))
    #:packages (list tmux))))

(use-modules (gnu packages))
(define (rde-browsers rde-config)
  (list
   (home-generic-service
    'browsers
    #:packages
    (map specification->package+output
	 '("ungoogled-chromium-wayland" "ublock-origin-chromium" "nyxt")))))


(define guix-and-rde-channels
  (with-output-to-string
    (lambda ()
      ((@@ (ice-9 pretty-print) pretty-print)
       '(cons*
	 (channel
	  (name 'rde)
	  (url "https://git.sr.ht/~abcdw/rde")
	  (introduction
	   (make-channel-introduction
	    "257cebd587b66e4d865b3537a9a88cccd7107c95"
	    (openpgp-fingerprint
	     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
	 %default-channels)))))

(use-modules (guix gexp))
(define (rde-guix-channels rde-config)
  (list
   (home-generic-service
    'rde-guix-channels
    #:files `(("config/guix/channels.scm"
	       ,(mixed-text-file "channels.scm" guix-and-rde-channels))))))

(use-modules (gnu home-services password-utils))
(define (rde-password-store rde-config)
  (list
   (service home-password-store-service-type)))

(define rde-features
  (list
   rde-guix-channels
   rde-xdg
   rde-bash
   rde-zsh
   rde-gnupg
   rde-password-store
   rde-ssh
   rde-git
   rde-sway
   rde-emacs
   rde-tmux
   rde-browsers
   rde-other-packages))


;; TODO: Move personal configurations to separate folder
(use-modules (gnu home-services state))
(use-modules (gnu home-services version-control))
(define (working-repos rde-config)
  (define (work-dir path)
    (format #f "~a/work/~a" (rde-config-home-directory rde-config) path))
  (list
   (service
    home-state-service-type
    (list
     (state-git
      (work-dir "gnu/guix")
      "https://git.savannah.gnu.org/git/guix.git/")
     (state-git
      (work-dir "gnu/shepherd")
      "https://git.savannah.gnu.org/git/shepherd.git/")
     (state-git
      (work-dir "notes")
      "git@github.com:abcdw/notes.git")
     (state-git
      (work-dir "rde")
      "git@git.sr.ht:~abcdw/rde"
      #:config
      (serialize-git-config
      #f
      '((core ((repositoryformatversion . "0")
	       (filemode . #t)
	       (bare . #f)
	       (logallrefupdates . #t)))
	(remote origin
		((url . "git@git.sr.ht:~abcdw/rde")
		 (fetch . "+refs/heads/*:refs/remotes/origin/*")))
	(remote github
		((url . "git@github.com:abcdw/rde.git")
		 (fetch . "+refs/heads/*:refs/remotes/github/*")))
	(remote ((pushDefault . "github")))
	(branch master
		((remote . "origin")
		 (merge . "refs/heads/master"))))))))))

(define rde-cfg
  (rde-config
   (user-name "bob")
   (full-name "Andrew Tropin")
   (email "andrew@trop.in")
   (gpg-sign-key "2208D20958C1DEB0")
   (keyboard-layout dvorak-jcuken)
   (features (append
	      rde-features
	      (list working-repos)))))

(use-modules (guix gexp) (gnu packages linux))
(define ixy-he
  (get-home-environment
   rde-cfg
   #:additional-services
   (list
    (simple-service
     'set-brightness-on-first-login home-run-on-first-login-service-type
     #~(system* #$(file-append light "/bin/light") "-S" "100")))))

ixy-he
