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

  (keyboard-layout rde-config-keyboard-layout
		   (default #f))
  ;; (shell rde-config-shell
  ;; 	 (default "bash"))

  (home-directory
   rde-config-home-directory
   (thunked)
   (default (string-append
	     "/home/"
             (rde-config-user-name this-rde-config)))))
(use-modules (gnu system keyboard))

(define dvorak-jcuken
  (keyboard-layout "us,ru" "dvorak,"
		   #:options '("grp:win_space_toggle" "ctrl:nocaps")))

(define rde-cfg
  (rde-config
   (user-name "bob")
   (full-name "Andrew Tropin")
   (email "andrew@trop.in")
   (keyboard-layout dvorak-jcuken)))

(use-modules (rde emacs packages))
(define (rde-other-packages rde-config)
  (list
   (home-generic-service
    'other-packages
    #:packages
    (append
     %rde-emacs-all-packages
     (map specification->package+output
	  '("tmux" "make"
	    ;; "zsh"
	    ;; "qbittorrent"
	    "xdg-utils" "xdg-user-dirs"
	    "youtube-dl"
	    "mpv" "imv" "ffmpeg"
	    "obs" "obs-wlrobs"
	    "curl" "sway"))))))

(use-modules (gnu home-services shells))
(use-modules (gnu home-services shellutils))
(use-modules (gnu packages shells))
(use-modules (gnu packages shellutils))
(use-modules (guix gexp))
(define (rde-zsh rde-config)
  (list
   (simple-service 'set-default-shell-to-zsh
		   home-environment-vars-service-type
		   '(("SHELL" . "zsh")))

   (service home-zsh-plugin-manager-service-type
	    (list zsh-autosuggestions))
   home-zsh-direnv-service

   (service home-zsh-service-type
	    (home-zsh-configuration
	     (xdg-flavor? #t)
	     (zshrc '("\
autoload -U compinit && compinit

alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'
"))))))

(use-modules (gnu home-services gnupg))
(define (rde-gnupg rde-config)
  (list
   (service home-gnupg-service-type
	    (home-gnupg-configuration
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
	     (config
	      `((user
		 ((name . ,(rde-config-full-name rde-config))
		  (email . ,(rde-config-email rde-config))))
		(sendmail
		 ((annotate . #t)))))))))

(define (rde-sway rde-config)
  (list
   (simple-service 'set-wayland-specific-env-vars
		   home-environment-vars-service-type
		   '(("_JAVA_AWT_WM_NONREPARENTING" . "1")))))

(use-modules (gnu packages))
(define (rde-browsers rde-config)
  (list
   (home-generic-service
    'browsers
    #:packages
    (map specification->package+output
	 '("ungoogled-chromium-wayland" "ublock-origin-chromium" "nyxt")))))


(define rde-features
  (list
   rde-zsh
   rde-gnupg
   rde-ssh
   rde-git
   rde-sway
   rde-browsers
   rde-other-packages))

(define (get-rde-services config features)
  (append-map (lambda (item) (item config)) features))

;; (generic-service
     ;;  'youtube-dl
     ;;  ;; #:files `(("config/youtube-dl/config"
     ;;  ;; 		 ,(plain-file "ytdl-config"
     ;;  ;; 			      "--netrc /home/bob/.authinfo.gpg\n")))
     ;;  #:packages (list youtube-dl))

     ;; (service home-shell-profile-service-type)

;; (simple-service
     ;;  'set-some-vars home-environment-vars-service-type
     ;;  '(("QT_QPA_PLATFORM" . "wayland")))


(use-modules (guix gexp) (gnu packages linux))
(define ixy-he
  (home-environment
   (keyboard-layout dvorak-jcuken)
   (services
    (append
     (get-rde-services
      rde-cfg
      rde-features)

     (list
      (simple-service
       'set-brightness-on-login home-run-on-first-login-service-type
       (list #~(string-append #$(file-append light "/bin/light") " -S 100"))))

     ))))

ixy-he
