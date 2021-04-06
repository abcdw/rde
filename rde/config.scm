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

(use-modules (gnu home-services xdg))
(define (rde-xdg rde-config)
  (list
   (service home-xdg-base-directories-service-type)
   (service home-xdg-user-directories-service-type
	    (home-xdg-user-directories-configuration
	     (music "$HOME/music")
	     (publicshare "$HOME")
	     (templates "$HOME")))))

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
	    ;; "qbittorrent"
	    "ripgrep"
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

;; TODO: Maybe make C-m/C-j in isearch accept current candidate
;; instead of just closing isearch
(define (rde-zsh rde-config)
  (list
   (simple-service 'set-default-shell-to-zsh
		   home-environment-vars-service-type
		   `(("SHELL" . ,(file-append zsh "/bin/zsh"))))

   ;; zsh-autosuggestions is very cool plugin, but a little
   ;; distractive, I find them a little against Attention-friendly
   ;; principle
   home-zsh-autosuggestions-service

   ;; https://github.com/purcell/envrc
   ;; home-zsh-direnv-service
   (service home-zsh-service-type
	    (home-zsh-configuration
	     (xdg-flavor? #t)
	     (zshrc (list
		     (slurp-file-gexp (local-file "./zsh/zshrc"))))))))

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
   rde-xdg
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
