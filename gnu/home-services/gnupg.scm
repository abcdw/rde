(define-module (gnu home-services gnupg)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu home-services files)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages qt)
  #:use-module (guix gexp)
  #:use-module (guix records)

  #:export (home-gnupg-service-type
	    home-gnupg-configuration))

;; TODO: rename gnupg to gpg-agent and split record for gpg config?
(define-record-type* <home-gnupg-configuration>
  home-gnupg-configuration make-home-gnupg-configuration
  home-gnupg-configuration?
  ;; TODO: Add pinentry-flavour.
  (package     home-gnupg-configuration-package
               (default gnupg))
  (ssh-agent   home-gnupg-configuration-ssh-agent
               (default #f)))

(define (add-ssh-agent-socket config)
  "Adds SSH_AUTH_SOCK variable to user's environment."
  (if (home-gnupg-configuration-ssh-agent config)
      `(("SSH_AUTH_SOCK" .
	 ("$("
	  ,(file-append gnupg "/bin/gpgconf")
	  " --list-dirs agent-ssh-socket)")))
      '()))

(define (add-home-gpg-agent-to-shepherd config)
  (let ((provision-list `(gpg-agent
			  ,@(if (home-gnupg-configuration-ssh-agent config)
				'(ssh-agent) '()))))
    (list
     (shepherd-service
      (documentation "Run and control gpg-agent.")
      (provision provision-list)
      (start #~(make-system-constructor "gpgconf --launch gpg-agent"
	;; 	make-forkexec-constructor
	;; 	(list #$(file-append gnupg "/bin/gpg-agent") "--daemon")
	;; 	#:environment-variables
	;; '("WAYLAND_DISPLAY=wayland-0")
	))
      (stop #~(make-system-destructor "gpgconf --kill gpg-agent"))))))

(define (add-gnupg-configs config)
  `(;; ("gnupg/gpg.conf" ,(plain-file "gpg.conf" "test"))
    ("gnupg/gpg-agent.conf"
     ,(apply
       mixed-text-file "gpg-agent.conf"
       (if (home-gnupg-configuration-ssh-agent config)
	   "enable-ssh-support\n" "")
       ;; NOTE: not really necessary, because if there is only one
       ;; pinentry package in the environment /bin/pinentry will be
       ;; pointing to /bin/pinentry-qt
       (list "pinentry-program " pinentry-qt "/bin/pinentry-qt" "\n")

       ))))

(define (add-gnupg-packages config)
  (append
   (list (home-gnupg-configuration-package config))
   ;; Probably have to be installed by some wayland-related service,
   ;; because it is not a required dependency for pinentry-qt
   ;; TODO: Move to wayland-related service
   `(,qtwayland)))

(define home-gnupg-service-type
  (service-type (name 'home-gnupg)
                (extensions
                 (list (service-extension
			home-environment-vars-service-type
                        add-ssh-agent-socket)
		       (service-extension
			home-shepherd-service-type
			add-home-gpg-agent-to-shepherd)
		       (service-extension
			home-files-service-type
			add-gnupg-configs)
		       (service-extension
			home-profile-service-type
			add-gnupg-packages
			)))
		(default-value (home-gnupg-configuration))
                (description "Configure and install gpg-agent.")))


;; TODO: create services for bash/sway to run this command
;; automatically to make pinentry work.
(define update-gnupg-tty-command
  (list
   (file-append gnupg "/bin/gpg-connect-agent")
   " updatestartuptty /bye\n"))
