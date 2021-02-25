(define-module (gnu home-services gnupg)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:use-module (guix records)

  #:export (home-gnupg-service-type
	    home-gnupg-configuration))

(define-record-type* <home-gnupg-configuration>
  home-gnupg-configuration make-home-gnupg-configuration
  home-gnupg-configuration?
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

(define (add-home-gnupg-agent-to-shepherd config)
  (let ((provision-list `(gpg-agent
			  ,@(if (home-gnupg-configuration-ssh-agent config)
				'(ssh-agent) '()))))
    (list
     (shepherd-service
      (documentation "Run and control gpg-agent.")
      (provision provision-list)
      (start #~(make-system-constructor "gpg-connect-agent /bye"))
      (stop #~(make-system-destructor "gpgconf --kill gpg-agent"))))))

(define home-gnupg-service-type
  (service-type (name 'home-gnupg)
                (extensions
                 (list (service-extension
			home-environment-vars-service-type
                        add-ssh-agent-socket)
		       (service-extension
			home-shepherd-service-type
			add-home-gnupg-agent-to-shepherd)
		       (service-extension
			home-profile-service-type
			(lambda (config)
			  (list (home-gnupg-configuration-package config))))))
		(default-value (home-gnupg-configuration))
                (description "Configure and install gpg-agent.")))
