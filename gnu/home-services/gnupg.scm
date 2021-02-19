(define-module (gnu home-services gnupg)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)

  #:export (home-gnupg-service-type))

(define (add-ssh-agent-socket %)
  "Adds SSH_AUTH_SOCK variable to user's environment."
  `(("SSH_AUTH_SOCK" "$(" ,(file-append gnupg "/bin/gpgconf") " --list-dirs agent-ssh-socket)")))

(define (add-gnupg-agent-to-services %)
  (list
   (shepherd-service
    (documentation "Run and control gpg-agent.")
    (provision '(gpg-agent ssh-agent))
    (start #~(make-system-constructor "gpg-connect-agent /bye"))
    (stop #~(make-system-destructor "gpgconf --kill gpg-agent")))))

(define home-gnupg-service-type
  (service-type (name 'home-gnupg)
                (extensions
                 (list (service-extension
			home-environment-vars-service-type
                        add-ssh-agent-socket)
		       (service-extension
			home-shepherd-service-type
			add-gnupg-agent-to-services)
		       (service-extension
			home-profile-service-type
			(lambda (%) `(,gnupg)))))
		(default-value #f)
                (description "Configures and installs gpg-agent.")))
