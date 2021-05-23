(define-module (rde features gnupg)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu services security-token)

  #:export (feature-gnupg))

(define* (feature-gnupg
	  #:key gpg-primary-key
	  (gpg-ssh-agent? #t)
	  (pinentry-flavor 'qt)
	  (gpg-smart-card? #f))
  "Sets up gnupg, if SSH-AGENT? specified also sets up gpg's ssh-agent
and provides GPG-PRIMARY-KEY value for other features."

  (ensure-pred string? gpg-primary-key)
  (ensure-pred boolean? gpg-ssh-agent?)
  (ensure-pred boolean? gpg-smart-card?)
  (ensure-pred pinentry-flavor? pinentry-flavor)
  ;; TODO: check pinentry flavor

  (define (home-gnupg-services _)
    "Return a list of home-services, required for gnupg to operate."
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
		 (ssh-agent? gpg-ssh-agent?)
		 (pinentry-flavor pinentry-flavor)))))))

  (define (system-gnupg-services _)
    "Return a list of home-services, required for gnupg to operate."
    (list
     (if gpg-smart-card?
	 (service pcscd-service-type)
	 #f)))

  (feature
   (name 'gnupg)
   (values (append
	    (make-feature-values gpg-primary-key gpg-ssh-agent?)
	    (if gpg-ssh-agent?
		'((ssh-agent? . #t))
		'())))
   (home-services-getter home-gnupg-services)
   (system-services-getter system-gnupg-services)))
