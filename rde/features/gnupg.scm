(define-module (rde features gnupg)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services wm)
  #:use-module (gnu services security-token)
  #:use-module (guix gexp)

  #:export (feature-gnupg))

(define* (feature-gnupg
	  #:key gpg-primary-key
	  (gpg-ssh-agent? #t)
	  (pinentry-flavor 'qt)
	  (gpg-smart-card? #f)
          (default-ttl 86400))
  "Sets up gnupg, if SSH-AGENT? specified also sets up gpg's ssh-agent
and provides GPG-PRIMARY-KEY value for other features."

  (ensure-pred string? gpg-primary-key)
  (ensure-pred boolean? gpg-ssh-agent?)
  (ensure-pred boolean? gpg-smart-card?)
  (ensure-pred pinentry-flavor? pinentry-flavor)
  (ensure-pred integer? default-ttl)

  (define (home-gnupg-services config)
    "Return a list of home-services, required for gnupg to operate."
    (list
     (when (get-value 'sway config)
       (simple-service
	'gnupg-updatestartuptty-on-sway-launch
	home-sway-service-type
	`((exec gpg-connect-agent updatestartuptty /bye >/dev/null)
          (,#~""))))

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
		 (extra-config
                  ;; TTL for smart-cards doesn't make sense
                  `(,@(if (not gpg-smart-card?)
                          `((default-cache-ttl . ,default-ttl)
                            (default-cache-ttl-ssh . ,default-ttl)
                            (max-cache-ttl . ,default-ttl)
                            (max-cache-ttl-ssh . ,default-ttl))
                          '())))
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
