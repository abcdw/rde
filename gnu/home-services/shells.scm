(define-module (gnu home-services shells)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
  #:use-module (gnu packages shells)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (home-shell-profile-service-type
	    home-shell-profile-configuration))

(define path? string?)
(define (serialize-path field-name val) val)

(define-configuration home-shell-profile-configuration
  (he-symlink-path
   (path "~/.guix-home-environment")
   "Path to home-environment symlink, which contains files that have
to be sourced or executed by login shell.  This path will be set
automatically by home-environment.")
  (profile
   (text-config '())
   "\
@code{home-shell-profile} is instantiated automatically by
@code{home-environment}, DO NOT create this service manually, it can
only be extended.

@code{profile} is a list of strings or gexps, which will go to
@file{~/.profile}.  By default @file{~/.profile} contains the
initialization code, which have to be evaluated by login shell to make
home-environment's profile avaliable to the user, but other commands
can be added to the file if it is really necessary.

In most cases shell's configuration files are preferred places for
user's customizations.  Extend home-shell-profile service only if you
really know what you do."))

(define (add-shell-profile-file config)
  `(("profile"
     ,(mixed-text-file
       "shell-profile"
       (format #f "\
HOME_ENVIRONMENT=\"~a\"
source $HOME_ENVIRONMENT/setup-environment
sh $HOME_ENVIRONMENT/on-login\n"
	       (home-shell-profile-configuration-he-symlink-path config))
       (serialize-configuration
	config
	(filter-configuration-fields
	 home-shell-profile-configuration-fields '(profile)))))))

(define (add-profile-extensions config extensions)
  (home-shell-profile-configuration
   (inherit config)
   (profile
    (append (home-shell-profile-configuration-profile config)
	    extensions))))

(define home-shell-profile-service-type
  (service-type (name 'home-shell-profile)
                (extensions
                 (list (service-extension
			home-files-service-type
			add-shell-profile-file)))
		(compose concatenate)
		(extend add-profile-extensions)
		(default-value (home-shell-profile-configuration))
                (description "\
Create @file{~/.profile}, which is used for environment initialization
of POSIX compatible login shells.  Can be extended with a list of strings or
gexps.")))
