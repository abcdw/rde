;; -*- mode: scheme -*-
(use-modules (gnu home)
	     (gnu home-services)
	     (gnu home-services gnupg)
	     (gnu home-services ssh)
	     (gnu home-services version-control)
	     (gnu home-services files)
	     (gnu home-services shells)
	     (gnu home-services mcron)
	     (gnu services)
	     (gnu packages)
	     (gnu packages linux)
	     (gnu packages admin)
	     (gnu packages gnupg)
	     (gnu system keyboard)
	     (guix gexp))


(define sample-he
  (home-environment
   (home-directory (getenv "HOME"))
   ;; (symlink-name ".guix-home-env")
   (packages (list htop))
   (services
    (list
     (simple-service
      'test-config home-files-service-type
      (list `("config/test.conf"
              ,(plain-file "tmp-file.txt" "hehe"))))
     (service home-bash-service-type
	      (home-bash-configuration
	       (guix-defaults? #f)
	       (bash-profile '("\
export HISTFILE=\"$XDG_CACHE_HOME\"/.bash_history"))))
     (service home-ssh-service-type
	      (home-ssh-configuration
	       (extra-config
		(list
		 (ssh-host "savannah"
			   '((compression . #f)))))))))))

sample-he
