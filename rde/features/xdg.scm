(define-module (rde features xdg)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home-services xdg)

  #:export (feature-xdg-base-directories))


(define* (feature-xdg-base-directories
	  #:key
	  (xdg-cache-home "$HOME/.cache")
	  (xdg-config-home "$HOME/.config")
	  (xdg-data-home "$HOME/.local/share")
	  (xdg-runtime-dir "${XDG_RUNTIME_DIR:-/run/user/$UID}")
	  (xdg-log-home "$HOME/.local/var/log")
	  (xdg-state-home "$HOME/.local/var/lib"))
  "Set XDG base directories and extensions."
  (ensure-pred string? xdg-cache-home)
  (ensure-pred string? xdg-config-home)
  (ensure-pred string? xdg-data-home)
  (ensure-pred string? xdg-runtime-dir)
  (ensure-pred string? xdg-log-home)
  (ensure-pred string? xdg-state-home)

  (define (xdg-base-directories-home-services config)
    (list (simple-service
	   'substitute-xdg-base-directories-values
	   home-xdg-base-directories-service-type
	   (home-xdg-base-directories-configuration
	    (cache-home xdg-cache-home)
	    (config-home xdg-config-home)
	    (data-home xdg-data-home)
	    (runtime-dir xdg-runtime-dir)
	    (log-home xdg-log-home)
	    (state-home xdg-state-home)))))

  (feature
   (name 'xdg-base-directories)
   (home-services-getter xdg-base-directories-home-services)
   (values (append
	    '((xdg-base-directories . #t))
	    (make-feature-values xdg-cache-home
				 xdg-config-home
				 xdg-data-home
				 xdg-runtime-dir
				 xdg-log-home
				 xdg-state-home)))))
