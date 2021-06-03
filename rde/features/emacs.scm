(define-module (rde features emacs)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde emacs packages)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-emacs
	    feature-emacs-org-mode
	    feature-emacs-magit
	    feature-emacs-faces
	    feature-emacs-completion
	    feature-emacs-org-roam))

(define* (feature-emacs
	  #:key
	  (emacs-server-mode? #t)
	  (additional-elisp-packages '()))
  "Setup and configure GNU Emacs."
  (ensure-pred boolean? emacs-server-mode?)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)
  
  (define (emacs-home-services config)
    "Returns home services related to GNU Emacs."
    (require-value 'full-name config)
    (require-value 'email config)
    (let* ((full-name (get-value 'full-name config))
	   (email     (get-value 'email config)))
      (list
       (service
	home-emacs-service-type
	(home-emacs-configuration
	 (package emacs-next-pgtk-latest)
	 (elisp-packages (append
			  additional-elisp-packages
			  (list emacs-modus-themes)))
	 (server-mode? emacs-server-mode?)
	 (xdg-flavor? #f)
	 (init-el
	  `((setq user-full-name ,full-name)
	    (setq user-mail-address ,email)
	    ,#~""
	    (setq custom-file
		  (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			      "/emacs/custom.el"))
	    (load custom-file t)
	    ,#~""
	    (load-theme 'modus-operandi t)
	    ,#~""
	    (setq message-auto-save-directory
		  (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			  "/emacs/mail-drafts"))
	    ,#~""))
	 (early-init-el
	  `(,(slurp-file-gexp (local-file "../emacs/early-init.el"))))
	 ;;; TODO: Rebuilding packages with emacs will be useful for
	 ;;; native-comp, but for some reason dash.el fails to build,
	 ;;; need to investigate the issue.
	 ;; (rebuild-elisp-packages? #t)
	 )))))

  (feature
   (name 'emacs)
   (values (append
	    '((emacs . #t))
	    (make-feature-values emacs-server-mode?)))
   (home-services-getter emacs-home-services)))

(define* (feature-emacs-org-mode)
  "Configure org-mode for GNU Emacs."
  (define (emacs-org-mode-home-services config)
    "Returns home services related to org-mode."
    (let* ((configure-org-mode
	    (elisp-configuration-package
	     "configure-org-mode"
	     `(,#~";;;###autoload"
	       (with-eval-after-load
		'org
		(progn
		 (setq org-adapt-indentation nil)
		 (setq org-edit-src-content-indentation 0)
		 (setq org-startup-indented t)))))))
  (list
   (simple-service
    'emacs-org-mode-configurations
    home-emacs-service-type
    (home-emacs-extension
     (elisp-packages (list emacs-org configure-org-mode)))))))

  (feature
   (name 'emacs-org-mode)
   (values `((emacs-org-mode . #t)))
   (home-services-getter emacs-org-mode-home-services)))

(define* (feature-emacs-magit)
  "Configure Magit for GNU Emacs."
  (define (emacs-magit-home-services config)
    "Returns home services related to Magit."
    (require-value 'git config)
    (list
     (simple-service
      'emacs-magit-configurations
      home-emacs-service-type
      (home-emacs-extension
       (elisp-packages (list emacs-magit))))))

  (feature
   (name 'emacs-magit)
   (values `((emacs-magit . #t)))
   (home-services-getter emacs-magit-home-services)))


;; TODO: Move font record to apropriate module
(use-modules (rde features fontutils))

;; TODO: Can be useful to have different presets for different
;; environments.  For easier and faster switching.
(define* (feature-emacs-faces)
  "Configure faces for GNU Emacs."
  (define (emacs-faces-home-services config)
    "Returns home services related to faces."
    (require-value 'fonts config)
    (let* ((font-monospace      (get-value 'font-monospace config))
	   (font-sans           (get-value 'font-sans config))

	   (configure-faces
	    (elisp-configuration-package
	     "configure-faces"
	     `(,#~";;;###autoload"
		  (with-eval-after-load
		   'faces
		   (let* ((mono-fn ,(font-name font-monospace))
			  (sans-fn ,(font-name font-sans))
			  (mono (font-spec
				 :name ,(font-name font-monospace)
				 :size   ,(font-size font-monospace)
				 :weight ',(or (font-weight font-monospace) 'normal)))
			  ;; For people coming here years later, only
			  ;; face which can contain size or integer
			  ;; height is default, everything else should
			  ;; set only family or relative height
			  ;; (decimal value), the font-spec even
			  ;; without height/size shouldn't be used.
			  ;; Otherwise text-adjust and other stuff can
			  ;; be broken.
			  (faces `((default ((t (:font ,mono))))
				   (fixed-pitch ((t (:family ,mono-fn))))
				   (button ((t (:inherit (fixed-pitch)))))
				   (variable-pitch ((t (:family ,sans-fn)))))))
		     (dolist (face faces)
			     (custom-set-faces face))

		     (dolist (face faces)
			  (put (car face) 'saved-face nil))))))))
      
      (list
       (simple-service
	'emacs-fonts-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-faces)))))))

  (feature
   (name 'emacs-faces)
   (values `((emacs-faces . #t)))
   (home-services-getter emacs-faces-home-services)))

(define* (feature-emacs-completion)
  "Configure completion system for GNU Emacs."
  (define (emacs-completion-home-services config)
    "Returns home services related to Emacs completion system."
    (let* ((configure-completion
	    (elisp-configuration-package
	     "configure-completion"
	     `(,#~";;;###autoload"
	       (with-eval-after-load
		'minibuffer
		(setq completion-styles '(orderless))
		(setq savehist-file
		      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			      "/emacs/history"))
		(savehist-mode 1))))))
      
      (list
       (simple-service
	'emacs-completion-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-completion emacs-orderless)))))))

  (feature
   (name 'emacs-completion)
   (values `((emacs-completion . #t)))
   (home-services-getter emacs-completion-home-services)))

(define* (feature-emacs-org-roam
	  #:key
	  (org-roam-directory #f))
  "Configure org-rome for GNU Emacs."
  (define (not-boolean? x) (not (boolean? x)))
  (ensure-pred not-boolean? org-roam-directory)

  (define (emacs-org-roam-home-services config)
    "Returns home services related to org-roam."
    (let* ((configure-org-roam
	    (elisp-configuration-package
	     "configure-org-roam"
	     `(,#~";;;###autoload"
	       (progn
		(add-hook 'after-init-hook 'org-roam-mode)
		(with-eval-after-load
		 'org-roam
		 (define-key org-roam-mode-map
		   (kbd "C-c n n") 'org-roam)
		 (define-key org-roam-mode-map
		   (kbd "C-c n f") 'org-roam-find-file)
		 (define-key org-mode-map
		   (kbd "C-c n i") 'org-roam-insert)
		 (setq org-roam-directory ,org-roam-directory)))))))
      
      (list
       (simple-service
	'emacs-completion-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-org-roam emacs-org-roam)))))))

  (feature
   (name 'emacs-org-roam)
   (values `((emacs-org-roam . #t)))
   (home-services-getter emacs-org-roam-home-services)))

;; TODO: feature-emacs-reasonable-keybindings
;; TODO: Fix env vars for emacs daemon
;; https://github.com/purcell/exec-path-from-shell
;; TODO: feature-emacs-epub https://depp.brause.cc/nov.el/
