(define-module (rde features emacs)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde emacs packages)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home-services xdg)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (feature-emacs
	    feature-emacs-faces
	    feature-emacs-completion
	    feature-emacs-input-method
	    feature-emacs-project
	    feature-emacs-magit
	    feature-emacs-org-mode
	    feature-emacs-org-roam
	    feature-emacs-message
	    feature-emacs-erc
	    feature-emacs-telega))

(define* (feature-emacs
	  #:key
	  (package emacs-next-pgtk-latest)
	  (emacs-server-mode? #t)
	  (additional-elisp-packages '()))
  "Setup and configure GNU Emacs."
  (ensure-pred boolean? emacs-server-mode?)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)
  (ensure-pred package? package)

  (define emacs-client (file-append package "/bin/emacsclient"))
  (define emacs-client-create-frame
    (program-file "emacs-client-create-frame"
		  #~(apply system*
			   #$(file-append package "/bin/emacsclient")
			   "--create-frame"
			   (cdr (command-line)))))
  (define emacs-client-no-wait
    (program-file "emacs-client-no-wait"
		  #~(apply system*
			   #$(file-append package "/bin/emacsclient")
			   "--no-wait"
			   (cdr (command-line)))))
  (define emacs-editor
    (program-file "emacs-editor"
		  #~(apply system*
			   #$(file-append package "/bin/emacs")
			   "--no-splash"
			   (cdr (command-line)))))

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
	 (package package)
	 (elisp-packages (cons* emacs-modus-themes additional-elisp-packages))
	 (server-mode? emacs-server-mode?)
	 (xdg-flavor? #t)
	 (init-el
	  `((setq user-full-name ,full-name)
	    (setq user-mail-address ,email)
	    ,#~""
	    (setq custom-file
		  (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			  "/emacs/custom.el"))
	    (load custom-file t)
	    ,#~""
	    (define-key global-map (kbd "M-/") 'hippie-expand)

	    (column-number-mode 1)
	    (save-place-mode 1)
	    (show-paren-mode 1)

	    (setq-default indent-tabs-mode nil)
	    (setq save-interprogram-paste-before-kill t)
	    (setq mouse-yank-at-point t)
	    (setq require-final-newline t)

            (add-hook 'prog-mode-hook
                      (lambda () (setq show-trailing-whitespace t)))

	    (load-theme 'modus-operandi t)))
	 (early-init-el
	  `(,(slurp-file-gexp (local-file "../emacs/early-init.el"))))
	 ;;; TODO: Rebuilding packages with emacs will be useful for
	 ;;; native-comp, but for some reason dash.el fails to build,
	 ;;; need to investigate the issue.
	 ;; (rebuild-elisp-packages? #t)
	 ))
       (simple-service 'emacs-set-default-editor
		       home-environment-variables-service-type
		       `(("ALTERNATE_EDITOR" . ,emacs-editor)
			 ("VISUAL" . ,emacs-client-no-wait)))
       (when (get-value 'sway config)
 	 (simple-service
	  'emacs-update-display-variable-on-sway-start
	  home-sway-service-type
	  `((exec
	     ,(program-file
	       "update-emacs-env-variables"
	       #~(system*
		  #$emacs-client "--eval"
		  (string-append
		   "(setenv \"DISPLAY\" \"" (getenv "DISPLAY") "\")"))))))))))

  (feature
   (name 'emacs)
   (values (append
	    `((emacs . #t))
	    (make-feature-values emacs-editor emacs-client
                                 emacs-client-create-frame
                                 emacs-client-no-wait
                                 emacs-server-mode?)))
   (home-services-getter emacs-home-services)))

(define (strip-emacs-name p)
  (let ((name (package-name p)))
    (string->symbol
     (if (string-prefix? "emacs-" name)
         (string-drop name (string-length "emacs-"))
         name))))

(define* (feature-emacs-input-method
	  #:key
	  (input-method "cyrillic-dvorak")
	  (input-method-package emacs-cyrillic-dvorak-im))
  "Configure input-method for GNU Emacs.  Allows to use other layouts
with emacs, whithout losing ability to use keybindings.  Supported
both Emacsy toggle-input-method (C-\\) and system layout switching by
utilizing reverse-im package."

  (define (emacs-input-method-home-services config)
    "Returns home services related to input-method."
    (let* ((configure-input-method
	    (elisp-configuration-package
	     "configure-input-method"
	     `((with-eval-after-load
		'mule
		(require ',(strip-emacs-name input-method-package))
		(setq default-input-method ,input-method)
		(require 'reverse-im))
	       (with-eval-after-load
		'reverse-im
		(setq reverse-im-input-methods ,input-method)
		(reverse-im-mode 1)))
	       #:elisp-packages (list emacs-reverse-im input-method-package))))
      (list
       (simple-service
	'emacs-input-method-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-input-method)))))))

  (feature
   (name 'emacs-input-method)
   (values `((emacs-input-method . #t)))
   (home-services-getter emacs-input-method-home-services)))


(define* (feature-emacs-message
	  #:key
	  (smtp-server #f)
	  (smtp-port 587))
  "Configure email capabilities provided by message.el for GNU Emacs."
  (ensure-pred string? smtp-server)
  (ensure-pred integer? smtp-port)

  (define (emacs-message-home-services config)
    "Returns home services related to message.el."
    (let* ((emacs-command (get-value 'emacs-command config "emacs"))
	   (configure-message
	    (elisp-configuration-package
	     "configure-message"
	     `((with-eval-after-load
		'message
		(setq send-mail-function 'smtpmail-send-it)
		(setq smtpmail-smtp-server ,smtp-server)
		(setq smtpmail-smtp-service ,smtp-port)

		(setq message-auto-save-directory
		      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			      "/emacs/mail-drafts")))))))

      (list
       (simple-service
	'emacs-message-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-message))))

       (simple-service
	'emacs-xdg-mailto-handler
	home-xdg-mime-applications-service-type
	(home-xdg-mime-applications-configuration
	 (default '((x-scheme-handler/mailto . emacs-mailto.desktop)))
	 (desktop-entries
	  (list
	   (xdg-desktop-entry
	    (file "emacs-mailto")
	    (name "Emacs (Client) [mailto:]")
	    (type 'application)
	    (config
	     `((exec . ,(file-append
			 (program-file
			  "emacs-mailto"
			  #~(system
			     (string-append
			      #$emacs-command
			      " --eval '(browse-url-mail \""
			      (car (cdr (command-line))) "\")'")))
			 " %u"))))))))))))

  (feature
   (name 'emacs-message)
   (values `((emacs-message . #t)))
   (home-services-getter emacs-message-home-services)))
(define* (feature-emacs-erc
	  #:key
	  ;; (emacs-client? #f)
	  (erc-server "irc.libera.chat")
	  (erc-port 6697)
	  (erc-nick #f)
	  (erc-autojoin-channels-alist '()))
  "Configure GNU Emacs IRC client."
  (ensure-pred string? erc-server)
  (ensure-pred integer? erc-port)
  (ensure-pred maybe-string? erc-nick)
  (ensure-pred list? erc-autojoin-channels-alist)

  (define (emacs-erc-home-services config)
    "Returns home services related to ERC."
    (let* ((emacs-command (get-value 'emacs-command config "emacs"))
	   (configure-erc
	    (elisp-configuration-package
	     "configure-erc"
	     `((with-eval-after-load
		'erc
		(setq erc-server ,erc-server)
		(setq erc-port ,erc-port)
		,@(if erc-nick `((setq erc-nick ,erc-nick)) '())
		(setq erc-autojoin-channels-alist
		      ',erc-autojoin-channels-alist)

		(setq erc-fill-static-center 14)
		(setq erc-fill-function 'erc-fill-static)
		(setq erc-fill-column 86)

		(setq erc-track-visibility nil)

		(define-key erc-mode-map (kbd "s-b") 'erc-switch-to-buffer)
		)))))

      (list
       (simple-service
	'emacs-erc-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-erc))))

       (simple-service
	'xdg-emacs-erc
	home-xdg-mime-applications-service-type
	(home-xdg-mime-applications-configuration
	 (desktop-entries
	  (list
	   (xdg-desktop-entry
	    (file "emacs-erc")
	    (name "Emacs IRC client")
	    (type 'application)
	    (config
	     `((exec . ,(program-file
			 "emacs-erc"
			 #~(system
			    (string-append
			     #$emacs-command
			     " --eval '(erc-tls)'"))))))))))))))

  (feature
   (name 'emacs-erc)
   (values `((emacs-erc . #t)))
   (home-services-getter emacs-erc-home-services)))

(define* (feature-emacs-telega)
  "Configure telega.el for GNU Emacs"

  (define (emacs-telega-home-services config)
    "Returns home services related to telega-el."
    (let* ((emacs-client (get-value 'emacs-client config "emacs"))
	   (configure-telega
	    (elisp-configuration-package
	     "configure-telega"
	     `((with-eval-after-load
		'telega
		(define-key telega-chat-mode-map (kbd "s-b") 'telega-switch-buffer)
		(define-key telega-root-mode-map (kbd "s-b") 'telega-switch-buffer)

                (setq telega-emoji-company-backend 'telega-company-emoji)
                (defun my-telega-chat-mode ()
                  (set (make-local-variable 'company-backends)
                       (append (list telega-emoji-company-backend
                                     'telega-company-username
                                     'telega-company-hashtag)
                               (when (telega-chat-bot-p telega-chatbuf--chat)
                                 '(telega-company-botcmd))))
                  (company-mode 1))
                (add-hook 'telega-chat-mode-hook 'my-telega-chat-mode)

		(setq telega-completing-read-function completing-read-function)))
	     #:elisp-packages (list emacs-telega))))

      (list
       (simple-service
	'emacs-telega-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-telega))))

       (simple-service
	'emacs-xdg-tg-handler
	home-xdg-mime-applications-service-type
	(home-xdg-mime-applications-configuration
	 (default '((x-scheme-handler/tg . emacs-telega.desktop)))
	 (desktop-entries
	  (list
	   (xdg-desktop-entry
	    (file "emacs-telega")
	    (name "Emacs (Client) [tg://]")
	    (type 'application)
	    (config
	     `((exec . ,(file-append
			 (program-file
			  "emacs-telega"
			  #~(system*
                             #$emacs-client
                             "--create-frame"
			     "--eval"
			     (string-append
			      "(progn
(if (and (boundp 'telega--status) (equal telega--status \"Ready\"))
 (telega-browse-url \"" (car (cdr (command-line))) "\")"
"
 (telega)
 (add-hook 'telega-ready-hook
  (lambda ()
   (telega-browse-url \"" (car (cdr (command-line))) "\")))"
"))")))
			 " %u"))))))))))))

  (feature
   (name 'emacs-telega)
   (values `((emacs-telega . #t)))
   (home-services-getter emacs-telega-home-services)))

(define* (feature-emacs-org-mode)
  "Configure org-mode for GNU Emacs."
  (define (emacs-org-mode-home-services config)
    "Returns home services related to org-mode."
    (let* ((configure-org-mode
	    (elisp-configuration-package
	     "configure-org-mode"
	     `((with-eval-after-load
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
	     `((with-eval-after-load
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
	     `((with-eval-after-load
		'minibuffer

		(setq enable-recursive-minibuffers t)

		(require 'orderless)
		(require 'savehist)
		(require 'vertico)
		(require 'corfu)
		(require 'marginalia)
		(require 'embark)
		(require 'consult))
	       (with-eval-after-load
		'orderless
		(setq completion-styles '(orderless))
		(setq completion-category-overrides
		      '((file (styles . (partial-completion))))))

	       (with-eval-after-load
		'savehist
		(setq savehist-file
		      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			      "/emacs/history"))
		(savehist-mode 1))

	       (with-eval-after-load
		'embark
		(define-key global-map (kbd "s-e") 'embark-act))

	       (with-eval-after-load
		'consult
		;; TODO: Move to feature-emacs-buffers
		(define-key global-map (kbd "s-w") 'kill-current-buffer)
		(define-key global-map (kbd "s-o") 'other-window)
		(define-key global-map (kbd "s-b") 'consult-buffer)

		(define-key global-map (kbd "M-y") 'consult-yank-pop))
	       (with-eval-after-load 'vertico (vertico-mode 1))
	       (with-eval-after-load 'corfu (corfu-global-mode 1))
	       (with-eval-after-load
		'marginalia
		;; FIXME: Temporary disable annotations for describe-variables.
		;; See: <https://github.com/masm11/emacs/issues/104>
		(setf (alist-get 'variable marginalia-annotator-registry)
		      '(none builtin marginalia-annotate-variable))
		(marginalia-mode 1)))

	     #:elisp-packages
	     (list emacs-orderless emacs-marginalia
		   emacs-vertico emacs-corfu
		   emacs-consult emacs-embark-next))))
      
      (list
       (simple-service
	'emacs-completion-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-completion)))))))

  (feature
   (name 'emacs-completion)
   (values `((emacs-completion . #t)))
   (home-services-getter emacs-completion-home-services)))

(define* (feature-emacs-project)
  "Configure project.el for GNU Emacs."
  (define (emacs-project-home-services config)
    "Returns home services related to project.el."
    (let* ((configure-project
	    (elisp-configuration-package
	     "configure-project"
	     `((with-eval-after-load
		'project
		(with-eval-after-load
		 'consult
		 (setq consult-project-root-function
		       (lambda ()
			 (when-let (project (project-current))
				   (car (project-roots project)))))))))))
      ;; TODO: https://github.com/muffinmad/emacs-ibuffer-project
      (list
       (simple-service
	'emacs-project-configurations
	home-emacs-service-type
	(home-emacs-extension
	 (elisp-packages (list configure-project)))))))

  (feature
   (name 'emacs-project)
   (values `((emacs-project . #t)))
   (home-services-getter emacs-project-home-services)))

;; (define* (feature-emacs-window)
;;   "Configure window.el for GNU Emacs."
;;   (define (emacs-project-home-services config)
;;     "Returns home services related to project.el."
;;     (let* ((configure-project
;; 	    (elisp-configuration-package
;; 	     "configure-project"
;; 	     `((with-eval-after-load
;; 		'project
;; 		)))))
;;       (list
;;        (simple-service
;; 	'emacs-project-configurations
;; 	home-emacs-service-type
;; 	(home-emacs-extension
;; 	 (elisp-packages (list configure-project)))))))

;;   (feature
;;    (name 'emacs-project)
;;    (values `((emacs-project . #t)))
;;    (home-services-getter emacs-project-home-services)))

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
;; TODO: feature-series-tracker https://github.com/MaximeWack/seriesTracker
