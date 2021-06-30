(define-module (rde features emacs)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home-services xdg)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)

  #:export (feature-emacs
	    feature-emacs-faces
	    feature-emacs-completion
	    feature-emacs-input-methods
	    feature-emacs-project
	    feature-emacs-git
	    feature-emacs-eshell
	    feature-emacs-org
	    feature-emacs-org-roam
	    feature-emacs-message
	    feature-emacs-erc
	    feature-emacs-telega
            feature-emacs-which-key

            elisp-configuration-service
            emacs-xdg-service))

(define* (elisp-configuration-service
          name
          #:optional (elisp-expressions '())
          #:key
          (elisp-packages '())
          (autoloads? #t))
  (let* ((configure-package
	  (elisp-configuration-package
	   (string-append "configure-" (symbol->string name))
           elisp-expressions
           #:elisp-packages elisp-packages
           #:autoloads? autoloads?)))
    (simple-service
     (symbol-append 'emacs- name '-configurations)
     home-emacs-service-type
     (home-emacs-extension
      ;; It's necessary to explicitly add elisp-packages here, because
      ;; we want to overwrite builtin emacs packages.  Propagated
      ;; inputs have lowest priority on collisions, that's why we have
      ;; to list those package here in addition to propagated-inputs.
      (elisp-packages (append elisp-packages (list configure-package)))))))

(define* (emacs-xdg-service
          name xdg-name gexp
          #:key (default-for '()))
  (define file-name (string-append "emacs-" (symbol->string name)))
  (define file-file (file-append (program-file file-name gexp) " %u"))
  (define desktop-file (symbol-append 'emacs- name '.desktop))
  (simple-service
   (symbol-append 'emacs-xdg- name)
   home-xdg-mime-applications-service-type
   (home-xdg-mime-applications-configuration
    (default (map (lambda (m) (cons m desktop-file)) default-for))
    (desktop-entries
     (list
      (xdg-desktop-entry
       (file file-name)
       (name xdg-name)
       (config `((exec . ,file-file)))
       (type 'application)))))))



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
	 (elisp-packages (cons* emacs-modus-themes
                                emacs-guix
                                emacs-expand-region
                                additional-elisp-packages))
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
            (define-key global-map (kbd "C-=") 'er/expand-region)

            (defun rde/display-load-time ()
              (interactive)
              (message "rde emacs loaded in %s, C-h r i for search in emacs manual by topic. C-h C-a for welcome screen." (emacs-init-time)))

            ;; (setq inhibit-splash-screen t)
            (defun display-startup-echo-area-message ()
              (rde/display-load-time))
	    ,#~""
	    ;; (define-key global-map (kbd "M-/") 'hippie-expand)

            (defun rde-compilation-colorizer ()
              "Prevent color escape sequences to popup in compilation buffer."
              (ansi-color-apply-on-region compilation-filter-start (point)))
            (add-hook 'compilation-filter-hook 'rde-compilation-colorizer)

            ;; <https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/>
            ;; Actually there is M-m for back-to-indentation
            (defun smarter-move-beginning-of-line (arg)
              "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
              (interactive "^p")
              (setq arg (or arg 1))

              ;; Move lines first
              (when (/= arg 1)
                (let ((line-move-visual nil))
                  (forward-line (- arg 1))))

              (let ((orig-point (point)))
                (move-beginning-of-line 1)
                (when (= orig-point (point))
                  (back-to-indentation))))
            ,#~"
(define-key global-map
  [remap move-beginning-of-line]
  'smarter-move-beginning-of-line)\n"

	    (column-number-mode 1)
	    (save-place-mode 1)
	    (show-paren-mode 1)
            (global-guix-prettify-mode)

	    (setq-default indent-tabs-mode nil)
	    (setq save-interprogram-paste-before-kill t)
	    (setq mouse-yank-at-point t)
	    (setq require-final-newline t)
            (add-hook 'prog-mode-hook
                      (lambda () (setq show-trailing-whitespace t)))

            ;; MAYBE: Move to dired
            (dolist (mode-hook '(prog-mode-hook dired-mode-hook
                                 compilation-mode-hook))
                    (add-hook mode-hook (lambda () (setq truncate-lines t))))
	    (define-key global-map (kbd "s-r") 'recompile)

	    (load-theme 'modus-operandi t)))
	 (early-init-el
	  `(,(slurp-file-gexp (local-file "./emacs/early-init.el"))))
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
	  'emacs-update-environment-variables-on-sway-start
	  home-sway-service-type
	  `((exec_always
	     ,(program-file
	       "update-emacs-env-variables"
	       #~(system*
		  #$emacs-client "--eval"
		  (string-append
                   "(mapcar (lambda (lst) (apply #'setenv lst)) '"
                   (let* ((port   ((@@ (ice-9 popen) open-input-pipe)
		                   (string-append "env")))
	                  (result ((@@ (ice-9 rdelim) read-delimited) "" port))
	                  (vars (map (lambda (x)
                                       (let ((si (string-index x #\=)))
                                         (list (string-take x si)
                                               (string-drop x (+ 1 si)))))
			             ((@@ (srfi srfi-1) remove)
			              string-null? (string-split
                                                    result #\newline)))))
	             (close-port port)
	             (format #f "~s" vars))
                   ")"))))
            (for_window "[title=\".* - Emacs Client\"]"
                        floating enable,
                        resize set 80 ppt 80 ppt)))))))

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

(define* (feature-emacs-input-methods
	  #:key
	  (default-input-method "cyrillic-dvorak")
	  (input-method-packages (list emacs-cyrillic-dvorak-im)))
  "Configure input-method for GNU Emacs.  Allows to use other layouts
with emacs, whithout losing ability to use keybindings.  Supported
both Emacsy toggle-input-method (C-\\) and system layout switching by
utilizing reverse-im package."

  (define emacs-f-name 'input-method)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'mule

         ,@(map (lambda (x) `(require ',(strip-emacs-name x)))
                input-method-packages)

	 (setq default-input-method ,default-input-method)
         (define-key global-map (kbd "s-SPC") 'toggle-input-method)

	 (require 'reverse-im))

	(with-eval-after-load
	 'reverse-im
	 (setq reverse-im-input-methods ,default-input-method)
	 (reverse-im-mode 1)))
      #:elisp-packages (cons emacs-reverse-im input-method-packages))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-message
	  #:key
	  (smtp-server #f)
	  (smtp-port 587))
  "Configure email sending capabilities provided by @file{message.el}."
  (ensure-pred string? smtp-server)
  (ensure-pred integer? smtp-port)

  (define emacs-f-name 'message)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'message
	 (setq send-mail-function 'smtpmail-send-it)
	 (setq smtpmail-smtp-server ,smtp-server)
	 (setq smtpmail-smtp-service ,smtp-port)
         (setq message-kill-buffer-on-exit t)
         (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

	 (setq message-auto-save-directory
	       (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
		       "/emacs/mail-drafts")))))

     (emacs-xdg-service
      emacs-f-name
      "Emacs (Client) [mailto:]"
      #~(system*
         #$emacs-cmd "--eval"
	 (string-append
          "\
(progn
 (set-frame-name \"Reply to Email - Emacs Client\")
 (browse-url-mail \"" (cadr (command-line)) "\"))"))
      #:default-for '(x-scheme-handler/mailto))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

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

  (define emacs-f-name 'erc)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (list
     (elisp-configuration-service
      emacs-f-name
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

	 (setq erc-track-visibility nil))))
     (emacs-xdg-service
      emacs-f-name
      "Emacs (Client) [IRC]"
      #~(system* #$emacs-cmd "--eval" "(erc-tls)"))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-telega)
  "Configure telega.el for GNU Emacs"
  (define emacs-f-name 'telega)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         "--eval"
         (string-append
	  "(progn
(set-frame-name \"Telega - Emacs Client\")
(if (and (boundp 'telega--status) (equal telega--status \"Ready\"))
 (telega-browse-url \"" (car (cdr (command-line))) "\")"
 "
 (telega)
 (add-hook 'telega-ready-hook
  (lambda ()
   (telega-browse-url \"" (car (cdr (command-line))) "\")))"
   "))")))

    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'telega

         (define-key telega-chat-mode-map (kbd "s-B") 'telega-chat-with)
	 (define-key telega-root-mode-map (kbd "s-B") 'telega-chat-with)

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
      #:elisp-packages (list emacs-telega))

     (emacs-xdg-service emacs-f-name "Emacs (Client) [tg:]" xdg-gexp
                        #:default-for '(x-scheme-handler/tg))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-eshell)
  "Configure Eshell, the Emacs shell."
  (define emacs-f-name 'eshell)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((define-key global-map (kbd "s-e") 'eshell)
        (with-eval-after-load
         'eshell
         (add-hook
          'eshell-hist-mode-hook
          (lambda ()
            (when (fboundp 'consult-history)
              (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history))))

         ;;; <https://www.emacswiki.org/emacs/AnsiColor#h5o-2>
         (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

         (add-hook
          'eshell-mode-hook
          (lambda ()
            (setenv "PAGER" "")

            (eshell/alias "e" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            (eshell/alias "d" "dired $1")
            (with-eval-after-load
             'magit (eshell/alias "gd" "magit-diff-unstaged"))

            (define-key eshell-mode-map (kbd "s-e") 'switch-to-prev-buffer))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org)
  "Configure org-mode for GNU Emacs."
  (define emacs-f-name 'org)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
         'org
	 (setq org-adapt-indentation nil)
	 (setq org-edit-src-content-indentation 0)
	 (setq org-startup-indented t)

         (setq org-outline-path-complete-in-steps nil)
         (setq org-refile-use-outline-path t)
         (setq org-refile-targets `((nil . (:maxlevel . 3))))

         (setq org-ellipsis "⤵")
         (set-face-attribute 'org-ellipsis nil
		             :inherit '(font-lock-comment-face default)
		             :weight 'normal)
         (setq org-hide-emphasis-markers t)

         (with-eval-after-load 'notmuch (require 'ol-notmuch))))
      #:elisp-packages (list emacs-org emacs-org-contrib))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-git)
  "Configure git-related utilities for GNU Emacs, including magit,
git-link, git-timemachine."
  (define emacs-f-name 'git)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      #:elisp-packages (list emacs-magit emacs-git-link emacs-git-timemachine))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-which-key)
  "Configure which-key."
  (define emacs-f-name 'which-key)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      '((require 'which-key)
        (which-key-mode 1)
        (define-key global-map (kbd "C-h C-k") 'which-key-show-top-level))
      #:elisp-packages (list emacs-which-key))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;; TODO: Move font record to apropriate module
(use-modules (rde features fontutils))

;; TODO: Can be useful to have different presets for different
;; environments.  For easier and faster switching.
(define* (feature-emacs-faces)
  "Configure faces for GNU Emacs."

  (define emacs-f-name 'faces)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'fonts config)
    (define font-monospace (get-value 'font-monospace config))
    (define font-sans      (get-value 'font-sans config))

    (list
     (elisp-configuration-service
      emacs-f-name
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

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: Remove corfu, rename to minibuffer
(define* (feature-emacs-completion)
  "Configure completion system for GNU Emacs."
  (define emacs-f-name 'completion)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'minibuffer

         (setq completion-styles '(orderless))
	 (setq completion-category-overrides
	       '((file (styles . (partial-completion)))))
         (setq completion-in-region-function 'consult-completion-in-region)
	 (setq enable-recursive-minibuffers t)

         ;; Move modeline to the top
         (setq-default header-line-format mode-line-format)
         (setq-default mode-line-format nil)

         (setq resize-mini-windows nil)

         (add-hook 'after-init-hook 'mini-frame-mode)
         (with-eval-after-load
          'mini-frame
          (custom-set-faces
           '(child-frame-border
             ;; TODO: inherit ,(face-attribute 'default :foreground)
             ((t (:background "#000000")))))
          (put 'child-frame-border 'saved-face nil)

          (custom-set-variables
           '(mini-frame-show-parameters
             '((top . 0.2)
               (width . 0.8)
               (left . 0.5)
               (child-frame-border-width . 1)))
           '(mini-frame-detach-on-hide nil)
           '(mini-frame-color-shift-step 0)
           '(mini-frame-advice-functions '(read-from-minibuffer
                                           read-string save-some-buffers
                                           yes-or-no-p))
           '(mini-frame-ignore-commands '()))))

	(custom-set-variables
         '(savehist-file (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
		                 "/emacs/history")))
	(add-hook 'after-init-hook 'savehist-mode)

        (define-key global-map (kbd "s-.") 'embark-act)
	(with-eval-after-load
	 'embark
         ;;; TODO: Enable it when user-rde-unexperienced?
         ;; (setq embark-prompter 'embark-completing-read-prompter)
         (require 'embark-consult))

        (autoload 'consult-customize "consult" "" nil 'macro)

        (progn
         (define-key global-map (kbd "C-S-s") 'consult-line)
         (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
	 (define-key global-map (kbd "M-y") 'consult-yank-pop)
         (define-key global-map (kbd "s-b") 'consult-buffer)
         (define-key minibuffer-local-map (kbd "s-b") 'exit-minibuffer)

         ;; MAYBE: Move to feature-emacs-buffers/windows
         (define-key global-map (kbd "s-B") 'switch-to-buffer)
         (define-key global-map (kbd "s-w") 'kill-current-buffer)
	 (define-key global-map (kbd "s-o") 'other-window))

        (with-eval-after-load
	 'consult
         (consult-customize consult-line :inherit-input-method t))

        (add-hook 'after-init-hook 'marginalia-mode)
        (add-hook 'after-init-hook 'vertico-mode)
	(with-eval-after-load
         'vertico
         (custom-set-variables '(vertico-cycle t))))
      #:elisp-packages
      (map
       ;; For inherit-input-method for consult-line
       (options->transformation
        '((with-commit . "emacs-consult=5cef041e001548874dd1aa98e2764e0518d9a92d")
          (with-commit . "emacs-embark=acbe1cba548832d295449da348719f69b9685c6f")))
       (list emacs-orderless emacs-marginalia
	     emacs-vertico emacs-mini-frame
             emacs-consult emacs-embark)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-project)
  "Configure project.el for GNU Emacs."

  (define emacs-f-name 'project)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      ;; TODO: https://github.com/muffinmad/emacs-ibuffer-project
      ;; MAYBE: Rework the binding approach
      `((add-hook 'after-init-hook
                  (lambda ()
                    (define-key global-map (kbd "s-p") project-prefix-map)))
        (with-eval-after-load
	 'project
	 (with-eval-after-load
	  'consult
	  (setq consult-project-root-function
		(lambda ()
		  (when-let (project (project-current))
			    (car (project-roots project)))))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: rewrite to states
(define* (feature-emacs-org-roam
	  #:key
	  (org-roam-directory #f))
  "Configure org-roam for GNU Emacs."
  (define (not-boolean? x) (not (boolean? x)))
  (ensure-pred not-boolean? org-roam-directory)

  (define emacs-f-name 'org-roam)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((add-hook 'after-init-hook 'org-roam-mode)
	(with-eval-after-load
	 'org-roam
	 (define-key org-roam-mode-map (kbd "C-c n n") 'org-roam-jump-to-index)
	 (define-key org-roam-mode-map (kbd "C-c n f") 'org-roam-find-file)
	 (define-key org-mode-map      (kbd "C-c n i") 'org-roam-insert)
	 (setq org-roam-directory ,org-roam-directory)))
      #:elisp-packages (list emacs-org-roam))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;; TODO: feature-emacs-monocole
;; TODO: feature-emacs-reasonable-keybindings
;; TODO: Fix env vars for emacs daemon
;; https://github.com/purcell/exec-path-from-shell
;; TODO: feature-emacs-epub https://depp.brause.cc/nov.el/
;; TODO: feature-series-tracker https://github.com/MaximeWack/seriesTracker
