(define-module (rde features emacs)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)

  #:export (feature-emacs
            feature-emacs-appearance
            feature-emacs-faces
	    feature-emacs-completion
	    feature-emacs-vertico
	    feature-emacs-mct
	    feature-emacs-input-methods
	    feature-emacs-project
	    feature-emacs-perspective
	    feature-emacs-git
	    feature-emacs-dired
            feature-emacs-eshell
	    feature-emacs-monocle
	    feature-emacs-org
	    feature-emacs-org-roam
	    feature-emacs-org-agenda
	    feature-emacs-erc
            feature-emacs-elpher
	    feature-emacs-telega
	    feature-emacs-pdf-tools
            feature-emacs-which-key
            feature-emacs-keycast

            elisp-configuration-service
            emacs-xdg-service))

(define autoload-each-sexp-in-configure-package? #f)

(define* (rde-emacs-configuration-package
          name
          #:optional (elisp-expressions '())
          #:key
          summary authors url keywords commentary
          (elisp-packages '())
          (autoloads? autoload-each-sexp-in-configure-package?))
  "Returns a package, which configures emacs.  Can be used as a
dependency for other packages."
    (let* ((configure-package
	  (elisp-configuration-package
	   (string-append "configure-" (symbol->string name))
           elisp-expressions
           #:elisp-packages elisp-packages
           #:autoloads? autoloads?
           #:summary summary
           #:commentary commentary
           #:keywords keywords
           #:url (or url "https://trop.in/rde")
           #:authors (or authors '("Andrew Tropin <andrew@trop.in>")))))
      configure-package))

(define* (elisp-configuration-service
          name
          #:optional (elisp-expressions '())
          #:key
          summary authors url keywords commentary
          (early-init '())
          (elisp-packages '())
          (autoloads? autoload-each-sexp-in-configure-package?)
          (require-in-init? (not autoload-each-sexp-in-configure-package?)))
  (let* ((pkg-name (symbol-append 'configure- name))
         (configure-package
	  (rde-emacs-configuration-package
           name elisp-expressions
           #:summary summary
           #:commentary commentary
           #:keywords keywords
           #:url url
           #:authors authors
           #:elisp-packages elisp-packages
           #:autoloads? autoloads?)))
    (simple-service
     (symbol-append 'emacs- name '-configurations)
     home-emacs-service-type
     (home-emacs-extension
      (init-el (if require-in-init? `((require ',pkg-name)) '()))
      (early-init-el early-init)
      ;; It's necessary to explicitly add elisp-packages here, because
      ;; we want to overwrite builtin emacs packages.  Propagated
      ;; inputs have lowest priority on collisions, that's why we have
      ;; to list those package here in addition to propagated-inputs.
      (elisp-packages (append elisp-packages (list configure-package)))))))

;; MAYBE: make handler to be actions instead of desktop entries?
(define* (emacs-xdg-service
          name xdg-name gexp
          #:key
          (default-for '())
          (exec-argument "%u"))
  (define file-name (string-append "emacs-" (symbol->string name)))
  (define file-file (file-append (program-file file-name gexp)
                                 (string-append " " exec-argument)))
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
	  (emacs emacs-next-pgtk-latest)
	  (emacs-server-mode? #t)
	  (additional-elisp-packages '())
          (extra-init-el '())
          (extra-early-init-el '()))
  "Setup and configure GNU Emacs."
  (ensure-pred boolean? emacs-server-mode?)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)
  (ensure-pred package? emacs)

  (define emacs-client (file-append emacs "/bin/emacsclient"))
  (define emacs-client-create-frame
    (program-file "emacs-client-create-frame"
		  #~(apply system*
			   #$(file-append emacs "/bin/emacsclient")
			   "--create-frame"
			   (cdr (command-line)))))
  (define emacs-client-no-wait
    (program-file "emacs-client-no-wait"
		  #~(apply system*
			   #$(file-append emacs "/bin/emacsclient")
			   "--no-wait"
			   (cdr (command-line)))))
  (define emacs-editor
    (program-file "emacs-editor"
		  #~(apply system*
			   #$(file-append emacs "/bin/emacs")
			   "--no-splash"
			   (cdr (command-line)))))

  (define emacs-configure-rde-keymaps
    (rde-emacs-configuration-package
     'rde-keymaps
     `((defvar rde-app-map nil "Prefix keymap for applications.")
       (define-prefix-command 'rde-app-map nil "rde applications")
       (defvar rde-toggle-map nil "\
Prefix keymap for binding various minor modes for toggling functionalitty.")
       (define-prefix-command 'rde-toggle-map nil "rde toggles"))
     #:summary "Keymaps inteded for reuse among configure-* packages"))

  (define (emacs-home-services config)
    "Returns home services related to GNU Emacs."
    (require-value 'full-name config)
    (require-value 'email config)
    (let* ((full-name (get-value 'full-name config))
	   (email     (get-value 'email config)))
      (list
       (emacs-xdg-service 'emacs-q "Emacs (No init: -q)"
                          #~(system* "emacs" "-q"))
       (emacs-xdg-service 'emacs-Q "Emacs (No init, no site-lisp: -Q)"
                          #~(system* "emacs" "-Q"))

       (elisp-configuration-service
        'rde-emacs
        `((require 'configure-rde-keymaps)

          (setq user-full-name ,full-name)
	  (setq user-mail-address ,email)

          ,#~""
	  (setq custom-file
		(concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			"/emacs/custom.el"))
	  (load custom-file t)

          (setq backup-directory-alist
                `(,(cons "." (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
		                     "/emacs/backup"))))

          ,#~""
          (column-number-mode 1)
	  (save-place-mode 1)
          ;; MAYBE: Make it buffer local?
          (show-paren-mode 1)
	  (setq-default indent-tabs-mode nil)
	  (setq save-interprogram-paste-before-kill t)
	  (setq mouse-yank-at-point t)
	  (setq require-final-newline t)
          (add-hook 'prog-mode-hook
                    (lambda () (setq show-trailing-whitespace t)))
          ,#~""
          (define-key global-map (kbd "C-=") 'er/expand-region)

          ,#~""
          (defun rde-display-load-time ()
            (interactive)
            (message "\
rde emacs loaded in %s, C-h r i for search in emacs manual by topic. \
C-h C-a to open About Emacs buffer."
                     (emacs-init-time)))

          (defun display-startup-echo-area-message ()
            (rde-display-load-time))

          ,#~""
          ;; TODO: Move it to feature-isearch
          (setq search-whitespace-regexp ".*?")

          ,#~""
          ;; TODO: Move it to feature-compile
          (defun rde-compilation-colorizer ()
            "Prevent color escape sequences to popup in compilation buffer."
            (ansi-color-apply-on-region (point-min) (point-max)))
          (add-hook 'compilation-filter-hook 'rde-compilation-colorizer)

          (dolist (mode-hook '(prog-mode-hook compilation-mode-hook))
                  (add-hook mode-hook (lambda () (setq truncate-lines t))))
          (setq compilation-scroll-output 'first-error)
	  (define-key global-map (kbd "s-r") 'recompile)

          ,#~""
          ;; TODO: Move it to feature-emacs-tramp
          (eval-when-compile (require 'tramp))
          (with-eval-after-load
           'tramp
           (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

          ,#~""
          ;; TODO: Extend info-lookup-alist with Guix Manual nodes to
          ;; make `C-h S' find guix services and other items.
          (eval-when-compile (require 'guix))
          (define-key rde-toggle-map (kbd "p") 'guix-prettify-mode)
          (define-key rde-toggle-map (kbd "P") 'global-guix-prettify-mode)
          (add-hook 'after-init-hook 'global-guix-prettify-mode)

          ,#~""
          (eval-when-compile
           (require 'time))
          (setq world-clock-list
                '(("America/Los_Angeles" "Los Angeles")
                  ("America/Boise" "Boise")
                  ("America/New_York" "New York")
                  ("Europe/London" "London")
                  ("Europe/Paris" "Paris")
                  ("Europe/Helsinki" "Helsinki")
                  ("Europe/Moscow" "Moscow")
                  ("Asia/Tokyo" "Tokyo")))
          (define-key rde-app-map (kbd "w") 'world-clock)

          ;; TODO: Move to feature-sane-bindings
          (let ((map goto-map))
            (define-key map "L" 'find-library)
            (define-key map "F" 'find-function)
            (define-key map "K" 'find-function-on-key)
            (define-key map "V" 'find-variable))

          (defun kill-region-dwim (&optional count)
            "The function kills region if mark is active, otherwise kills word.
Prefix argument can be used to kill a few words."
            (interactive "p")
            (if (use-region-p)
                (kill-region (region-beginning) (region-end) 'region)
                (backward-kill-word count)))

          ;; (define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
          (define-key global-map (kbd "M-K") 'kill-whole-line)
          (define-key global-map (kbd "M-c") 'capitalize-dwim)
          (define-key global-map (kbd "M-l") 'downcase-dwim)
          (define-key global-map (kbd "M-u") 'upcase-dwim)
          (define-key global-map (kbd "C-w") 'kill-region-dwim)

          (define-key mode-specific-map (kbd "a") 'rde-app-map)
          (define-key mode-specific-map (kbd "t") 'rde-toggle-map))
        #:summary "General settings"
        #:elisp-packages (list (get-value 'emacs-configure-rde-keymaps config)
                               emacs-expand-region emacs-guix)
        #:keywords '(convenience))

       (service
	home-emacs-service-type
	(home-emacs-configuration
	 (package emacs)
	 (elisp-packages additional-elisp-packages)
	 (server-mode? emacs-server-mode?)
	 (xdg-flavor? #t)
	 (init-el extra-init-el)
	 (early-init-el
	  `(,(slurp-file-gexp (local-file "./emacs/early-init.el"))
            ,@extra-early-init-el))
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
	  `((exec_always "sleep 2s && " ;; Need to wait until emacs daemon loaded.
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
   (values (make-feature-values
            emacs emacs-editor emacs-client
            emacs-configure-rde-keymaps
            emacs-client-create-frame
            emacs-client-no-wait
            emacs-server-mode?))
   (home-services-getter emacs-home-services)))

(define* (feature-emacs-appearance
          #:key
          (margin 8))
  "Make Emacs looks modern and minimalistic."
  (ensure-pred integer? margin)

  (define emacs-f-name 'appearance)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((set-default 'cursor-type  '(bar . 1))
        (blink-cursor-mode 0)
        (setq-default cursor-in-non-selected-windows nil)

        (custom-set-variables '(window-divider-default-right-width ,margin))

        (require 'modus-themes)
        (setq modus-themes-diffs 'fg-only-deuteranopia)
        (setq modus-themes-success-deuteranopia t)
	(load-theme 'modus-operandi t)

        ;; (setq header-line-format (delete 'mode-line-modes header-line-format))
        (setq mode-line-modes
              (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
                (list (propertize "%[" 'help-echo recursive-edit-help-echo)
	              "("
	              `(:propertize ("" mode-name)
			            help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
			            mouse-face mode-line-highlight
			            local-map ,mode-line-major-mode-keymap)
	              '("" mode-line-process)
	              ")"
	              (propertize "%]" 'help-echo recursive-edit-help-echo)
	              " ")))
        (custom-set-faces
         `(git-gutter-fr:modified
           ((t (:foreground "blue" :background "white"))))
         `(git-gutter-fr:added
           ((t (:foreground "green" :background "white"))))
         `(git-gutter-fr:deleted
           ((t (:foreground "red" :background "white")))))

        (defun rde--set-divider-faces ()
          (custom-set-faces
              `(window-divider
                ((t (:foreground ,(face-background 'default)))))
              `(window-divider-first-pixel
                ((t (:foreground ,(face-background 'default)))))
              `(window-divider-last-pixel
                ((t (:foreground ,(face-background 'default)))))))

        (if (daemonp)
            (add-hook 'after-make-frame-functions
                      (lambda (frame)
                        (with-selected-frame frame (rde--set-divider-faces))))
            (rde--set-divider-faces))
        (window-divider-mode))
      #:early-init
      `(,#~"\n;; Prevent the glimpse of un-styled Emacs by disabling \
these UI elements early."
        (push '(menu-bar-lines . 0) default-frame-alist)
        (push '(tool-bar-lines . 0) default-frame-alist)
        (push '(vertical-scroll-bars) default-frame-alist)
        (push '(internal-border-width . ,margin) default-frame-alist)

        ,#~""
        (setq-default fringes-outside-margins t)
        (setq-default left-margin-width 1)
        (setq-default right-margin-width 1)

        ,#~""
        (setq use-dialog-box nil)
        (setq use-file-dialog nil)
        ,@(if (get-value 'emacs-advanced-user? config)
              '((setq inhibit-startup-screen t))
              '())

        ,#~"\n;; Move modeline to the top"
        (setq-default header-line-format mode-line-format)
        (setq-default mode-line-format nil))
      #:elisp-packages (list emacs-modus-themes)
      #:keywords '(appearance mode-line faces accessibility)
      #:summary "\
Sets theme, fonts, faces and provides different visual tweaks"
      #:commentary "\
The goal is to provide non-distractive and safe visual design.

Modus operandi is light, high-contrast, calm, colorblind-friendly.
The light colorschemes are better for productivity according to
various researchs, more eye-friendly and works better with other apps
and media like PDFs, web pages, etc, which are also light by default.
Later here will be a link to rde manual with more in-depth explanation
with references to researches.

Modeline is simplified and moved to the top of the window.

Almost all visual elements are disabled.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-margin . ,margin)))
   (home-services-getter get-home-services)))


(define (strip-emacs-name p)
  (let ((name (package-name p)))
    (string->symbol
     (if (string-prefix? "emacs-" name)
         (string-drop name (string-length "emacs-"))
         name))))

(define* (feature-emacs-input-methods
	  #:key
          (enable-reverse-im #f)
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
         (setq-default mode-line-mule-info nil)
         ;; Feels a little hacky, but mule-related hooks are
         ;; inconsistent and cursor not changed back in some cases.
         (add-hook 'post-command-hook
                   '(lambda ()
                      (set-cursor-color
                       (if current-input-method "DarkOrange1" "black"))))

         ,@(map (lambda (x) `(require ',(strip-emacs-name x)))
                input-method-packages)

	 (setq default-input-method ,default-input-method)
         (define-key global-map (kbd "s-SPC") 'toggle-input-method))

	,@(if enable-reverse-im
              `((add-hook 'after-init-hook 'reverse-im-mode)
                (with-eval-after-load
	         'reverse-im
	         (setq reverse-im-input-methods ,default-input-method)))
            '()))
      #:elisp-packages `(,@(if enable-reverse-im (list emacs-reverse-im) '())
                         ,@input-method-packages))))

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
      `((eval-when-compile
         (require 'erc-join)
         (require 'erc-fill)
         (require 'erc-track))
        (with-eval-after-load
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
      `((eval-when-compile
         (require 'telega)
         (require 'company))

        (require 'configure-rde-keymaps)
        (define-key rde-app-map (kbd "t") telega-prefix-map)

        (with-eval-after-load
	 'telega

         (define-key telega-chat-mode-map (kbd "s-B") 'telega-chat-with)
	 (define-key telega-root-mode-map (kbd "s-B") 'telega-chat-with)
         (setq telega-emoji-company-backend 'telega-company-emoji)
         ,@(if (get-value 'mpv config)
               `((setq telega-video-player-command
                       ,(file-append (get-value 'mpv config) "/bin/mpv")))
               '())
         (defun rde-telega-chat-mode ()
           (set (make-local-variable 'company-backends)
                (append (list telega-emoji-company-backend
                              'telega-company-username
                              'telega-company-hashtag)
                        (when (telega-chat-bot-p telega-chatbuf--chat)
                          '(telega-company-botcmd))))
           (company-mode 1))
         (add-hook 'telega-chat-mode-hook 'rde-telega-chat-mode)

	 (setq telega-completing-read-function completing-read-function)))
      #:elisp-packages (list emacs-telega
                             (get-value 'emacs-configure-rde-keymaps config)))

     (emacs-xdg-service emacs-f-name "Emacs (Client) [tg:]" xdg-gexp
                        #:default-for '(x-scheme-handler/tg))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-pdf-tools)
  "Configure pdf-tools, to work with pdfs inside Emacs."
  (define emacs-f-name 'pdf-tools)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         (car (cdr (command-line)))))
    (list
     (elisp-configuration-service
      emacs-f-name
      `((custom-set-variables '(pdf-view-use-scaling t))
        (autoload 'pdf-view-mode "pdf-view" "")
        (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
        (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
        (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
        (with-eval-after-load
         'saveplace
         (require 'saveplace-pdf-view)))
      #:elisp-packages (list emacs-pdf-tools emacs-saveplace-pdf-view))
     (emacs-xdg-service emacs-f-name "Emacs (Client) [PDF]" xdg-gexp
                        #:default-for '(application/pdf))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-monocle)
  "Configure olivetti and helper functions for focused editing/reading."
  (define emacs-f-name 'monocle)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile
         (require 'olivetti)
         (require 'hide-mode-line))

        (custom-set-variables '(olivetti-body-width 85)
                              '(olivetti-margin-width 0))

        (with-eval-after-load
         'hide-mode-line
         (custom-set-variables '(hide-mode-line-excluded-modes '())))

        (defun rde--match-modes (modes)
          "Check if current mode is derived from one of the MODES."
          (seq-filter 'derived-mode-p modes))

        (defun rde--turn-on-olivetti-mode ()
          (unless (memq major-mode '(minibuffer-mode which-key-mode))
            (olivetti-mode 1)))

        (define-globalized-minor-mode global-olivetti-mode
          olivetti-mode rde--turn-on-olivetti-mode
          :require 'olivetti-mode
          :group 'olivetti)

        (defvar rde--monocle-previous-window-configuration nil
          "Window configuration for restoring on monocle exit.")

        (defun rde-toggle-monocle (arg)
          "Make window occupy whole frame if there are many windows. Restore
previous window layout otherwise.  With universal argument toggles
`global-olivetti-mode'."
          (interactive "P")

          (if arg
              (if (and global-olivetti-mode global-hide-mode-line-mode)
                  (progn
                   (global-hide-mode-line-mode -1)
                   (global-olivetti-mode -1))
                  (progn
                   (global-hide-mode-line-mode 1)
                   (global-olivetti-mode 1)))
              (if (one-window-p)
                  (if rde--monocle-previous-window-configuration
	              (let ((cur-buffer (current-buffer)))
                        (set-window-configuration
                         rde--monocle-previous-window-configuration)
	                (setq rde--monocle-previous-window-configuration nil)
                        (switch-to-buffer cur-buffer)))
                  (setq rde--monocle-previous-window-configuration
                        (current-window-configuration))
                  (delete-other-windows))))

        (require 'configure-rde-keymaps)
        (define-key rde-toggle-map (kbd "o") 'olivetti-mode)
        (define-key rde-toggle-map (kbd "O") 'global-olivetti-mode)
        (define-key rde-toggle-map (kbd "m") 'hide-mode-line-mode)
        (define-key rde-toggle-map (kbd "M") 'global-hide-mode-line-mode)
	(define-key global-map (kbd "s-f") 'rde-toggle-monocle))
      #:elisp-packages (list emacs-olivetti emacs-hide-header-line
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-dired)
  "Configure dired, the Emacs' directory browser and editor."
  (define emacs-f-name 'dired)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         "--eval"
         (string-append
	  "(dired \"" (car (cdr (command-line))) "\")")))
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile (require 'dired))
        (with-eval-after-load
         'dired
         (setq dired-dwim-target t)
         (setq dired-listing-switches "-l --time-style=long-iso -h -AG")
         (add-hook 'dired-mode-hook 'dired-hide-details-mode)
         (add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))
         (setq dired-hide-details-hide-symlink-targets nil))))
     (emacs-xdg-service emacs-f-name "Emacs (Client) [file:]" xdg-gexp
                        #:default-for '(inode/directory))))

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
      `((eval-when-compile
         (require 'eshell)
         (require 'em-alias)
         (require 'em-hist)
         (require 'project))
        (defun rde-project-eshell-or-eshell (&optional arg)
          "If there is a project open project-eshell"
          (interactive "P")
          (if (project-current)
              (project-eshell)
              (eshell arg)))

        (define-key global-map (kbd "s-e") 'rde-project-eshell-or-eshell)
        (with-eval-after-load
         'eshell
         (add-hook
          'eshell-hist-mode-hook
          (lambda ()
            (when (fboundp 'consult-history)
              (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history))))

         ;;; <https://www.emacswiki.org/emacs/AnsiColor#h5o-2>
         (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

         (defun switch-to-prev-buffer-or-eshell (arg)
           (interactive "P")
           (if arg
               (eshell arg)
               (switch-to-buffer (other-buffer (current-buffer) 1))))

         (add-hook
          'eshell-mode-hook
          (lambda ()
            (if envrc-global-mode
                (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
                (setenv "PAGER" ""))

            (eshell/alias "e" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            (eshell/alias "d" "dired $1")
            (with-eval-after-load
             'magit (eshell/alias "gd" "magit-diff-unstaged"))

            (define-key eshell-mode-map (kbd "s-e")
              'switch-to-prev-buffer-or-eshell))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org
          #:key
          (org-directory "~/org")
          (org-rename-buffer-to-title #t))
  "Configure org-mode for GNU Emacs."
  (define emacs-f-name 'org)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile
         (require 'org)
         (require 'org-refile))
        (with-eval-after-load
         'org
	 (setq org-adapt-indentation nil)
	 (setq org-edit-src-content-indentation 0)
	 (setq org-startup-indented t)

         (setq org-outline-path-complete-in-steps nil)
         (setq org-refile-use-outline-path 'path)
         (setq org-refile-targets `((nil . (:maxlevel . 3))))

         (setq org-ellipsis "⤵")
         (set-face-attribute 'org-ellipsis nil
		             :inherit '(font-lock-comment-face default)
		             :weight 'normal)
         (setq org-hide-emphasis-markers t)
         (setq org-log-into-drawer t)

         (setq org-directory ,org-directory)
         (setq org-default-notes-file (concat org-directory "/todo.org"))

         (define-key org-mode-map (kbd "C-c o n") 'org-num-mode)

         ;; <https://emacs.stackexchange.com/questions/54809/rename-org-buffers-to-orgs-title-instead-of-filename>
         (defun rde-buffer-name-to-title (&optional end)
           "Rename buffer to value of #+TITLE:.
If END is non-nil search for #+TITLE: at `point' and
delimit it to END.
Start an unlimited search at `point-min' otherwise."
           (interactive)
           (let ((case-fold-search t)
                 (beg (or (and end (point))
                          (point-min))))
             (save-excursion
              (when end
                (goto-char end)
                (setq end (line-end-position)))
              (goto-char beg)
              (when (re-search-forward
                     "^[[:space:]]*#\\+TITLE:[[:space:]]*\\(.*?\\)[[:space:]]*$"
                     end t)
                (rename-buffer (match-string 1)))))
           nil)

         (defun rde-buffer-name-to-title-config ()
           "Configure Org to rename buffer to value of #+TITLE:."
           (font-lock-add-keywords nil '(rde-buffer-name-to-title)))

         ,@(when org-rename-buffer-to-title
             '((add-hook 'org-mode-hook 'rde-buffer-name-to-title-config)))

         (with-eval-after-load 'notmuch (require 'ol-notmuch))))
      #:elisp-packages (list emacs-org emacs-org-contrib))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-agenda
          #:key
          (org-agenda-files 'nil))
  "Configure org-agenda for GNU Emacs."
  (define emacs-f-name 'org-agenda)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-org config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile
         (require 'org-agenda))
        (define-key global-map (kbd "C-x C-a") 'org-agenda)
        (with-eval-after-load
         'org-agenda
         ;; Impressive agenda examples
         ;; https://github.com/fniessen/emacs-leuven/blob/master/org-leuven-agenda-views.txt
         ;; Clean agenda view
         ;; https://gist.github.com/rougier/ddb84c16c28f7cd75e27e50d4c3c43da
         (setq org-agenda-custom-commands
               `((,(kbd "C-d") "Agenda for the day"
                  ((agenda
                    ""
                    ((org-agenda-span 1)
                     (org-deadline-warning-days 0)
                     (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
                     (org-agenda-block-separator nil)
                     (org-scheduled-past-days 0)
                     ;; We don't need the `org-agenda-date-today'
                     ;; highlight because that only has a practical
                     ;; utility in multi-day views.
                     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'todo '("NEXT")))
                     (org-agenda-format-date "%A %-e %B %Y")
                     (org-agenda-overriding-header "\nAgenda for the day\n")))
                   (todo
                    "NEXT"
                    ((org-agenda-block-separator nil)
                     (org-agenda-overriding-header "\nCurrent Tasks\n")))))
                 (,(kbd "C-o") "Overview"
                  ;; TODO: Add A priority to the top.
                  ((agenda
                    ""
                    ((org-agenda-time-grid nil)
                     (org-agenda-start-on-weekday nil)
                     (org-agenda-start-day "+1d")
                     (org-agenda-span 14)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-time-grid nil)
                     (org-deadline-warning-days 0)
                     (org-agenda-block-separator nil)
                     (org-agenda-entry-types '(:deadline))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
                     (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
                   (agenda
                    "*"
                    ((org-agenda-block-separator nil)
                     (org-agenda-span 14)
                     (org-agenda-overriding-header "\nAgenda\n")))
                   (alltodo
                    ""
                    ((org-agenda-block-separator nil)
                     (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled deadline))))
                     (org-agenda-overriding-header "\nBacklog\n")))))))



         (setq org-agenda-files ',org-agenda-files))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-elpher)
  "Configure elpher, the Emacs' gemini and gopher browser."
  (define emacs-f-name 'elpher)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         "--eval"
         (string-append
	  "(elpher-go \"" (car (cdr (command-line))) "\")")))
    (list
     (elisp-configuration-service
      emacs-f-name
      `((autoload 'elpher-go "elpher"))
      #:elisp-packages (list emacs-elpher))
     (emacs-xdg-service emacs-f-name "Emacs (Client) [gemini:]" xdg-gexp
                        #:default-for '(x-scheme-handler/gemini))))

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
      `((eval-when-compile
         (require 'git-gutter))
        (custom-set-variables '(git-link-use-commit t)
                              '(git-gutter:lighter " GG"))

        (require 'configure-rde-keymaps)
        (define-key rde-toggle-map (kbd "g") 'git-gutter-mode)
        (define-key rde-toggle-map (kbd "G") 'global-git-gutter-mode)
        (define-key global-map (kbd "s-g") 'git-gutter-transient)

        (with-eval-after-load
         'git-gutter
         (require 'git-gutter-fringe)

         (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
         (add-to-list 'git-gutter:update-commands 'other-window)

         (add-hook 'magit-post-stage-hook 'git-gutter:update-all-windows)
         (add-hook 'magit-post-unstage-hook 'git-gutter:update-all-windows)

         (defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
           (cl-letf (((symbol-function 'yes-or-no-p) 'y-or-n-p))
                    (apply orig-fun r)))

         (dolist (fn '(git-gutter:stage-hunk git-gutter:revert-hunk))
                 (advice-add fn :around 'yes-or-no-p->-y-or-n-p))

         (defadvice git-gutter:stage-hunk (around auto-confirm compile activate)
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t)))
                    ad-do-it))

         (dolist (fringe '(git-gutter-fr:added
                           git-gutter-fr:modified))
                 (define-fringe-bitmap fringe (vector 8) nil nil '(top repeat)))
         (define-fringe-bitmap 'git-gutter-fr:deleted
           (vector 8 12 14 15)
           nil nil 'bottom)))
      #:elisp-packages (list emacs-magit emacs-magit-todos
                             (get-value 'emacs-configure-rde-keymaps config)
                             emacs-git-link emacs-git-timemachine
                             emacs-git-gutter emacs-git-gutter-fringe
                             emacs-git-gutter-transient))))

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
    (define font-sans      (get-value 'font-sans      config))
    (define font-serif     (get-value 'font-serif     config))

    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'faces
	 (let* ((mono-fn ,(font-name font-monospace))
		(sans-fn ,(font-name font-sans))
		(serif-fn ,(font-name font-serif))
		(mono (font-spec
		       :name ,(font-name font-monospace)
                       ;; For some reason pgtk emacs has much smaller
                       ;; font than alacritty with the same size value
		       :size   ,(+ 3 (font-size font-monospace))
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
			 (variable-pitch ((t (:family ,serif-fn)))))))
	   (dolist (face faces)
		   (custom-set-faces face))

	   (dolist (face faces)
		   (put (car face) 'saved-face nil))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-completion
          #:key
          (mini-frame? #f)
          (emacs-consult emacs-consult)
          (emacs-embark emacs-embark))
  "Configure completion system for GNU Emacs."
  (define emacs-f-name 'completion)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     ;; Do we need it or better to set absolute path instead of rg in
     ;; `consult-ripgrep-args'
     (simple-service
      'emacs-completion-add-ripgrep-packages
      home-profile-service-type
      (list (@ (gnu packages rust-apps) ripgrep)))
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'minibuffer

         (setq completion-in-region-function 'consult-completion-in-region)

         (setq completion-styles '(orderless))
	 (setq completion-category-overrides
	       '((file (styles . (partial-completion)))
                 (project-file (styles . (partial-completion)))))
	 (setq enable-recursive-minibuffers t)

         (setq resize-mini-windows nil)

         ;; MAYBE: Make transient use child-frame:
         ;; https://github.com/magit/transient/issues/102
         ,@(if mini-frame?
             `((add-hook 'after-init-hook 'mini-frame-mode)
               (with-eval-after-load
                'mini-frame
                (custom-set-faces
                 '(child-frame-border
                   ;; TODO: inherit ,(face-attribute 'default :foreground)
                   ((t (:background "#000000")))))
                (put 'child-frame-border 'saved-face nil)

                (custom-set-variables
                 '(mini-frame-show-parameters
                   (lambda ()
                     `((top . 0.2)
                       (width . 0.8)
                       (left . 0.5)
                       (child-frame-border-width . 1))))
                 '(mini-frame-detach-on-hide nil)
                 '(mini-frame-color-shift-step 0)
                 '(mini-frame-advice-functions '(read-from-minibuffer
                                                 read-key-sequence
                                                 save-some-buffers yes-or-no-p))
                 '(mini-frame-ignore-commands '()))))
             '()))

	(custom-set-variables
         '(history-length 10000)
         '(savehist-file (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
		                 "/emacs/history")))
	(add-hook 'after-init-hook 'savehist-mode)

        (define-key global-map (kbd "s-.") 'embark-act)
        (define-key global-map (kbd "s->") 'embark-become)

        (autoload 'consult-customize "consult" "" nil 'macro)

        (progn
         (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
	 (define-key global-map (kbd "M-y") 'consult-yank-pop)
         (define-key global-map (kbd "s-B") 'consult-buffer)
         (define-key minibuffer-local-map (kbd "s-B") 'embark-become)
         ;; (define-key global-map (kbd "M-.") 'embark-dwim)

         (let ((map goto-map))
           (define-key map (kbd "g") 'consult-goto-line)
           (define-key map (kbd "M-g") 'consult-goto-line)
           (define-key map (kbd "o") 'consult-outline)
           (define-key map (kbd "i") 'consult-imenu)
           (define-key map (kbd "m") 'consult-mark)
           (define-key map (kbd "M") 'consult-global-mark)
           (define-key map (kbd "b") 'consult-bookmark))

         (let ((map search-map))
           (define-key map (kbd "f") 'consult-find)
           (define-key map (kbd "g") 'consult-ripgrep)
           (define-key map (kbd "e") 'consult-isearch)
           (define-key map (kbd "l") 'consult-line))
         ;; (define-key global-map (kbd "C-S-s") 'consult-line)

         (autoload 'consult-isearch-history "consult")
         (let ((map isearch-mode-map))
           (define-key map (kbd "M-e") 'consult-isearch-history)
           (define-key map (kbd "M-s e") 'consult-isearch-history)
           (define-key map (kbd "M-s l") 'consult-line))
         ;; (define-key isearch-mode-map (kbd "C-S-s") 'consult-line)

         ;; MAYBE: Move to feature-emacs-buffers/windows
         (define-key minibuffer-local-map (kbd "s-b") 'exit-minibuffer)
         (define-key global-map (kbd "s-b") 'switch-to-buffer)
         (define-key global-map (kbd "s-w") 'kill-current-buffer)
         (define-key global-map (kbd "s-W") 'kill-buffer-and-window)
	 (define-key global-map (kbd "s-o") 'other-window))

        (with-eval-after-load
	 'consult
         (require 'embark-consult)
         (consult-customize consult-history :category 'consult-history)
         (consult-customize consult-line :inherit-input-method t))

        (with-eval-after-load
         'marginalia
         (add-to-list 'marginalia-prompt-categories
                      '("\\<completion\\>" . consult-completion)))
        (add-hook 'after-init-hook 'marginalia-mode))
      #:elisp-packages
      (append
       (if mini-frame?
           (list emacs-mini-frame)
           '())
       (list emacs-orderless emacs-marginalia
             emacs-pcmpl-args
             emacs-consult emacs-embark)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-embark . ,emacs-embark)
             (emacs-consult . ,emacs-consult)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-vertico
          #:key
          (emacs-vertico emacs-vertico))
  "Configure vertico completion UI for GNU Emacs."
  (define emacs-f-name 'vertico)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
         'vertico
         (define-key global-map (kbd "s-s") 'vertico-repeat)
         (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
         (custom-set-variables '(vertico-cycle t)))

	(add-hook 'after-init-hook 'vertico-mode))
      #:elisp-packages (list emacs-vertico))))

  (feature
   (name f-name)
   (values (make-feature-values emacs-vertico))
   (home-services-getter get-home-services)))

(define* (feature-emacs-mct
          #:key
          (emacs-mct emacs-mct))
  "Configure mct completion UI for GNU Emacs."
  (define emacs-f-name 'mct)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile (require 'mct))
        (with-eval-after-load
         'mct
         (setq mct-live-update-delay 0)
         (setq mct-minimum-input 3)
         (defvar rde-completion-categories-other-window
           '(imenu)
           "Completion categories that has to be in other window than
current, otherwise preview functionallity will fail the party.")

         (defvar rde-completion-categories-not-show-candidates-on-setup
           '(command variable function)
           "Completion categories that has to be in other window than
current, otherwise preview functionallity will fail the party.")

         (defun rde-display-mct-buffer-pop-up-if-apropriate (buffer alist)
           "Call `display-buffer-pop-up-window' if the completion category
one of `rde-completion-categories-other-window', it will make
sure that we don't use same window for completions, which should
be in separate window."
           (if (memq (mct--completion-category)
                     rde-completion-categories-other-window)
               (display-buffer-pop-up-window buffer alist)
               nil))

         (defun rde-display-mct-buffer-apropriate-window (buffer alist)
           "Displays completion buffer in the same window, where completion
was initiated (most recent one), but in case, when compeltion
buffer should be displayed in other window use least recent one."
           (let* ((window (if (memq (mct--completion-category)
                                    rde-completion-categories-other-window)
                              (get-lru-window (selected-frame) nil nil)
                              (get-mru-window (selected-frame) nil nil))))
             (window--display-buffer buffer window 'reuse alist)))

         (setq mct-display-buffer-action
               (quote ((display-buffer-reuse-window
                        rde-display-mct-buffer-pop-up-if-apropriate
                        rde-display-mct-buffer-apropriate-window))))

         (defun rde-mct-show-completions ()
           "Instantly shows completion candidates for categories listed in
`rde-completion-categories-show-candidates-on-setup'."
           (unless (memq (mct--completion-category)
                         rde-completion-categories-not-show-candidates-on-setup)
             (setq-local mct-minimum-input 0)
             (mct--live-completions)))

         (add-hook 'minibuffer-setup-hook 'rde-mct-show-completions)
         ,@(if (get-value 'emacs-consult config)
               `((autoload 'consult-preview-at-point-mode "consult")
                 (setq rde-completion-categories-other-window
                       (append
                        '(consult-location
                          consult-grep consult-yank
                          consult-history consult-completion)
                        rde-completion-categories-other-window))
                 (add-hook 'completion-list-mode-hook
                           'consult-preview-at-point-mode))
               '()))
	(add-hook 'after-init-hook 'mct-mode))
      #:elisp-packages (list emacs-mct
                             (get-value 'emacs-consult config emacs-consult)))))

  (feature
   (name f-name)
   (values (make-feature-values emacs-mct))
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
      `((eval-when-compile
         (require 'project)
         (require 'consult))
        (add-hook 'after-init-hook
                  (lambda ()
                    (define-key global-map (kbd "s-p") project-prefix-map)))
        (with-eval-after-load
	 'project
         (add-to-list 'project-switch-commands '(project-compile "Compile") t)
	 (with-eval-after-load
	  'consult
	  (setq consult-project-root-function
		(lambda ()
		  (when-let (project (project-current))
			    (car (project-roots project))))))))
      #:elisp-packages (list (get-value 'emacs-consult config emacs-consult)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-perspective)
  "Configure perspective.el to group/isolate buffers per frames.  Make
emacsclient feels more like a separate emacs instance."

  (define emacs-f-name 'perspective)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((add-hook 'after-init-hook 'persp-mode)
        (custom-set-variables
         '(persp-show-modestring nil)
         '(persp-modestring-dividers '(" [" "]" "|"))))
      #:elisp-packages (list emacs-perspective))))

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
      `((eval-when-compile
         (let ((org-roam-v2-ack t))
           (require 'org-roam)))
        (setq org-roam-v2-ack t)
        (setq org-roam-completion-everywhere t
              org-roam-directory ,org-roam-directory)

        (autoload 'org-roam-db-autosync-enable "org-roam")
        (with-eval-after-load 'org-roam (org-roam-db-autosync-enable))

	(define-key global-map (kbd "C-c n n") 'org-roam-buffer-toggle)
	(define-key global-map (kbd "C-c n f") 'org-roam-node-find)
	(define-key global-map (kbd "C-c n i") 'org-roam-node-insert))
      #:elisp-packages (list emacs-org-roam))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-keycast)
  "Show keybindings and related functions as you type."

  (define emacs-f-name 'keycast)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((eval-when-compile (require 'keycast))
        (with-eval-after-load
         'keycast
         (require 'moody)
         (setq keycast-window-predicate 'moody-window-active-p)
         (setq keycast-separator-width 1)
         (add-to-list 'global-mode-string mode-line-keycast))

        (autoload 'keycast--update "keycast")
        ;; <https://github.com/tarsius/keycast/issues/7#issuecomment-627604064>
        (define-minor-mode rde-keycast-mode
          "Show current command and its key binding in the mode line."
          :global t
          (if rde-keycast-mode
              (add-hook 'pre-command-hook 'keycast--update t)
              (progn
               (setq keycast--this-command nil)
               (setq keycast--this-command-keys nil)
               (setq keycast--command-repetitions 0)
               (remove-hook 'pre-command-hook 'keycast--update))))

        (require 'configure-rde-keymaps)
        (define-key rde-toggle-map (kbd "k") 'rde-keycast-mode)
        (define-key rde-toggle-map (kbd "K") 'rde-keycast-mode))
      #:elisp-packages (list emacs-moody emacs-keycast
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: feature-emacs-reasonable-keybindings
;; TODO: Fix env vars for emacs daemon
;; https://github.com/purcell/exec-path-from-shell
;; TODO: feature-emacs-epub https://depp.brause.cc/nov.el/
;; TODO: feature-series-tracker https://github.com/MaximeWack/seriesTracker
