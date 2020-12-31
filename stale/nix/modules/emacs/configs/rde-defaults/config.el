(use-package files
  :config
  ;; Most of the files are under version control anyway.
  (setq make-backup-files nil)
  ;; Doesn't make a lot of sense for single user setup, also
  ;; modification date will notify in case someone edit file.
  (setq create-lockfiles nil)

  ;; Add newline at the end of the file on save, the reason:
  ;; https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline
  (setq require-final-newline t)

  ;; Highlight trailing whitespace and delete on save
  (setq whitespace-style '(face trailing spaces space-mark))
  :hook ((before-save-hook . delete-trailing-whitespace)
	 ((prog-mode-hook text-mode-hook) . (lambda () (setq show-trailing-whitespace t)))))

(use-package custom
  :config
  ;; According to XDG files with data should be placed in
  ;; ~/.local/share, btw it's not necessary to use customize at all
  (setq custom-file rde/custom-file)
  ;; FIXME: probably (load custom-file) is required here
  )

(use-package recentf
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-save-file (expand-file-name "recentf" rde/data-dir))
  (run-with-idle-timer 127 t 'recentf-save-list)

  (defun rde/recentf-dired ()
    (interactive)
    (dired (cons "recentf" recentf-list)))

  ;; For inspiration: https://github.com/bbatsov/crux
  (defun rde/recentf-find-file ()
    "Find a recent file using `completing-read'."
    (interactive)
    ;; Space as initial input required to trigger icomplete,
    ;; but it's not necessary with icomplete-vertical
    (let ((file (completing-read "Choose recent file: "
				 (mapcar #'abbreviate-file-name recentf-list)
				 nil t
				 ;; " "
				 )))
      (when file
	(find-file file))))
  :bind ("C-c f r" . rde/recentf-find-file)
  :hook (after-init-hook . recentf-mode))

(use-package saveplace
  ;; Saves position in file
  :config
  (setq save-place-file (expand-file-name "places" rde/data-dir))
  (save-place-mode 1))

(use-package autorevert
  :diminish
  :config
  (setq auto-revert-verbose t)
  :hook (after-init-hook . global-auto-revert-mode))

(use-package bind-key
  :bind ("C-h y" . describe-personal-keybindings))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix nil)
  (setq uniquify-after-kill-buffer-p t))

(use-package mouse
  :config
  ;; Insert where edit, not where clicked.
  (setq mouse-yank-at-point t))

(use-package frame
  :config
  ;; (setq hl-line-sticky-flag nil)
  ;; (global-hl-line-mode 1)

  ;; box shape works bad with variable-pitch font because cursor has
  ;; the same width as letter under it and letters has variable width.
  (setq-default cursor-type '(bar . 3))
  (setq-default cursor-in-non-selected-windows 'hollow)
  ;; Blinking sometimes distracting
  (blink-cursor-mode -1))

(use-package elisp-mode
  :delight (emacs-lisp-mode "Elisp" :major))

;; (use-package eldoc
;;   :delight (eldoc-mode " εδ"))

(use-package emacs
  :config
  ;; Disabled by default, but pretty useful in some cases
  (put 'narrow-to-region 'disabled nil)

  (defun rde/display-load-time ()
    (interactive)
    (message "rde emacs loaded in %s, C-h r i for search in emacs manual by topic. C-h C-a for welcome screen." (emacs-init-time)))

  (setq inhibit-splash-screen t)
  (defun display-startup-echo-area-message ()
    (rde/display-load-time)))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package simple
  :config
  ;; Save system clipboard in yank-ring
  (setq save-interprogram-paste-before-kill t)
  :bind ("C-c t t" . toggle-truncate-lines)
  ;; Disable soft word wrapping
  :hook
  ((prog-mode-hook dired-mode-hook) . (lambda () (setq truncate-lines t))))

(use-package isearch
  :config
  (setq isearch-lazy-count t))

(use-package minibuffer
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))


(use-package emacs
  :config
  (defun rde/run-command-in-eshell (cmd)
    (eshell)
    (eshell-kill-input)
    (end-of-buffer)
    (insert cmd)
    (eshell-send-input))

  (defun rde/build ()
    (interactive)
    (rde/run-command-in-eshell
     (format "nixos-rebuild build --flake %s" rde/rde-dir)))

  (defun rde/switch ()
    (interactive)
    (rde/run-command-in-eshell
     (format "sudo nixos-rebuild switch --flake %s" rde/rde-dir)))

  (defun rde/switch-and-restart-emacs ()
    (interactive)
    (rde/run-command-in-eshell
     (format "sudo nixos-rebuild switch --flake %s && restart-emacs" rde/rde-dir)))

  (global-set-key (kbd "C-c r r") 'rde/switch-and-restart-emacs)
  (global-set-key (kbd "C-c f c")
		  '(lambda () (interactive)
		     (find-file (format "%s/init.el" rde/config-dir))))
  (global-set-key (kbd "C-c f d")
		  '(lambda () (interactive)
		     (find-file (format "%s/src/modules/emacs/default.nix" rde/rde-dir))))
  (global-set-key (kbd "C-c f e")
		  '(lambda () (interactive)
		     (find-file (format "%s/src/modules/emacs/configs/rde-defaults/config.el" rde/rde-dir))))

  (global-set-key (kbd "C-c f h")
		  '(lambda () (interactive)
		     (find-file (format "%s/src/home.nix" rde/rde-dir))))
  (global-set-key (kbd "C-c f i")
		  '(lambda () (interactive)
		     (find-file (format "%s/src/hosts/ixy/configuration.nix" rde/rde-dir))))

  (defun rde/join-line (number-of-lines)
    "number-of-lines passed as universal argument. For positive
  value joins number-of-lines lines downwards. For negative joins
  -number-of-lines upwards."
    (interactive "p")
    (dotimes (i (abs number-of-lines))
      (if (> number-of-lines 0)
	  (join-line 1)
	(join-line))))

  (global-set-key (kbd "s-j") 'rde/join-line)
  (global-set-key (kbd "s-o") 'other-window)
  (global-set-key (kbd "s-n") 'switch-to-next-buffer)
  (global-set-key (kbd "s-p") 'switch-to-prev-buffer)
  ;; TODO: Add universal argument support to kill window and buffer.
  (global-set-key (kbd "s-w") 'kill-current-buffer))

(use-package restart-emacs
  :commands restart-emacs
  :bind ("C-c r e" . restart-emacs))

(use-package pulse
  :config
  (setq pulse-delay 0.06)

  ;; Inspired by Prot
  (defface rde/pulse-face
    '((t
       :inherit
       modus-theme-intense-yellow
       ;; pulse-highlight-face
       :extend t))
    "Ad-hoc face for `rde/pulse-line'.
This is done because it is not possible to highlight empty lines
without the `:extend' property.")

  (defun rde/pulse-line (&optional face)
    "Temporarily highlight the current line."
    (interactive)
    (let ((start (if (and (eobp) (bolp))
                     (line-beginning-position 0)
                   (line-beginning-position)))
          (end (line-beginning-position 2))
          (face (or face 'rde/pulse-face)))
      (pulse-momentary-highlight-region start end face)))

  ;; https://www.reddit.com/r/emacs/comments/8qyq53/can_i_make_emacs_highlight_the_area_that_i_just/
  (advice-add 'insert-for-yank :around
	    (lambda (oldfun &rest args)
	      (let ((beg (point)))
		(prog1 (apply oldfun args)
		  (pulse-momentary-highlight-region beg (point) 'rde/pulse-face))))
            '((name . rde/pulse-after-yank)))
  :hook (window-state-change-hook . rde/pulse-line))


(use-package info
  :config
  (add-to-list 'Info-directory-list ;; "/var/guix/profiles/per-user/abcdw/current-guix/share/info")
	       "/home/abcdw/.guix-profile/share/info"))


(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(1
                               ((shift) . 5)
                               ((control))))
  (mouse-wheel-progressive-speed nil))

(use-package pixel-scroll
  :config
  (pixel-scroll-mode))

(use-package emacs
  :config
  ;; (setq auto-window-vscroll nil)
  (setq scroll-margin 7)
  (setq scroll-conservatively 10000))
