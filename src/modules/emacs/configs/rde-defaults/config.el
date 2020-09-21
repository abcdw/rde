(use-package files
  :config
  ;; Most of the files are under version control anyway.
  (setq make-backup-files nil)
  ;; Doesn't make a lot of sense for single user setup, also
  ;; modification date will notify in case someone edit file.
  (setq create-lockfiles nil)

  ;; Add newline at the end of the file on save, the reason:
  ;; https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline
  (setq require-final-newline t))

(use-package custom
  :config
  ;; According to XDG files with data should be placed in
  ;; ~/.local/share, btw it's not necessary to use customize at all
  (setq custom-file rde/custom-file))

(use-package recentf
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-save-file (expand-file-name "recentf" rde/data-dir))
  :hook (after-init-hook . recentf-mode))

(use-package saveplace
  ;; Saves position in file
  :config
  (setq save-place-file (expand-file-name "places" rde/data-dir))
  (save-place-mode 1))

(use-package dired
  :defer t
  :config
  (setq dired-listing-switches
        "-aFhl --group-directories-first --time-style=long-iso"))


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
  (global-hl-line-mode 1)

  (blink-cursor-mode -1)
  (column-number-mode 1)

  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))
  (setq visible-bell nil
	ring-bell-function 'flash-mode-line))

(use-package emacs
  :config
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
  ;; Disable soft word wrapping
  (set-default 'truncate-lines t))

(use-package isearch
  :config
  (setq isearch-lazy-count t))

(use-package eshell
  :config
  (global-set-key (kbd "s-e") 'eshell)

  ;; Eshell bug prevents this from working
  ;; https://github.com/noctuid/general.el/issues/32
  ;; :bind (:map eshell-mode-map
  ;; 	      ("s-e" . 'switch-to-prev-buffer))

  :hook
  (eshell-mode-hook .
   (lambda ()
     (define-key eshell-mode-map (kbd "s-e") 'switch-to-prev-buffer))))
