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
  (setq custom-file rde/custom-file))

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
    ;; Space as initial input required to trigger icomplete
    (let ((file (completing-read "Choose recent file: "
				 (mapcar #'abbreviate-file-name recentf-list)
				 nil t " ")))
      (when file
	(find-file file))))
  :bind ("C-c f r" . rde/recentf-find-file)
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
        "-aFhl --group-directories-first --time-style=long-iso")
  (setq dired-hide-details-hide-symlink-targets nil)
  ;; Suggest other dired buffer path instead of current. Works
  ;; similiar to midnight commander, when two dired buffers available
  (setq dired-dwim-target t)

  :hook (dired-mode-hook . dired-hide-details-mode)
  )

(use-package dired-x
  :bind ("s-d" . dired-jump))

(use-package project
  :config
  (define-key ctl-x-map "p" project-prefix-map))

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
  ;; Disable soft word wrapping
  :hook
  ((prog-mode-hook dired-mode-hook) . (lambda () (setq truncate-lines t))))

(use-package isearch
  :config
  (setq isearch-lazy-count t))

(use-package minibuffer
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

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



(use-package emacs
  ;; Inspired by Prot's monocle-mode
  ;; https://protesilaos.com/dotemacs/#h:12591f89-eeea-4b12-93e8-9293504e5a12
  :config
  (defvar rde/previous--window-configuration nil
    "Window configuration for restoring on monocle exit.")

  (defun rde/toggle-monocle ()
    "Make window occupy whole frame if there are many windows. Restore
previous window layout otherwise."
    (interactive)
    (if (one-window-p)
        (when rde/previous--window-configuration
	  (let ((prev--buffer (current-buffer)))
            (set-window-configuration rde/previous--window-configuration)
	    (setq rde/previous--window-configuration nil)
	    (switch-to-buffer prev--buffer)))
      (setq rde/previous--window-configuration (current-window-configuration))
      (delete-other-windows)))
  :bind ("s-m" . rde/toggle-monocle))

(defun run-command-in-eshell (cmd)
  (eshell)
  (eshell-kill-input)
  (end-of-buffer)
  (insert cmd)
  (eshell-send-input))

(defun rde/build ()
  (interactive)
  (run-command-in-eshell "nixos-rebuild build --flake /home/abcdw/work/rde"))

(defun rde/switch ()
  (interactive)
  (run-command-in-eshell "sudo nixos-rebuild switch --flake /home/abcdw/work/rde"))

(defun rde/switch-and-restart-emacs ()
  (interactive)
  (run-command-in-eshell "sudo nixos-rebuild switch --flake /home/abcdw/work/rde && restart-emacs"))

;;; Configs to reference
;;; https://github.com/bbatsov/emacs.d/blob/master/init.el

;; It works


;;; https://www.freedesktop.org/software/fontconfig/fontconfig-user.html

;; ;; It doesn't


(global-set-key (kbd "C-c r r") 'rde/switch-and-restart-emacs)
(global-set-key (kbd "C-c f c") '(lambda () (interactive) (find-file "~/.config/emacs/init.el")))
(global-set-key (kbd "C-c f d") '(lambda () (interactive) (find-file "~/work/rde/src/modules/emacs/default.nix")))
(global-set-key (kbd "C-c f e") '(lambda () (interactive) (find-file "~/work/rde/src/modules/emacs/configs/rde-defaults/config.el")))

(global-set-key (kbd "C-c f h") '(lambda () (interactive) (find-file "~/work/rde/src/home.nix")))
(global-set-key (kbd "C-c f i") '(lambda () (interactive) (find-file "~/work/rde/src/hosts/ixy/configuration.nix")))

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


(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :bind (:map nix-mode-map
	      (("C-c l p" . nix-format-buffer))))

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package modus-operandi-theme
  ;;; Read more here: https://protesilaos.com/modus-themes/
  :config
  (load-theme 'modus-operandi t))


(use-package org :defer t)
(use-package org-tempo :after org)
;; (setq org-hide-emphasis-markers t)

;; (use-package company-org-roam
;;   :after org-roam company ; saves 0.3s startup time
;;   :config
;;   (push 'company-org-roam company-backends))

(use-package company
  :hook
  (after-init-hook . global-company-mode))

(use-package olivetti
  :config
  (setq olivetti-minimum-body-width 80)
  :bind ("C-c t o" . olivetti-mode))

(use-package restart-emacs
  :commands restart-emacs
  :bind ("C-c r e" . restart-emacs))

(use-package keycast
  ;; :config
  ;; (setq keycast-window-predicate 'keycast-bottom-left-window-p)
  :bind ("C-c t k" . keycast-mode))
