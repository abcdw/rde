(use-package dired
  :defer t
  :config
  (setq dired-listing-switches
        "-aFhl --group-directories-first --time-style=long-iso")
  (setq dired-hide-details-hide-symlink-targets nil)
  ;; Suggest other dired buffer path instead of current. Works
  ;; similiar to midnight commander, when two dired buffers available
  (setq dired-dwim-target t)
  (defun rde/dired-xdg-open ()
    (interactive)
    (let* ((files (dired-get-marked-files)))
      (dolist (file files)
	(browse-url-xdg-open file))))

  :hook (dired-mode-hook . dired-hide-details-mode)
  :bind (:map dired-mode-map
	      ("C-M-o" . rde/dired-xdg-open)
	      ("M-n" . dired-next-subdir)
	      ("M-p" . dired-prev-subdir)))

(use-package dired-x
  :bind ("s-d" . dired-jump))
