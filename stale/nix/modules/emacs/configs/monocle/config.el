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
