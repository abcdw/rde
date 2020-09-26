(use-package emacs
  :config
  (column-number-mode 1)

  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))
  (setq visible-bell nil
	ring-bell-function 'flash-mode-line)

  ;; Inspired by Prot
  (define-minor-mode rde/toggle-mode-line
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if rde/toggle-mode-line
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format))
    (force-mode-line-update))
  :bind ("C-c t m" . rde/toggle-mode-line))
