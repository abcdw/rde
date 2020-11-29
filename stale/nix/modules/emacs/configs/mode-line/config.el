(use-package emacs
  :config
  (column-number-mode 1)

  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))
  (setq visible-bell nil
	ring-bell-function 'flash-mode-line)
  (setq mode-line-defining-kbd-macro
        (propertize " Macro" 'face 'mode-line-emphasis))
  ;; Almost default value of the variable, but has a smaller number of
  ;; spaces between vc-mode and modes
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (vc-mode vc-mode)
		  ;; One space instead of two
                  " "
                  mode-line-modes
                  " "
                  mode-line-misc-info
                  mode-line-end-spaces))

  ;; Inspired by Prot
  (define-minor-mode rde/toggle-mode-line
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if rde/toggle-mode-line
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      ;; (force-mode-line-update)
      ;; For some reason modeline not re-renderend after update
      (redraw-frame)))
  :bind ("C-c t m" . rde/toggle-mode-line))
