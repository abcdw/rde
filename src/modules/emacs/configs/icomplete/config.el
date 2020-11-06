(use-package icomplete
  :demand
  :config
  (use-package orderless
    :config
    (setq completion-styles '(orderless)))
  (icomplete-mode 1)

  (defun rde/icomplete-eol-or-complete (arg)
    (interactive "^p")
    (let ((original-point (point)))
	  (move-end-of-line arg)
	  (when (= original-point (point))
	    (icomplete-force-complete))))

  ;; (setq icomplete-prospects-height 1)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
	      ("C-e" . rde/icomplete-eol-or-complete)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)))

(use-package icomplete-vertical
  :demand
  :after icomplete
  :config
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle)))
