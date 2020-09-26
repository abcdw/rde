(use-package icomplete
  :demand
  :config
  (use-package orderless
    :config
    (setq completion-styles '(orderless)))
  (icomplete-mode 1)
  (setq icomplete-prospects-height 1)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)))

(use-package icomplete-vertical
  :demand t
  :after icomplete
  :config
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle)))
