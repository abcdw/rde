(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package icomplete
  :demand
  :config
  (icomplete-mode 1)
  (setq icomplete-prospects-height 1))
