;;; Read more here: https://protesilaos.com/modus-themes/
(use-package modus-operandi-theme
  :config
  (setq modus-operandi-theme-prompts 'intense)
  (setq modus-operandi-theme-completions 'opinionated)
  (setq modus-operandi-theme-org-blocks 'rainbow)
  (setq modus-operandi-theme-scale-headings t)
  (setq modus-operandi-theme-headings
      '((t . highlight)))
  (load-theme 'modus-operandi t))
