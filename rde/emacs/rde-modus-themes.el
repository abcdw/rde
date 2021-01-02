(eval-when-compile
  (require 'rde-use-package))

;;; Read more here: https://protesilaos.com/modus-themes/
(use-package modus-operandi-theme
  :config
  (setq modus-operandi-theme-prompts 'intense)
  (setq modus-operandi-theme-completions 'opinionated)
  (setq modus-operandi-theme-org-blocks 'greyscale)
  (setq modus-operandi-theme-scale-headings t)

  (load-theme 'modus-operandi t)
  ;; Without it tables becomes missaligned
  (set-face-attribute 'button nil :inherit '(fixed-pitch)))

(provide 'rde-modus-themes)
