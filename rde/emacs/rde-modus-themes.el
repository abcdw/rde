;;;###autoload
(eval-when-compile
  (require 'rde-use-package))

;;;###autoload
(use-package modus-operandi-theme
  ;; Read more here: https://protesilaos.com/modus-themes/
  :config
  (setq modus-operandi-theme-prompts 'intense)
  (setq modus-operandi-theme-completions 'opinionated)
  (setq modus-operandi-theme-org-blocks 'greyscale)
  (setq modus-operandi-theme-scale-headings t)

  (load-theme 'modus-operandi t)
  ;; TODO: Move to faces config?
  ;; Without it tables becomes missaligned
  (set-face-attribute 'button nil :inherit '(fixed-pitch)))

(provide 'rde-modus-themes)
