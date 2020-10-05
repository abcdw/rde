(use-package org
  :defer t
  :config
  (setq org-adapt-indentation nil)
  (setq org-hide-emphasis-markers t)

  (setq org-edit-src-content-indentation 0)
  ;; Without it tables becomes missaligned
  (set-face-attribute 'org-link nil :inherit '(fixed-pitch link))
  ;; To make width of checked checkbox the same as unchecked
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-table nil :inherit '(fixed-pitch))

  :hook
  (org-mode-hook . org-indent-mode)
  (org-mode-hook . variable-pitch-mode))

(use-package org-tempo :after org)
