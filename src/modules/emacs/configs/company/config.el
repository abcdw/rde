(use-package company
  :delight
  :config
  (set-face-attribute 'company-tooltip nil :inherit 'fixed-pitch)
  :hook
  (after-init-hook . global-company-mode)
  :bind (:map company-active-map
	      ("C-j" . company-complete-selection)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)))
