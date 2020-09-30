(use-package company
  :hook
  (after-init-hook . global-company-mode)
  :bind (:map company-active-map
	      ("C-e" . company-complete-selection)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)))
