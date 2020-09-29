(use-package company
  :hook
  (after-init-hook . global-company-mode))

;; (use-package company-org-roam
;;   :after org-roam company ; saves 0.3s startup time
;;   :config
;;   (push 'company-org-roam company-backends))
