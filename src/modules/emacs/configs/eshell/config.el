(use-package eshell
  :config
  (global-set-key (kbd "s-e") 'eshell)

  ;; Eshell bug prevents this from working
  ;; https://github.com/noctuid/general.el/issues/32
  ;; :bind (:map eshell-mode-map
  ;; 	      ("s-e" . 'switch-to-prev-buffer))
  :hook
  (eshell-mode-hook .
   (lambda ()
     (define-key eshell-mode-map (kbd "s-e") 'switch-to-prev-buffer))))
