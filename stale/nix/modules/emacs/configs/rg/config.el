(use-package rg
  :demand
  :bind (:map rg-mode-map
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))
