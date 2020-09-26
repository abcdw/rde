(use-package keycast
  :config
  (use-package moody)
  (setq keycast-separator-width 1)
  (setq keycast-window-predicate 'moody-window-active-p)
  (setq keycast-remove-tail-elements nil)
  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))
  :bind ("C-c t k" . keycast-mode))
