;;; packages.el --- clojure-additions layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andrew Tropin <andrewtropin@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst clojure-additions-packages
  '((lispy (recipe :fetcher github :repo "abo-abo/lispy"))
    ;; evil-lispy
    parinfer
    (sayid :location (recipe :fetcher github :repo "bpiel/sayid"))))


(defun clojure-additions/init-sayid ()
  (use-package sayid :defer t))

(defun clojure-additions/init-lispy ()
  (use-package lispy :defer t))

(defun clojure-additions/init-parinfer ()
  ;; https://github.com/DogLooksGood/parinfer-mode
  (use-package
    parinfer
    :defer t
    :init
    (progn
      (setq parinfer-extensions
            '(defaults       ; should be included.
               pretty-parens  ; different paren styles for different modes.
               evil           ; If you use Evil.
               lispy))          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
               ;; paredit ; Introduce some paredit commands.
      ;; smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
      ;; smart-yank
                                        ; Yank behavior depend on mode.
      (setq parinfer-lighters '("P:Ind" . "P:Par"))
      (add-hook 'clojure-mode-hook #'parinfer-mode)
      (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
      (add-hook 'common-lisp-mode-hook #'parinfer-mode)
      (add-hook 'scheme-mode-hook #'parinfer-mode)
      (add-hook 'lisp-mode-hook #'parinfer-mode))

    :config
    (progn
      (define-key evil-insert-state-map (kbd "C-.") 'parinfer-toggle-mode)
      (define-key evil-normal-state-map (kbd "C-.") 'parinfer-toggle-mode)
      (define-key evil-insert-state-map (kbd "C-;") 'lispy-comment)
      (define-key evil-normal-state-map (kbd "C-;") 'lispy-comment))))



;;; packages.el ends here
