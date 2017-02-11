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
  '((lispy :location (recipe :fetcher github :repo "abo-abo/lispy"))
    (evil-lispy :location (recipe :fetcher github
                                  :repo "sp3ctum/evil-lispy"
                                  :branch "master"))
    ;; parinfer
    (sayid :location (recipe :fetcher github :repo "bpiel/sayid"))))


(defun clojure-additions/init-sayid ()
  (use-package sayid :defer t))

(defun clojure-additions/init-lispy ()
  (use-package lispy
    :defer t
    :config (spacemacs|diminish lispy-mode "" "")))

(defun clojure-additions/init-evil-lispy ()
  (use-package evil-lispy
    :defer t
    :init
    (progn
     ;; (require 'evil-lispy)
     (evil-lispy-layer-configure-colorization)
     (add-hook 'clojure-mode-hook #'evil-lispy-mode)
     (add-hook 'common-lisp-mode-hook #'evil-lispy-mode)
     (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
     (add-hook 'scheme-mode-hook #'evil-lispy-mode)
     (add-hook 'lisp-mode-hook #'evil-lispy-mode))
    :commands (evil-lispy-mode)
    :config
    (progn
      (spacemacs|diminish evil-lispy-mode " Ⓛ" " L")
      (add-to-list 'lispy-compat 'cider)
      (setq lispy-eval-display-style 'overlay)
      (define-key evil-lispy-state-map (kbd "C-t") 'evil-escape)
      (define-key evil-lispy-state-map (kbd "C-g") 'evil-escape)
      (define-key evil-lispy-state-map (kbd "SPC") 'spacemacs-cmds)
      (define-key evil-normal-state-map (kbd "C-.") 'evil-lispy/enter-state-right)
      (define-key evil-insert-state-map (kbd "C-.") 'evil-lispy/enter-state-right))))

(defun evil-lispy-layer-configure-colorization ()
  ;; this will be displayed in the modeline
  (let ((mode-color "deep sky blue"))

    (defface spacemacs-lispy-face
      `((t :inherit 'mode-line
           :background ,mode-color))
      "lispy state face."
      :group 'spacemacs)

    ;; (setq evil-lispy-state-cursor '(mode-color box))

    (setq evil-lispy-state-cursor
          (list (when dotspacemacs-colorize-cursor-according-to-state mode-color)
                'box))))

;; (defun clojure-additions/init-parinfer ()
;;   ;; https://github.com/DogLooksGood/parinfer-mode
;;   (use-package
;;     parinfer
;;     :defer t
;;     :init
;;     (progn
;;       (setq parinfer-extensions
;;             '(defaults       ; should be included.
;;                pretty-parens  ; different paren styles for different modes.
;;                evil           ; If you use Evil.
;;                lispy))          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;;       ;; paredit ; Introduce some paredit commands.
;;       ;; smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;       ;; smart-yank
;;                                         ; Yank behavior depend on mode.
;;       (setq parinfer-lighters '("P:Ind" . "P:Par"))
;;       (add-hook 'clojure-mode-hook #'parinfer-mode)
;;       (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;       (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;       (add-hook 'scheme-mode-hook #'parinfer-mode)
;;       (add-hook 'lisp-mode-hook #'parinfer-mode)
;;       )

;;     :config
;;     (progn
;;       (define-key evil-insert-state-map (kbd "C-.") 'parinfer-toggle-mode)
;;       (define-key evil-normal-state-map (kbd "C-.") 'parinfer-toggle-mode)
;;       (define-key evil-insert-state-map (kbd "C-;") 'lispy-comment)
;;       (define-key evil-normal-state-map (kbd "C-;") 'lispy-comment))))



;;; packages.el ends here
