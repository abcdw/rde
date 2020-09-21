;;; use-package configuration
(eval-and-compile
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  ;; without ivy `describe-variable' won't suggest after-init-hook for
  ;; after-init
  (setq use-package-hook-name-suffix nil))

(require 'use-package)

;;; May improve startup time, but doesn't work with compute-statistics
;; (eval-when-compile
;;   (require 'use-package))
;; (require 'bind-key)

;; TODO: add use-package info to info dirs
;; https://jwiegley.github.io/use-package/installation/


(defun run-command-in-eshell (cmd)
  (eshell)
  (eshell-kill-input)
  (end-of-buffer)
  (insert cmd)
  (eshell-send-input))

(defun rde/build ()
  (interactive)
  (run-command-in-eshell "nixos-rebuild build --flake /home/abcdw/work/rde"))

(defun rde/switch ()
  (interactive)
  (run-command-in-eshell "sudo nixos-rebuild switch --flake /home/abcdw/work/rde"))

(defun rde/switch-and-restart-emacs ()
  (interactive)
  (run-command-in-eshell "sudo nixos-rebuild switch --flake /home/abcdw/work/rde && restart-emacs"))

;;; Configs to reference
;;; https://github.com/bbatsov/emacs.d/blob/master/init.el

;; It works


;;; https://www.freedesktop.org/software/fontconfig/fontconfig-user.html

;; ;; It doesn't


(global-set-key (kbd "C-c r r") 'rde/switch-and-restart-emacs)
(global-set-key (kbd "C-c f c") '(lambda () (interactive) (find-file "~/.config/emacs/init.el")))
(global-set-key (kbd "C-c f d") '(lambda () (interactive) (find-file "~/work/rde/src/modules/emacs/default.nix")))
(global-set-key (kbd "C-c f e") '(lambda () (interactive) (find-file "~/work/rde/src/modules/emacs/configs/rde-defaults/config.el")))
(global-set-key (kbd "C-c f r") '(lambda () (interactive) (dired (cons "recentf" recentf-list))))
(global-set-key (kbd "C-c f h") '(lambda () (interactive) (find-file "~/work/rde/src/home.nix")))
(global-set-key (kbd "C-c f i") '(lambda () (interactive) (find-file "~/work/rde/src/hosts/ixy/configuration.nix")))
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-n") 'switch-to-next-buffer)
(global-set-key (kbd "s-p") 'switch-to-prev-buffer)



(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :bind (:map nix-mode-map
	      (("C-c l p" . nix-format-buffer))))

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package modus-operandi-theme
  ;;; Read more here: https://protesilaos.com/modus-themes/
  :config
  (load-theme 'modus-operandi t))

;; (require 'org-tempo)
(use-package org :defer t)
;; (setq org-hide-emphasis-markers t)

;; (use-package company-org-roam
;;   :after org-roam company ; saves 0.3s startup time
;;   :config
;;   (push 'company-org-roam company-backends))

(use-package company
  :hook
  (after-init-hook . global-company-mode))

(use-package olivetti
  :config
  (setq olivetti-minimum-body-width 80)
  :bind ("C-c t o" . olivetti-mode))

(use-package restart-emacs
  :commands restart-emacs
  :bind ("C-c r e" . restart-emacs))

(use-package keycast
  ;; :config
  ;; (setq keycast-window-predicate 'keycast-bottom-left-window-p)
  :bind ("C-c t k" . keycast-mode))
