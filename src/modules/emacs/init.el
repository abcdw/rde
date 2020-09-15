(eval-and-compile
  ;; (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t)

  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  ;; without ivy `describe-variable' won't suggest after-init-hook for
  ;; after-init
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path
	     (expand-file-name "rde/" user-emacs-directory))

(eval-when-compile
  (require 'rde-variables))

(require 'rde-features)

(setq custom-file rde/custom-file)

(set-face-attribute 'default nil :font rde/font)

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


(defun rde/display-load-time ()
  (interactive)
  (message "rde emacs loaded in %s, C-h C-a for welcome screen." (emacs-init-time)))
;; (add-hook 'after-init-hook 'rde/display-load-time)

(setq inhibit-splash-screen t)
;; (setq inhibit-startup-echo-area-message "abcdw")
(defun display-startup-echo-area-message ()
  (rde/display-load-time))

;; (set-face-attribute 'default (selected-frame) :family "Iosevka" :weight 'semi-light)
;;; https://www.freedesktop.org/software/fontconfig/fontconfig-user.html

;; ;; It doesn't


(global-set-key (kbd "C-c r r") 'rde/switch-and-restart-emacs)
(global-set-key (kbd "C-c f c") '(lambda () (interactive) (find-file "~/.config/emacs/init.el")))
(global-set-key (kbd "C-c f e") '(lambda () (interactive) (find-file "~/work/rde/src/modules/emacs/init.el")))
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

(use-package dired
  :defer t
  :config
  (setq dired-listing-switches
        "-aFhl --group-directories-first --time-style=long-iso"))
  
(use-package org :defer t)

(use-package org-roam
  :hook
  (after-init-hook . org-roam-mode)
  :custom
  (org-roam-directory "~/work/org-files/notes")
  :bind (
	 :map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph-show))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))

(use-package company-org-roam
  :after org-roam company ; saves 0.3s startup time
  :config
  (push 'company-org-roam company-backends))

(use-package company
  :hook
  (after-init-hook . global-company-mode))

(use-package ivy
  :demand t
  :config
  (ivy-mode 1)
  :bind ("C-c C-r" . ivy-resume))

(use-package olivetti
  :config
  (setq olivetti-minimum-body-width 80)
  :bind ("C-c t o" . olivetti-mode))

(use-package restart-emacs
  :commands restart-emacs
  :bind ("C-c r e" . restart-emacs))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix nil)
  (setq uniquify-after-kill-buffer-p t))

(use-package keycast
  ;; :config
  ;; (setq keycast-window-predicate 'keycast-bottom-left-window-p)
  :bind ("C-c t k" . keycast-mode))

;; custom file is set for one session
;; (setq custom-file (expand-file-name
;;                    (format "custom-%d-%d.el" (emacs-pid) (random))
;;                    temporary-file-directory))

;; (setq org-hide-emphasis-markers t)

(defgroup rde-core nil
  "Configuration variables for rde Emacs."
  :group 'convenience
  :prefix "rde-"
  :link '(url-link :tag "GitHub" "https://github.com/abcdw/rde"))

(defcustom rde-test-variable t
  "Non-nil values enable rde test feature."
  :type 'boolean
  :group 'rde-core)


