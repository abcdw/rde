;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 24 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'leuven)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/work/org-files/"
      org-roam-directory (expand-file-name "notes/" org-directory))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;

(defun kill-other-window-buffer ()
  "Kill buffer in other window"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1))

(defun kill-other-window-and-buffer ()
  "Kill buffer in other window"
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (delete-window))

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun switch-to-next-buffer-other-window ()
  (interactive)
  (switch-to-next-buffer (next-window)))

(defun switch-to-prev-buffer-other-window ()
  (interactive)
  (switch-to-prev-buffer (next-window)))

(defun maximize-other-window ()
  (interactive)
  (other-window 1)
  (delete-other-windows))

(global-set-key (kbd "s-w") #'kill-current-buffer)
(global-set-key (kbd "s-o") #'other-window)
(global-set-key (kbd "s-n") #'switch-to-next-buffer)
(global-set-key (kbd "s-p") #'switch-to-prev-buffer)
(global-set-key (kbd "s-q") #'kill-buffer-and-window)
(global-set-key (kbd "s-m") #'delete-other-windows)
;; (global-set-key (kbd "s-TAB") #'alternate-buffer)
(global-set-key (kbd "C-s-n") 'switch-to-next-buffer-other-window)
(global-set-key (kbd "C-s-p") 'switch-to-prev-buffer-other-window)
(global-set-key (kbd "C-s-w") #'kill-other-window-and-buffer)
;; (global-set-key (kbd "C-s-m") #'maximize-other-window)
(global-set-key (kbd "s-a") #'projectile-toggle-between-implementation-and-test)
(global-set-key (kbd "s-f") #'counsel-projectile)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-?") 'help-command)

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
