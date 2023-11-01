;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (contrib features emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)

  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)

  #:export (feature-emacs-evil
            feature-emacs-undo-fu-session))


(define* (feature-emacs-evil
          #:key
          (emacs-evil emacs-evil)
          (emacs-evil-collection emacs-evil-collection)
          (emacs-evil-commentary emacs-evil-commentary)
          (emacs-evil-surround emacs-evil-surround)
          (emacs-evil-org emacs-evil-org)
          (emacs-undo-fu emacs-undo-fu)
          (hide-state-message? #f))
  "Configure evil-mode for emacs. The feature is in contrib because the
recommended RDE experience is to rely on Emacs default keybindings."
  (ensure-pred file-like? emacs-evil)
  (ensure-pred file-like? emacs-evil-collection)
  (ensure-pred file-like? emacs-evil-commentary)
  (ensure-pred file-like? emacs-evil-surround)
  (ensure-pred file-like? emacs-evil-org)
  (ensure-pred file-like? emacs-undo-fu)
  (ensure-pred boolean? hide-state-message?)

  (define emacs-f-name 'evil)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun rde-evil-hook ()
          (dolist (mode '(custom-mode
                          eshell-mode
                          git-rebase-mode
                          term-mode))
                  (add-to-list 'evil-emacs-state-modes mode)))

        (eval-when-compile
         (require 'evil)
         (require 'evil-collection)
         (require 'evil-commentary)
         (require 'evil-surround))

        (setq evil-want-keybinding nil)

        (with-eval-after-load 'evil-autoloads
          (evil-mode 1))
        (with-eval-after-load 'evil-collection-autoloads
          (evil-collection-init))
        (with-eval-after-load 'evil-commentary-autoloads
          (evil-commentary-mode))
        (with-eval-after-load 'evil-surround-autoloads
          (global-evil-surround-mode 1))

        (setq evil-want-integration t)
        (setq evil-want-C-u-scroll t)
        (setq evil-want-C-i-jump nil)
        (setq evil-want-minibuffer t)
        (setq evil-respect-visual-line-mode t)
        (setq evil-undo-system 'undo-fu)
        (setq evil-want-fine-undo t)

        ;; Since =evil-mode= take over =C-u= for buffer scrolling,
        ;; the =universal-argument= command needs to be rebind to another key
        ;; sequence, here =C-M-u=.
        (global-unset-key (kbd "C-M-u"))
        (global-unset-key (kbd "C-u"))
        (global-set-key (kbd "C-M-u") 'universal-argument)
        (global-set-key (kbd "C-u") 'evil-scroll-up)
        ;; Keybinding preferences
        (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
        (global-set-key (kbd "<lwindow-j>") 'ignore)
        (global-set-key (kbd "<lwindow-k>") 'ignore)

        (setq evil-shift-width tab-width)

        ;; use evil in minibuffers with Ctrl key.
        (let ((map minibuffer-mode-map))
          (define-key map (kbd "C-j") 'next-line-or-history-element)
          (define-key map (kbd "C-k") 'previous-line-or-history-element)
          (define-key map (kbd "C-r") 'consult-history))

        ,@(if hide-state-message?
              `(;; Hide state messages, rely on "<N>", "<I>" and "<V>" in modeline
                (setq evil-insert-state-message nil)
                (setq evil-visual-state-message nil))
              '())

        (with-eval-after-load
            'evil
          (add-hook 'evil-mode-hook 'rde-evil-hook)
          (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
          (define-key evil-insert-state-map
            (kbd "C-h") 'evil-delete-backward-char-and-join)

          ;; Use visual line motions even outside of visual-line-mode buffers
          (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
          (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
          (define-key evil-operator-state-map (kbd "j") 'evil-next-line)
          (define-key evil-operator-state-map (kbd "k") 'evil-previous-line)
          ;; undo and redo
          (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
          (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo)

          (evil-define-key '(normal insert visual)
                           org-mode-map (kbd "C-j") 'org-next-visible-heading)
          (evil-define-key '(normal insert visual)
                           org-mode-map (kbd "C-k")
                           'org-previous-visible-heading)
          (evil-define-key '(normal insert visual)
                           org-mode-map (kbd "M-j") 'org-metadown)
          (evil-define-key '(normal insert visual)
                           org-mode-map (kbd "M-k") 'org-metaup)

          (evil-set-initial-state 'messages-buffer-mode 'normal)
          (evil-set-initial-state 'dashboard-mode 'normal)

          ;; Is this a bug in evil-collection?
          (setq evil-collection-company-use-tng nil)
          (setq evil-collection-outline-bind-tab-p nil)

          (with-eval-after-load
              'winner
            (let ((map evil-window-map))
              (define-key map (kbd "u") 'winner-undo)
              (define-key map (kbd "U") 'winner-redo))))

        (with-eval-after-load
            'evil-collection
          (setq evil-collection-mode-list
                (remove 'lispy evil-collection-mode-list)))

        (add-hook 'org-mode-hook 'evil-org-mode)
        (add-hook 'org-agenda-mode-hook 'evil-org-mode)
        (with-eval-after-load
            'org
          (add-hook 'evil-org-mode-hook
                    (lambda ()
                      (evil-org-set-key-theme
                       '(navigation todo insert textobjects additional))))
          (with-eval-after-load
              'evil-org
            (require 'evil-org-agenda)
            (evil-org-agenda-set-keys))
          ,@(if (get-value 'emacs-mini-frame? config)
                `((with-eval-after-load
                      'mini-frame
                    (dolist (element
                                '(evil-change
                                  evil-delete
                                  evil-org-delete
                                  evil-yank
                                  evil-find-char
                                  evil-find-char-backward
                                  evil-record-macro
                                  evil-replace
                                  evil-org-replace
                                  evil-indent
                                  evil-fill
                                  evil-fill-and-move
                                  evil-use-register))
                            (push element mini-frame-ignore-commands))))
                '())))
      #:elisp-packages (list emacs-evil emacs-evil-collection emacs-evil-org
                             emacs-evil-commentary emacs-evil-surround
                             emacs-undo-fu)
      #:authors '("Nicolas Graves <ngraves@ngraves.fr>")
      #:summary "\
Extensible vi layer for Emacs."
      #:commentary "\
Evil configuration mostly taken from daviwil. Currently there is still an
issue with the cursor in normal-mode whne using modus-vivendi or
modus-vivendi-tinted.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-evil)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-undo-fu-session
          #:key
          (emacs-undo-fu-session emacs-undo-fu-session))
  "Save & recover undo steps between Emacs sessions."
  (ensure-pred file-like? emacs-undo-fu-session)

  (define emacs-f-name 'undo-fu-session)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'xdg-base-directories-configuration config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'undo-fu-session))
        (autoload 'undo-fu-session-global-mode "undo-fu-session")
        ;; This hack is necessary for the rde-package to be built. The call to
        ;; the global mode tries to ensure existence of a directory, which
        ;; breaks the build with /homeless-shelter. But we can't set the
        ;; undo-fu-session-directory to /tmp permanently either.
        (setq undo-fu-session-directory
              (if (getenv "XDG_CACHE_HOME")
                  (concat (getenv "XDG_CACHE_HOME") "/emacs/undo-fu-session")
                  "/tmp"))
        (setq undo-fu-session-file-limit 1000)
        (undo-fu-session-global-mode 1))
      #:elisp-packages (list emacs-undo-fu-session)
      #:summary "Save & recover undo steps between Emacs sessions."
      #:authors '("Nicolas Graves <ngraves@ngraves.fr>"))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;;; emacs-xyz.scm end here
