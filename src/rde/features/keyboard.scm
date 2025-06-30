;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features keyboard)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde home services keyboard)

  #:export (feature-keyboard
            feature-emacs-cua
            %dvorak-layout
            %dvorak-jcuken-layout)

  #:re-export (keyboard-layout))

;; Example of multi-layer layout: https://neo-layout.org/index_en.html

(define %dvorak-layout
  (keyboard-layout "us" "dvorak" #:options '("ctrl:nocaps")))

(define %dvorak-jcuken-layout
  (keyboard-layout
   "us,ru" "dvorak,"
   #:options '("grp:win_space_toggle" "ctrl:nocaps")))

;; TODO: Add ability to provide custom layout package or file

;; There is no default value to force user specify some keyboard
;; layout in case they use this feature

;; TODO: [Andrew Tropin, 2024-04-28] Add an ability to add custom layouts
;; https://todo.sr.ht/~abcdw/tickets/8
(define* (feature-keyboard #:key keyboard-layout)
  "Sets keyboard layout.  Affects bootloader, and XKB_* variables for
the user."
  (ensure-pred keyboard-layout? keyboard-layout)

  (define (keyboard-services values)
    "Returns home-keyboard service."
    (list
     (service home-keyboard-service-type keyboard-layout)))

  (feature
   (name 'keyboard)
   (values (make-feature-values keyboard-layout))
   (home-services-getter keyboard-services)))

(define* (feature-emacs-cua
          #:key (emacs-undo-fu emacs-undo-fu))
  "Provide IBM Common User Acces for Emacs (More usual keybindings like
Ctrl-C/Ctrl-V for copypaste and so on).  It not only alters some keybindings,
but also adjust the behavior of some region related operations and undo
system."

  (define emacs-f-name 'cua)
  (define f-name (symbol-append 'emacs- emacs-f-name))
  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((setopt cua-prefix-override-inhibit-delay 0.0001)
        (with-eval-after-load 'smartparens
          (keymap-set smartparens-mode-map "C-<right>" 'forward-word)
          (keymap-set smartparens-mode-map "M-<right>" 'sp-forward-symbol)
          (keymap-set smartparens-mode-map "C-M-<right>" 'sp-forward-sexp)

          (keymap-set smartparens-mode-map "C-<left>" 'backward-word)
          (keymap-set smartparens-mode-map "M-<left>" 'sp-backward-symbol)
          (keymap-set smartparens-mode-map "C-M-<left>" 'sp-backward-sexp))
        (with-eval-after-load 'cua-base
          (setopt cua-keep-region-after-copy t)
          ;; it doesn't want to work with cua-global-map, so we go a bit into
          ;; internals.
          (defun rde--cua-remap-undo (orig-fun &rest args)
            "Advice to remap C-z to undo-fu-only-undo in CUA mode."
            (let ((result (apply orig-fun args)))
              ;; After the original function runs, modify the keymap
              (keymap-set cua--cua-keys-keymap "C-z" 'undo-fu-only-undo)
              result))

          (advice-add 'cua--init-keymaps :around 'rde--cua-remap-undo)

          (keymap-set cua-global-keymap "C-y" 'undo-fu-only-redo)

          (autoload 'save-buffer "files")
          (keymap-set cua-global-keymap "C-s" 'save-buffer)
          (keymap-set cua-global-keymap "C-f" 'isearch-forward)
          (keymap-set isearch-mode-map "C-f" 'isearch-repeat-forward)
          (defun rde-cua-comment-dwim ()
            "Comment or uncomment the region and preserve the selection."
            (interactive)
            (if (region-active-p)
                (let ((beg (region-beginning))
                      (end (region-end))
                      (deactivate-mark nil))

                  (comment-or-uncomment-region beg end))
                (save-excursion
                 (comment-line 1))))
          (keymap-set cua-global-keymap "C-/" 'rde-cua-comment-dwim)
          (defun rde-cua-join-lines (&optional arg beg end)
            "Join current and next line or multiple lines if region selected."
            (interactive
             (progn (barf-if-buffer-read-only)
                    (cons current-prefix-arg
                          (and (use-region-p)
                               (list (region-beginning) (region-end))))))
            (if (not (or arg beg end))
                (let ((p (point)))
                  (delete-indentation -1)
                  (goto-char p))
                (delete-indentation arg beg end)))

          (keymap-set cua-global-keymap "C-S-j" 'rde-cua-join-lines))

        (if after-init-time
            (cua-mode)
            (add-hook 'after-init-hook 'cua-mode)))
      #:elisp-packages (list emacs-undo-fu))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
