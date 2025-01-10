;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024 jgart <jgart@dismail.de>
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

(define-module (rde features emacs-meow)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services emacs)
  #:use-module (rde serializers elisp)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (meow-programmer-dvorak-layout
            meow-dvorak-simplified-layout
            meow-qwerty-layout
            meow-colemak-layout
            meow-colemak-dh-layout

            feature-emacs-meow))


(define meow-programmer-dvorak-layout
  `((defun meow-setup ()
      (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
      (meow-leader-define-key
       '("?" . meow-cheatsheet))
      (meow-motion-overwrite-define-key
       ;; custom keybinding for motion state
       '("<escape>" . ignore))
      (meow-normal-define-key
       '("?" . meow-cheatsheet)
       '("*" . meow-expand-0)
       '("=" . meow-expand-9)
       '("!" . meow-expand-8)
       '("[" . meow-expand-7)
       '("]" . meow-expand-6)
       '("{" . meow-expand-5)
       '("+" . meow-expand-4)
       '("}" . meow-expand-3)
       '(")" . meow-expand-2)
       '("(" . meow-expand-1)
       '("1" . digit-argument)
       '("2" . digit-argument)
       '("3" . digit-argument)
       '("4" . digit-argument)
       '("5" . digit-argument)
       '("6" . digit-argument)
       '("7" . digit-argument)
       '("8" . digit-argument)
       '("9" . digit-argument)
       '("0" . digit-argument)
       '("-" . negative-argument)
       '(";" . meow-reverse)
       '("," . meow-inner-of-thing)
       '("." . meow-bounds-of-thing)
       '("<" . meow-beginning-of-thing)
       '(">" . meow-end-of-thing)
       '("a" . meow-append)
       '("A" . meow-open-below)
       '("b" . meow-back-word)
       '("B" . meow-back-symbol)
       '("c" . meow-change)
       '("d" . meow-delete)
       '("D" . meow-backward-delete)
       '("e" . meow-line)
       '("E" . meow-goto-line)
       '("f" . meow-find)
       '("g" . meow-cancel-selection)
       '("G" . meow-grab)
       '("h" . meow-left)
       '("H" . meow-left-expand)
       '("i" . meow-insert)
       '("I" . meow-open-above)
       '("j" . meow-join)
       '("k" . meow-kill)
       '("l" . meow-till)
       '("m" . meow-mark-word)
       '("M" . meow-mark-symbol)
       '("n" . meow-next)
       '("N" . meow-next-expand)
       '("o" . meow-block)
       '("O" . meow-to-block)
       '("p" . meow-prev)
       '("P" . meow-prev-expand)
       '("q" . meow-quit)
       '("r" . meow-replace)
       '("R" . meow-swap-grab)
       '("s" . meow-search)
       '("t" . meow-right)
       '("T" . meow-right-expand)
       '("u" . meow-undo)
       '("U" . meow-undo-in-selection)
       '("v" . meow-visit)
       '("w" . meow-next-word)
       '("W" . meow-next-symbol)
       '("x" . meow-save)
       '("X" . meow-sync-grab)
       '("y" . meow-yank)
       '("z" . meow-pop-selection)
       '("'" . repeat)
       '("<escape>" . ignore)))))


(define* (make-colemak-layout
          #:key
          (use-mod-dh? #f))
  `((defun meow-setup ()
      (setq meow-cheatsheet-layout
            ,@(if use-mod-dh?
                  '((setq meow-cheatsheet-layout 'meow-cheatsheet-layout-colemak-dh))
                  '((setq meow-cheatsheet-layout 'meow-cheatsheet-layout-colemak-dh))))
      (meow-motion-overwrite-define-key
       ;; Use e to move up, n to move down.
       ;; Since special modes usually use n to move down, we only overwrite e here.
       '("e" . meow-prev)
       '("<escape>" . ignore))
      (meow-leader-define-key
       '("?" . meow-cheatsheet)
       ;; To execute the originally e in MOTION state, use SPC e.
       '("e" . "H-e")
       '("1" . meow-digit-argument)
       '("2" . meow-digit-argument)
       '("3" . meow-digit-argument)
       '("4" . meow-digit-argument)
       '("5" . meow-digit-argument)
       '("6" . meow-digit-argument)
       '("7" . meow-digit-argument)
       '("8" . meow-digit-argument)
       '("9" . meow-digit-argument)
       '("0" . meow-digit-argument))
      (meow-normal-define-key
       '("0" . meow-expand-0)
       '("1" . meow-expand-1)
       '("2" . meow-expand-2)
       '("3" . meow-expand-3)
       '("4" . meow-expand-4)
       '("5" . meow-expand-5)
       '("6" . meow-expand-6)
       '("7" . meow-expand-7)
       '("8" . meow-expand-8)
       '("9" . meow-expand-9)
       '("-" . negative-argument)
       '(";" . meow-reverse)
       '("," . meow-inner-of-thing)
       '("." . meow-bounds-of-thing)
       '("[" . meow-beginning-of-thing)
       '("]" . meow-end-of-thing)
       '("/" . meow-visit)
       '("a" . meow-append)
       '("A" . meow-open-below)
       '("b" . meow-back-word)
       '("B" . meow-back-symbol)
       '("c" . meow-change)
       '("d" . meow-delete)
       '("e" . meow-prev)
       '("E" . meow-prev-expand)
       '("f" . meow-find)
       '("g" . meow-cancel-selection)
       '("G" . meow-grab)
       '("h" . meow-left)
       '("H" . meow-left-expand)
       '("i" . meow-right)
       '("I" . meow-right-expand)
       '("j" . meow-join)
       '("k" . meow-kill)
       '("l" . meow-line)
       '("L" . meow-goto-line)
       '("m" . meow-mark-word)
       '("M" . meow-mark-symbol)
       '("n" . meow-next)
       '("N" . meow-next-expand)
       '("o" . meow-block)
       '("O" . meow-to-block)
       '("p" . meow-yank)
       '("q" . meow-quit)
       '("r" . meow-replace)
       '("s" . meow-insert)
       '("S" . meow-open-above)
       '("t" . meow-till)
       '("u" . meow-undo)
       '("U" . meow-undo-in-selection)
       '("v" . meow-search)
       '("w" . meow-next-word)
       '("W" . meow-next-symbol)
       '("x" . meow-delete)
       '("X" . meow-backward-delete)
       '("y" . meow-save)
       '("z" . meow-pop-selection)
       '("'" . repeat)
       '("<escape>" . ignore)))))

(define meow-colemak-layout (make-colemak-layout))

;; https://colemakmods.github.io/mod-dh/
(define meow-colemak-dh-layout
  (make-colemak-layout #:use-mod-dh? #t))


(define meow-qwerty-layout
  `((defun meow-setup ()
      (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
      (meow-motion-overwrite-define-key
       '("j" . meow-next)
       '("k" . meow-prev)
       '("<escape>" . ignore))
      (meow-leader-define-key
       ;; SPC j/k will run the original command in MOTION state.
       '("j" . "H-j")
       '("k" . "H-k")
       ;; Use SPC (0-9) for digit arguments.
       '("1" . meow-digit-argument)
       '("2" . meow-digit-argument)
       '("3" . meow-digit-argument)
       '("4" . meow-digit-argument)
       '("5" . meow-digit-argument)
       '("6" . meow-digit-argument)
       '("7" . meow-digit-argument)
       '("8" . meow-digit-argument)
       '("9" . meow-digit-argument)
       '("0" . meow-digit-argument)
       '("/" . meow-keypad-describe-key)
       '("?" . meow-cheatsheet))
      (meow-normal-define-key
       '("0" . meow-expand-0)
       '("9" . meow-expand-9)
       '("8" . meow-expand-8)
       '("7" . meow-expand-7)
       '("6" . meow-expand-6)
       '("5" . meow-expand-5)
       '("4" . meow-expand-4)
       '("3" . meow-expand-3)
       '("2" . meow-expand-2)
       '("1" . meow-expand-1)
       '("-" . negative-argument)
       '(";" . meow-reverse)
       '("," . meow-inner-of-thing)
       '("." . meow-bounds-of-thing)
       '("[" . meow-beginning-of-thing)
       '("]" . meow-end-of-thing)
       '("a" . meow-append)
       '("A" . meow-open-below)
       '("b" . meow-back-word)
       '("B" . meow-back-symbol)
       '("c" . meow-change)
       '("d" . meow-delete)
       '("D" . meow-backward-delete)
       '("e" . meow-next-word)
       '("E" . meow-next-symbol)
       '("f" . meow-find)
       '("t" . meow-till)
       '("g" . meow-cancel-selection)
       '("G" . meow-grab)
       '("h" . meow-left)
       '("H" . meow-left-expand)
       '("i" . meow-insert)
       '("I" . meow-open-above)
       '("j" . meow-next)
       '("J" . meow-next-expand)
       '("k" . meow-prev)
       '("K" . meow-prev-expand)
       '("l" . meow-right)
       '("L" . meow-right-expand)
       '("m" . meow-join)
       '("n" . meow-search)
       '("o" . meow-block)
       '("O" . meow-to-block)
       '("p" . meow-yank)
       '("q" . meow-quit)
       '("Q" . meow-goto-line)
       '("r" . meow-replace)
       '("R" . meow-swap-grab)
       '("s" . meow-kill)
       '("u" . meow-undo)
       '("U" . meow-undo-in-selection)
       '("v" . meow-visit)
       '("w" . meow-mark-word)
       '("W" . meow-mark-symbol)
       '("x" . meow-line)
       '("X" . meow-goto-line)
       '("y" . meow-save)
       '("Y" . meow-sync-grab)
       '("z" . meow-pop-selection)
       '("'" . repeat)
       '("<escape>" . ignore)))))


(define* (feature-emacs-meow
          #:key
          (emacs-meow emacs-meow)
          (meow-keyboard-layout meow-qwerty-layout))
  "Configure Meow for Emacs."
  (ensure-pred file-like? emacs-meow)

  (define emacs-f-name 'meow)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'meow-core))
        (autoload 'meow-global-mode "meow-core")
        (with-eval-after-load 'meow-core
          (require 'meow)
          ,@meow-keyboard-layout
          (meow-setup))
        (if after-init-time
            (meow-global-mode 1)
            (add-hook 'after-init-hook (lambda ()
                                         (meow-global-mode 1)))))

      #:elisp-packages (list emacs-meow)
      #:authors '("jgart <jgart@dismail.de>")
      #:summary "\
Yet another modal editing on Emacs."
      #:commentary "\
Meow configuration with support for various layouts.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-meow)))
   (home-services-getter get-home-services)))
