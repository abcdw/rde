;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features shellutils)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (gnu home services)
  #:use-module (rde home services shells)
  #:use-module (rde home services shellutils)
  #:use-module (gnu services)
  #:use-module (gnu packages base)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-compile
            feature-direnv))

(define* (feature-compile
          #:key
          (make gnu-make)
          (recompile-key "s-r")
          (notify-on-finish? #t)
          (buffers-to-ignore '()))
  "Configure compilation tooling."
  (ensure-pred file-like? make)
  (ensure-pred string? recompile-key)

  (define f-name 'compile)

  (define (get-home-services config)
    "Return home services related to compilation."
    (list
     (simple-service
      'add-compilation-packages
      home-profile-service-type
      (list make))
     (rde-elisp-configuration-service
      f-name
      config
      `((defgroup rde-compile nil
          "Customizations for `compile'."
          :group 'rde)

        (defvar rde-compile-notify-on-finish-p ,(if notify-on-finish? 't 'nil)
          "Wether to send notification on compilation finish or not.")

        (defvar rde-compile-buffers-to-ignore ',buffers-to-ignore
          "A list of buffers to not send notifications from.")

        (defun rde-compile-ansi-color-apply ()
          "Translate control sequences into text properties in compile buffer."
          (interactive)
          (ansi-color-apply-on-region (point-min) (point-max)))

        (autoload 'recompile "compile")
        (define-key global-map (kbd ,recompile-key) 'recompile)

        (add-hook 'compilation-start-hook 'toggle-truncate-lines)
        (add-hook 'compilation-filter-hook 'rde-compile-ansi-color-apply)

        (defun rde-compile--notification-on-action (id key)
          (select-frame-set-input-focus (selected-frame)))

        (defun rde-compile--notify-on-finish (buffer desc)
          ;; TODO: Don't send notification on interrupt
          (when (and rde-compile-notify-on-finish-p
                     (not (string= desc "interrupt\n")) ; it is in user focus rn
                     ;; frame in the focus and buffer is visible
                     (not
                      ;; comparing to t make sense because return value can be
                      ;; not only nil, but also 'unknown
                      (and (equal t (frame-focus-state (selected-frame)))
                           (get-buffer-window buffer)))
                     (not (member buffer rde-compile-buffers-to-ignore)))
            (require 'notifications)
            (notifications-notify
             :title "Compilation"
             :body (format "%s\nBuffer: %s" desc buffer)
             ;; :app-icon nil
             :actions '("focus" "Focus Frame")
             :timeout 1000
             :on-action 'rde-compile--notification-on-action)))

        (add-hook 'compilation-finish-functions 'rde-compile--notify-on-finish)

        (with-eval-after-load 'compile
          ;; Disable collapse of utf progress bar to [...]
          (setq compilation-max-output-line-length nil)
          (setq compilation-scroll-output 'first-error)
          (setq compilation-ask-about-save nil))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-direnv
          #:key
          (direnv direnv))
  "Configure direnv and related Emacs packages."
  (ensure-pred any-package? direnv)

  (define (get-home-services config)
    "Returns home services related to direnv."
    (list
     (simple-service
      'direnv-packages
      home-profile-service-type (list direnv))
     (simple-service
      'direnv-config
      home-xdg-configuration-files-service-type
      `(("direnv/direnvrc" ,(plain-file
                             "direnvrc"
                             "\
use_guixs() {
  LOCK_FILE=channels-lock.scm
  if [ -f $LOCK_FILE ]; then
    eval \"$(guix time-machine -C $LOCK_FILE -- shell \"$@\" --search-paths)\"
  else
    eval \"$(guix shell \"$@\" --search-paths)\"
  fi
}"))))
     (when (get-value 'zsh config #f)
       (simple-service
        'direnv-zsh-hook
        home-zsh-service-type
        (home-zsh-extension
         (zshrc
          (list
           "command -v direnv > /dev/null && eval \"$(direnv hook zsh)\"")))))

     ;; (add-hook 'Info-mode-hook
     ;;      (lambda ()
     ;;        (setq Info-additional-directory-list (split-string (getenv "INFOPATH") ":"))))
     (rde-elisp-configuration-service
      'envrc
      config
      `((eval-when-compile (require 'envrc))
        (add-hook 'after-init-hook 'envrc-global-mode)
        (with-eval-after-load 'envrc
         (define-key envrc-mode-map (kbd "C-c E") 'envrc-command-map)))
      #:summary "\
Source environment for the project from envrc"
      #:commentary "\
Default keybinding for `envrc-command-map'."
      #:keywords '(convenience project)
      #:elisp-packages (list emacs-envrc))))

  (feature
   (name 'direnv)
   (values `((direnv . ,direnv)))
   (home-services-getter get-home-services)))
