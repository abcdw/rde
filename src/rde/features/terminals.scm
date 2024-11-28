;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features terminals)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde features fontutils)
  #:use-module (rde home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu home-services terminals)
  #:use-module (gnu services)
  #:use-module (rde packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages terminals)
  #:use-module (guix gexp)

  #:export (feature-alacritty
            feature-vterm
            feature-foot))

(define (font-weight->style weight)
  "Transform kebab-cased symbols to capitalized strings without dashes."
  (string-capitalize (string-delete #\- (symbol->string weight))))

(define* (feature-alacritty
          #:key
          config-file
          (package alacritty)
          (default-terminal? #f)
          (backup-terminal? #t)
          (software-rendering? #f))
  "Configure Alacritty terminal."
  (ensure-pred maybe-file-like? config-file)
  (ensure-pred any-package? package)

  ;; TODO: Implement home service and rewrite to it to make this
  ;; feature extendable.
  (define (alacritty-home-services config)
    "Returns home services related to Alacritty."
    (define font-mono (get-value 'font-monospace config #f))
    (list
     (service
      home-alacritty-service-type
      (home-alacritty-configuration
       (package package)
       (config
        `((window . ((padding . ((x . 10)
                                 (y . 5)))))
          ,@(if software-rendering?
                '((env . ((LIBGL_ALWAYS_SOFTWARE . "1"))))
                '())
          ,@(if font-mono
                `((font . ((normal . ((style . , (font-weight->style
                                                  (font-weight font-mono)))
                                      (family . ,(font-name font-mono))))
                           (size . ,(font-size font-mono)))))
              '())
          ,@(if config-file
                `((import . #(,config-file)))
                '())))))))

  (feature
   (name 'alacritty)
   (values
    `((alacritty . ,package)
      ,@(if default-terminal?
            `((default-terminal . ,(file-append package "/bin/alacritty")))
            '())
      ,@(if backup-terminal?
            `((backup-terminal . ,(file-append package "/bin/alacritty")))
            '())))
   (home-services-getter alacritty-home-services)))


(define* (feature-vterm
          #:key
          (emacs-vterm emacs-vterm))
  "Configure emacs-vterm and shells."
  (ensure-pred file-like? emacs-vterm)

  (define (get-home-services config)
    (require-value 'emacs config)
    (define setup-vterm (local-file "./zsh/vterm" "setup-vterm"))

    (list
     (rde-elisp-configuration-service
      'vterm
      config
      `((define-key global-map (kbd "s-t") 'vterm)
        ,@(if (get-value 'emacs-consult config #f)
              `((eval-when-compile
                 (require 'cl-macs))

                (with-eval-after-load
                 'vterm
                 (defun vterm-consult-yank-pop-wrapper (orig-fun &rest args)
                   "Use `vterm-insert' instead of `insert-for-yank' if
`major-mode' is `vterm-mode'."
                   (interactive "p")
                   (if (equal major-mode 'vterm-mode)
                       (let ((inhibit-read-only t)
                             (yank-undo-function (lambda (_s _e) (vterm-undo))))
                         (cl-letf (((symbol-function 'insert-for-yank)
                                    'vterm-insert))
                                  (apply orig-fun args)))
                       (apply orig-fun args)))

                 (advice-add 'consult-yank-pop :around
                             'vterm-consult-yank-pop-wrapper)))
              '())
        ,@(if (get-value 'emacs-project config #f)
              `((with-eval-after-load
                 'project
                 (defun project-vterm ()
                   "Start vterm in the current project's root directory.
If a buffer already exists for running vterm in the project's root,
switch to it.  Otherwise, create a new vterm buffer.
With \\[universal-argument] prefix arg, create a new vterm buffer even
if one already exists."
                   (interactive)
                   (let* ((default-directory (project-root (project-current t)))
                          (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
                          (vterm-buffer (get-buffer vterm-buffer-name)))
                     (if (and vterm-buffer (not current-prefix-arg))
                         (pop-to-buffer-same-window vterm-buffer)
                         (vterm t))))
                 (define-key project-prefix-map (kbd "t") 'project-vterm)))
              '()))
      #:summary "\
Full-fledged terminal in Emacs"
      #:commentary "\
Adds integration with zsh, `consult-yank' and `project-prefix-map', provides
`s-t' hotkey."
      #:keywords '(convenience)
      #:elisp-packages `(,emacs-vterm
                         ,@(if (get-value 'emacs-consult config #f)
                               (list (get-value 'emacs-consult config))
                               '())))

     (when (get-value 'zsh config #f)
       (simple-service
        'emacs-vterm-zsh-configuration
        home-zsh-service-type
        (home-zsh-extension
         (privileged? #t)
         (zshrc
          (list #~(format #f "source ~a" #$setup-vterm))))))))

  (feature
   (name 'vterm)
   (values
    `((vterm . #t)
      (emacs-vterm . ,emacs-vterm)))
   (home-services-getter get-home-services)))

(define* (feature-foot
          #:key
          (foot foot)
          (default-terminal? #f)
          (backup-terminal? #t)
          (theme "modus-operandi"))
  "Configure foot terminal."
  (ensure-pred file-like? foot)
  (ensure-pred string? theme)

  (define (get-home-services config)
    (define font-mono (get-value 'font-monospace config #f))
    (list
     (simple-service
      'foot-package
      home-profile-service-type
      (list foot))
     ;; TODO: Migrate to home service to make it extandable
     (simple-service
      'foot-configuration
      home-xdg-configuration-files-service-type
      `(("foot/foot.ini"
         ,(mixed-text-file
           "foot.ini"
           "pad = 10x5\n"
           (if font-mono
               (string-append "font=monospace:size="
                              (number->string (font-size font-mono)) "\n")
               "")
           ;; "dpi-aware = yes\n" ; use dpi instead of output scaling factor
           "[main]
include = " (file-append foot "/share/foot/themes/") theme "\n"))))))

  (feature
   (name 'foot)
   (values
    `((foot . ,foot)
      ,@(if default-terminal?
            `((default-terminal . ,(file-append foot "/bin/foot")))
            '())
      ,@(if backup-terminal?
            `((backup-terminal . ,(file-append foot "/bin/foot")))
            '())))
   (home-services-getter get-home-services)))
