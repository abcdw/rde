;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 conses <contact@conses.eu>
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

(define-module (rde features lisp)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services lisp)
  #:use-module (rde serializers lisp)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (feature-lisp))

(define (%default-sly-custom-prompt _)
  `(lambda (_p nickname error-level next-idx _c)
     (let ((dir (propertize (abbreviate-file-name default-directory)
                            'font-lock-face 'diff-mode))
           (nick (propertize nickname 'font-lock-face 'sly-mode-line))
           (idx (propertize (number-to-string next-idx)
                            'font-lock-face 'diff-mode))
           (err-level (when (cl-plusp error-level)
                        (concat (sly-make-action-button
                                 (format " [%d]" error-level)
                                 'sly-db-pop-to-debugger-maybe)
                                ""))))
       (concat "(" dir ")\n"
               (propertize "<" 'font-lock-face 'sly-mrepl-prompt-face)
               idx
               (propertize ":" 'font-lock-face 'sly-mrepl-prompt-face)
               nick
               err-level
               (propertize "> " 'font-lock-face 'sly-mrepl-prompt-face)))))

(define* (feature-lisp
          #:key
          (lisp sbcl)
          (emacs-sly emacs-sly)
          (extra-sbclrc-lisp '())
          (extra-slynk-lisp '())
          (extra-lisp-packages '())
          (extra-source-registry-files '())
          (sly-custom-prompt %default-sly-custom-prompt))
  "Set up and configure Common Lisp tooling.
Choose your implementation of choice via LISP.
EXTRA-SOURCE-REGISTRY-FILES is a list of file-likes that will be added
under @file{.config/common-lisp/source-registry.conf.d} to allow the
ASDF source-registry mechanism to discover new Lisp systems in custom
file-system locations.
Customize the sly prompt with SLY-CUSTOM-PROMPT, a procedure
that takes the current RDE configuration and should return an Emacs Lisp
function that represents the custom prompt.  If you'd rather use the
default sly prompt, simply pass #f to it.
See @code{sly-mrepl-default-prompt} for its arguments and return value."
  (ensure-pred any-package? lisp)
  (ensure-pred file-like? emacs-sly)
  (ensure-pred lisp-config? extra-sbclrc-lisp)
  (ensure-pred lisp-config? extra-slynk-lisp)
  (ensure-pred list-of-file-likes? extra-lisp-packages)
  (ensure-pred list-of-file-likes? extra-source-registry-files)
  (ensure-pred maybe-procedure? sly-custom-prompt)

  (define f-name 'lisp)

  (define (get-home-services config)
    "Return home services related to Lisp tooling."
    (list
     (service
      home-lisp-service-type
      (home-lisp-configuration
       (lisp lisp)
       (sbclrc-lisp
        `((require :asdf)
          (let ((guix-profile (merge-pathnames
                               ".guix-home/profile/lib/"
                               (user-homedir-pathname))))
            (when (and (probe-file guix-profile)
                       (ignore-errors (asdf:load-system "cffi")))
              (push guix-profile
                    (symbol-value
                     (find-symbol (string '*foreign-library-directories*)
                                  (find-package 'cffi))))))
          (let ((quicklisp-init (merge-pathnames
                                 ".local/share/quicklisp/setup.lisp"
                                 (user-homedir-pathname))))
            (when (probe-file quicklisp-init)
              (load quicklisp-init)))
          ,@extra-sbclrc-lisp))
       (slynk-lisp
        `((setf (cdr (assoc 'slynk:*string-elision-length*
                            slynk:*slynk-pprint-bindings*)) nil)
          ,@extra-slynk-lisp))
       (extra-source-registry-files extra-source-registry-files)))
     (simple-service
      'add-extra-lisp-packages
      home-profile-service-type
      extra-lisp-packages)
     (rde-elisp-configuration-service
      f-name
      config
      `((eval-when-compile
         (require 'sly))
        (defun rde-lisp-sly-autoconnect ()
          "Start a SLY REPL unless an active connection is already present."
          (unless (sly-connected-p)
            (save-excursion
              (sly))))

        (defun rde-lisp-setup-sly-history ()
          "Create an empty history file for SLY if missing."
          (unless (file-exists-p sly-mrepl-history-file-name)
            (make-empty-file sly-mrepl-history-file-name)))

        (with-eval-after-load 'lisp-mode
          (setq inferior-lisp-program
                ,(file-append lisp "/bin/" (package-name lisp)))
          (define-key lisp-mode-map (kbd "C-c C-z") 'sly-mrepl))
        (add-hook 'debugger-mode-hook 'toggle-truncate-lines)
        (add-hook 'sly-mode-hook 'rde-lisp-sly-autoconnect)
        (add-hook 'sly-mode-hook 'rde-lisp-setup-sly-history)
        (add-to-list 'display-buffer-alist
                     `(,(rx "*sly-mrepl" (* any) "*")
                       (display-buffer-no-window)
                       (allow-no-window . t)))
        (sly-setup)
        (with-eval-after-load 'sly
          (setq sly-words-of-encouragement '(""))
          (setq sly-command-switch-to-existing-lisp 'always)
          (setq sly-description-autofocus t)
          (setq sly-net-coding-system 'utf-8-unix)
          (setq sly-connection-poll-interval 0.1)
          (setq sly-enable-evaluate-in-emacs t)
          (setq sly-keep-buffers-on-connection-close nil))
        (with-eval-after-load 'sly-mrepl
          (require 'xdg)
          (let ((map sly-mode-map))
            (define-key map (kbd "C-c C-b") 'sly-eval-buffer)
            (define-key map (kbd "C-c C-q") 'sly-interrupt))
          (let ((map sly-mrepl-mode-map))
            (define-key map (kbd "C-M-q") 'indent-sexp)
            (define-key map (kbd "C-c C-z") 'sly-switch-to-most-recent)
            (define-key map (kbd "C-c M-n") 'sly-mrepl-next-prompt)
            (define-key map (kbd "C-c M-p") 'sly-mrepl-previous-prompt))
          (setq sly-mrepl-history-file-name
                (expand-file-name "emacs/sly-mrepl-history"
                                  (or (xdg-cache-home) "~/.cache")))
          (setq sly-mrepl-prevent-duplicate-history t)
          (setq sly-mrepl-pop-sylvester nil)
          ,@(if sly-custom-prompt
                `((setq sly-mrepl-prompt-formatter
                        ,(sly-custom-prompt config)))
                '()))
        ,@(if (get-value 'emacs-org config)
              '((with-eval-after-load 'org
                  (require 'ob-lisp)
                  (add-to-list 'org-structure-template-alist '("li" . "src lisp")))
                (with-eval-after-load 'ob-lisp
                  (setq org-babel-lisp-eval-fn 'sly-eval))
                (with-eval-after-load 'ob-core
                  (setq org-babel-default-header-args:lisp '((:results . "scalar")))))
              '()))
      #:elisp-packages (list emacs-sly emacs-sly-asdf)
      #:summary "Extensions for Common Lisp tooling"
      #:commentary "Provide reasonable defaults to Common Lisp
 programming utilities.")))

  (feature
   (name f-name)
   (values `((,f-name . ,lisp)))
   (home-services-getter get-home-services)))
