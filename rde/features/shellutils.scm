;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
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
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-direnv))

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
     (when (get-value 'zsh config)
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
         (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)))
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
