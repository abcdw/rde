;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
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

(define-module (rde features ocaml)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services ocaml)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services shells)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (feature-ocaml))

(define* (feature-ocaml
          #:key
          (ocaml ocaml)
          (emacs-tuareg emacs-tuareg)
          (extra-init-ml '())
          (extra-ocaml-packages '())
          (opam? #f))
  "Configure tooling and environment for OCaml."
  (ensure-pred any-package? ocaml)
  (ensure-pred file-like? emacs-tuareg)
  (ensure-pred list-of-strings? extra-init-ml)
  (ensure-pred list-of-file-likes? extra-ocaml-packages)
  (ensure-pred boolean? opam?)

  (define f-name 'ocaml)

  (define (get-home-services config)
    "Return home services related to OCaml."
    (append
     (if (and opam? (get-value 'zsh config))
         (list
          (simple-service
           'set-opam-env-zsh
           home-zsh-service-type
           (home-zsh-extension
            (zshrc
             (list
              #~(format #f "eval \"$(~a env)\""
                        #$(file-append opam "/bin/opam")))))))
         '())
     (if (and opam? (get-value 'bash config))
         (list
          (simple-service
           'set-opam-env-bash
           home-bash-service-type
           (home-bash-extension
            (bashrc
             (list
              #~(format #f "eval \"$(~a env)\""
                        #$(file-append opam "/bin/opam")))))))
         '())
     (if opam?
         (list
          (simple-service
           'add-opam-home-envs
           home-environment-variables-service-type
           '(("OPAMROOT" . "$XDG_CACHE_HOME/opam"))))
         '())
     (list
      (simple-service
       'add-ocaml-home-packages
       home-profile-service-type
       (if opam?
           (list opam)
           (append
            (list
             ocaml-core
             ocaml-findlib
             dune
             ocaml-ocp-indent
             ocaml-merlin
             ocamlbuild)
            extra-ocaml-packages)))
      (service home-ocaml-service-type
               (home-ocaml-configuration
                (config extra-init-ml))))
     (if (get-value 'emacs config)
         (list
          (rde-elisp-configuration-service
           f-name
           config
           `((defgroup rde-ocaml nil
               "General OCaml programming utilities."
               :group 'rde)
             ,@(if opam?
                   '((defun rde-ocaml-load-merlin ()
                       "Set up `merlin-mode' for OCaml."
                       (let ((opam-share (car (process-lines
                                               "opam" "var" "share"))))
                         (when (and opam-share (file-directory-p opam-share))
                           (add-to-list 'load-path
                                        (expand-file-name
                                         "emacs/site-lisp" opam-share)))))

                     (defun rde-ocaml-set-environment ()
                       "Set corresponding opam environment variables."
                       (dolist (var (car (read-from-string
                                          (shell-command-to-string
                                           "opam config env --sexp"))))
                         (setenv (car var) (cadr var)))))
                 '())

             (define-minor-mode rde-ocaml-mode
               "Set up convenient tweaks for an OCaml programming environment."
               :group 'rde-ocaml
               (if rde-ocaml-mode
                   (progn
                     (setq-local comment-style 'multiline
                                 comment-continue "   ")
                     (when (fboundp 'prettify-symbols-mode)
                       (prettify-symbols-mode 1))
                     ,@(if opam?
                           '((rde-ocaml-load-merlin)
                             (rde-ocaml-set-environment))
                         '())
                     (autoload 'merlin-mode "merlin" nil t nil)
                     (merlin-mode))))

             (add-to-list 'auto-mode-alist '("\\.ml[ily]?\\'" . tuareg-mode))
             (add-hook 'tuareg-mode-hook 'rde-ocaml-mode)
             (setq tuareg-interactive-program
                   ,@(if opam?
                         '((format
                            "%s -nopromptcont"
                            (expand-file-name
                             "ocaml"
                             (ignore-errors
                              (car (process-lines "opam" "var" "bin"))))))
                         `((format "%s -nopromptcont"
                                   ,(file-append ocaml "/bin/ocaml")))))
             (with-eval-after-load 'tuareg-mode
               (setq tuareg-prettify-symbols-full nil))
             (with-eval-after-load 'merlin
               (setq merlin-command
                     ,(if opam?
                          'opam
                          (file-append ocaml-merlin "/bin/ocamlmerlin")))
               (setq merlin-report-warnings nil)
               (setq merlin-error-in-fringe nil)
               (setq merlin-error-check-then-move nil))
             ,@(if (get-value 'emacs-org config)
                   `((with-eval-after-load 'org
                       (add-to-list 'org-structure-template-alist
                                    '("ml" . "src ocaml")))
                     (with-eval-after-load 'ob-core
                       (require 'ob-ocaml))
                     (with-eval-after-load 'ob-ocaml
                       (setq org-babel-default-header-args:ocaml
                             '((:results . "scalar")))
                       (setq org-babel-ocaml-command
                             ,@(if opam?
                                   '((expand-file-name
                                      "ocaml"
                                      (ignore-errors
                                        (car (process-lines
                                              "opam" "var" "bin")))))
                                 `(,(file-append ocaml "/bin/ocaml"))))))
                 '()))
           #:elisp-packages (list emacs-tuareg)
           #:summary "OCaml programming utilities"
           #:commentary "Provide common OCaml programming utilities and \
tooling setup."))
       '())))

  (feature
   (name f-name)
   (values `((,f-name . ,ocaml)
             (emacs-tuareg . ,emacs-tuareg)))
   (home-services-getter get-home-services)))
