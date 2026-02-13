;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024-2026 jgart <jgart@dismail.de>
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

(define-module (rde features prolog)
  #:use-module (ice-9 match)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages prolog)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:export (feature-prolog))

(define (prolog-startup-file name)
  (plain-file name
              "\
:- use_module(library(dcgs)).
:- use_module(library(debug)).
:- use_module(library(clpz))."))

(define* (feature-prolog
          #:key
          (prolog scryer-prolog)
          (binary-name
           (match (package-name prolog)
             ("scryer-prolog" "scryer-prolog"
              "trealla" "tpl")))
          (emacs-ediprolog emacs-ediprolog)
          (dwim-key "C-c C-c"))
  "Configure Prolog for Emacs."
  (ensure-pred file-like? prolog)
  (ensure-pred string? binary-name)
  (ensure-pred file-like? emacs-ediprolog)
  (ensure-pred string? dwim-key)

  (define f-name 'prolog)

  (define (get-home-services config)
    (list
     (simple-service
      'add-prolog-home-package
      home-profile-service-type
      (list prolog))
     (simple-service
      'add-prolog-startup-file
      home-files-service-type
      (match (package-name prolog)
        ("scryer-prolog"
         `((".scryerrc" ,(prolog-startup-file "scryerrc"))))
        ("trealla"
         `((".tplrc" ,(prolog-startup-file "tplrc"))))
        (_ '())))
     (when (get-value 'emacs config #f)
       (rde-elisp-configuration-service
        f-name
        config
        `((add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
          (with-eval-after-load 'prolog
            (define-key prolog-mode-map (kbd ,dwim-key) 'ediprolog-dwim))
          (with-eval-after-load 'ediprolog
            (setq ediprolog-program
                  ,(file-append prolog (string-append "/bin/" binary-name)))))
        #:elisp-packages (list emacs-ediprolog)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
