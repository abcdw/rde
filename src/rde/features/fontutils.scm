;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
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

(define-module (rde features fontutils)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)
  #:use-module (rde serializers elisp)
  #:use-module (rde packages fonts)
  #:use-module (gnu home services)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (rde packages fonts)
  #:use-module (srfi srfi-9)

  #:export (feature-fonts

            font
            font-size
            font-name
            font-weight
            font-specification
            make-font
            font?))

(define-record-type* <font> font
  make-font
  font?
  (name font-name)
  (size font-size)
  (weight font-weight (default 'regular))
  (package font-package))

(define (font-specification font)
  "Convert <font> record to string."
  (string-join (list
                (font-name font)
                (string-capitalize (symbol->string (font-weight font)))
                (number->string (font-size font)))
               " "))

;; Fonts by its classification: https://modernfontstacks.com/
(define* (feature-fonts
          #:key
          (default-font-size 11)
          (font-monospace
           (font
            (name "Iosevka")
            (size default-font-size)
            (package font-iosevka)))
          (font-serif
           (font
            (name "Iosevka Etoile")
            (size default-font-size)
            (package font-iosevka-etoile)))
          (font-sans
           (font
            (name "Iosevka Aile")
            (size default-font-size)
            (package font-iosevka-aile)))
          (font-unicode
           (font
            (name "Noto Emoji")
            (size default-font-size)
            (package font-noto-emoji)))
          (emacs-fontaine emacs-fontaine)
          (use-serif-for-variable-pitch? #t)
          (extra-fontaine-presets '())
          ;; font-liberation needed for proper rendering of sites/pdfs using
          ;; Times, Helvetica, Nimbus.  Use pdffonts from poppler package to
          ;; check what fonts are needed for pdf.
          (extra-font-packages (list font-gnu-unifont font-liberation)))
  "Configure fonts.  DEFAULT-FONT-SIZE will be used for making
font-monospace default value, and it will be ignored if
#:font-monospace argument is specified."

  (ensure-pred integer? default-font-size)
  (ensure-pred font? font-monospace)
  (ensure-pred font? font-serif)
  (ensure-pred font? font-sans)
  (ensure-pred font? font-unicode)
  (ensure-pred file-like? emacs-fontaine)
  (ensure-pred elisp-config? extra-fontaine-presets)
  (ensure-pred boolean? use-serif-for-variable-pitch?)
  (ensure-pred list-of-file-likes? extra-font-packages)

  (define f-name 'fonts)

  (define (get-home-services config)
    "Return home services related to fonts."
    (define font-variable (if use-serif-for-variable-pitch?
                              font-serif
                              font-sans))
    (define default-font-height
      (inexact->exact
       ;; -5 is a random hacky adjustment to
       ;; make it work the same as in rde-faces
       (- (* (font-size font-monospace) 10) 5)))
    (list
     (simple-service
      'add-extra-fonts
      home-profile-service-type
      (append
       (map font-package
            (list font-sans font-serif font-monospace font-unicode))
       extra-font-packages))

     (simple-service
      'add-fontconfig-font-families
      home-fontconfig-service-type
      (list
       `(alias
         (family "sans-serif")
         (prefer
          (family ,(font-name font-sans))))
       `(alias
         (family "serif")
         (prefer
          (family ,(font-name font-serif))))
       `(alias
         (family "monospace")
         (prefer
          (family ,(font-name font-monospace))))
       `(alias
         (family "emoji")
         (prefer
          (family ,(font-name font-unicode))))))

     (rde-elisp-configuration-service
      f-name
      config
      `((eval-when-compile
         (require 'cl-macs)
         (require 'subr-x))
        (defvar rde-fonts-emoji-list nil
          "Cached list of emojis.")

        (defun rde-fonts--build-emojis ()
          "Create an emoji list by looping over the total range of characters."
          (delete
           nil
           (cl-loop with range = '(#x1f000 . #x1f9ff)
                    for i upto (- (cdr range) (car range))
                    collect (when-let* ((codepoint (+ (car range) i))
                                        (name (get-char-code-property
                                               codepoint 'name)))
                                       (thread-last
                                        (replace-regexp-in-string
                                         " " "-" (downcase name))
                                        (format ":%s:")
                                        (format
                                         "%s %s"
                                         (char-to-string (char-from-name name))))))))

        (defun rde-fonts-insert-emoji ()
          "Insert an emoji character to the current buffer."
          (interactive)
          (thread-first
           (completing-read
            "Select emoji: "
            (or rde-fonts-emoji-list
                (setq rde-fonts-emoji-list (rde-fonts--build-emojis))))
           (substring 0 1)
           (insert)))

        (define-key search-map "e" 'rde-fonts-insert-emoji)
        (define-key minibuffer-mode-map (kbd "C-c C-e") 'rde-fonts-insert-emoji)
        (with-eval-after-load 'fontset
          (set-fontset-font t 'symbol ,(font-name font-unicode) nil 'append)
          (set-fontset-font t 'unicode ,(font-name font-unicode) nil 'append)
          (set-fontset-font "fontset-default" nil
                            (font-spec :name ,(font-name font-unicode))))
        (setq use-default-font-for-symbols nil)
        (require 'fontaine)
        (setq fontaine-current-preset t)
        (setq fontaine-presets
              '((t
                 :default-family ,(font-name font-monospace)
                 :default-height ,default-font-height
                 :fixed-pitch-family ,(font-name font-monospace)
                 :fixed-pitch-height 1.0
                 :variable-pitch-family ,(font-name font-variable)
                 :variable-pitch-height 1.0
                 :variable-pitch-weight ,(font-weight font-variable))
                (regular)
                (large :default-weight semilight
                       :default-height ,(+ default-font-height 40)
                       :bold-weight extrabold)
                ,@extra-fontaine-presets))
        (require 'xdg)
        (setq fontaine-latest-state-file
              (expand-file-name "emacs/fontaine-latest.state.eld"
                                (xdg-cache-home)))

        (defun rde-font--set-default-fonts ()
          (fontaine-set-preset t))

        (if after-init-time
            (when (display-graphic-p) (rde-font--set-default-fonts))
            (add-hook 'after-init-hook 'rde-font--set-default-fonts)))
      #:elisp-packages (list emacs-fontaine))))

  (feature
   (name f-name)
   (values
    (append
     `((,f-name . #t)
       (emacs-fontaine . ,emacs-fontaine)
       (emacs-faces . #t))
     (make-feature-values font-sans font-monospace
                          font-serif font-unicode)))
   (home-services-getter get-home-services)))
