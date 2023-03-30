;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022, 2023 conses <contact@conses.eu>
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

(define-module (rde features emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde features emacs)
  #:use-module (rde features fontutils)

  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (rde home services emacs-xyz)
  #:use-module (rde home services emacs)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde serializers elisp)

  #:use-module (guix gexp)
  #:use-module (rde gexp)
  #:use-module (guix packages)

  #:use-module (ice-9 match)

  #:export (;; UI
            feature-emacs-appearance
            feature-emacs-modus-themes
            feature-emacs-circadian
            feature-emacs-faces
            feature-emacs-which-key
            feature-emacs-keycast
            feature-emacs-all-the-icons

            ;; Generic
            feature-emacs-input-methods
            feature-emacs-battery
            feature-emacs-time
            feature-emacs-calendar
            feature-emacs-tramp
            feature-emacs-dired
            feature-emacs-eshell
            feature-emacs-calc
            feature-emacs-re-builder
            feature-emacs-comint
            feature-emacs-help
            feature-emacs-shell
            feature-emacs-browse-url
            feature-emacs-tab-bar

            ;; Completion
            feature-emacs-completion
            feature-emacs-vertico
            feature-emacs-mct
            feature-emacs-corfu
            feature-emacs-tempel

            ;; Focus
            feature-emacs-monocle
            feature-emacs-project
            feature-emacs-perspective
            feature-emacs-ednc
            feature-emacs-ace-window

            ;; Development
            feature-emacs-smartparens
            feature-emacs-eglot
            feature-emacs-flymake
            feature-emacs-elisp
            feature-emacs-git
            feature-emacs-geiser
            feature-emacs-guix
            feature-emacs-xref

            ;; Reading
            feature-emacs-pdf-tools
            feature-emacs-nov-el
            feature-emacs-elfeed
            feature-emacs-info

            ;; Notetaking
            feature-emacs-org
            feature-emacs-org-roam
            feature-emacs-org-agenda
            feature-emacs-citar
            feature-emacs-org-protocol
            feature-emacs-spelling
            feature-emacs-org-recur
            feature-emacs-graphviz

            ;; Communication
            feature-emacs-telega
            feature-emacs-ebdb
            feature-emacs-elpher

            ;; Multimedia
            feature-emacs-dashboard
            feature-emacs-emms
            feature-emacs-pulseaudio-control
            feature-emacs-webpaste
            feature-emacs-display-wttr))



;;;
;;; Helpers.
;;;

(define (strip-emacs-name p)
  (let ((name (package-name p)))
    (string->symbol
     (if (string-prefix? "emacs-" name)
         (string-drop name (string-length "emacs-"))
         name))))

;;;
;;; UI.
;;;

;; This define used to visually separate components in imenu.  Later it can be
;; removed and done by extending imenu with feature category information.
(define --UI--)

;; TODO: Take a look at nano-emacs and maybe incorporate some ideas from it
;; https://github.com/rougier/nano-emacs
;; MAYBE: Use doom-nano-modeline
;; https://github.com/ronisbr/doom-nano-modeline
(define* (feature-emacs-appearance
          #:key
          (margin 8)
          (fringes 8)
          (mode-line-padding 4)
          (header-line-padding 4)
          (tab-bar-padding 4)
          (header-line-as-mode-line? #t))
  "Make Emacs look more modern and minimalistic.
It achieves this by removing most UI elements and allows you to add MARGIN,
FRINGES, and various padding values for MODE-LINE-PADDING, HEADER-LINE-PADDING,
and TAB-BAR-PADDING.
Move the mode line to the top by setting HEADER-LINE-AS-MODE-LINE? to #t."
  (ensure-pred integer? margin)
  (ensure-pred maybe-integer? fringes)
  (ensure-pred number? mode-line-padding)
  (ensure-pred number? header-line-padding)
  (ensure-pred number? tab-bar-padding)
  (ensure-pred boolean? header-line-as-mode-line?)

  (define emacs-f-name 'appearance)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((set-default 'cursor-type  '(bar . 1))
        (blink-cursor-mode 0)
        (setq-default cursor-in-non-selected-windows nil)
        (setq bookmark-set-fringe-mark nil)

        (with-eval-after-load 'minions-autoloads
          (minions-mode))
        (with-eval-after-load 'minions
          (setq minions-mode-line-lighter ";"))

        (setq mode-line-compact 'long)

        ,@(if header-line-as-mode-line?
              `((setq minions-mode-line-minor-modes-map
                      (let ((map (make-sparse-keymap)))
                        ;; Make minions menu work in header line
                        (define-key map (vector 'header-line 'down-mouse-1)
                          'minions-minor-modes-menu)
                        map))
                (defun rde--move-mode-line-to-header ()
                  "Move mode-line to header-line.
This function is needed for various modes to set up the mode-line late."
                  (setq-local header-line-format mode-line-format)
                  (setq-local mode-line-format nil))

                (add-hook 'calendar-initial-window-hook
                          'rde--move-mode-line-to-header)
                (setq-default header-line-format mode-line-format)
                (setq-default mode-line-format nil)
                (setq mode-line-format nil))
              '())

        (with-eval-after-load 'menu-bar
          (menu-bar-mode 0))
        (with-eval-after-load 'tool-bar
          (tool-bar-mode 0))
        (with-eval-after-load 'scroll-bar
          (scroll-bar-mode 0))
        (with-eval-after-load 'fringe
          (fringe-mode ,(or fringes 0)))

        (set-frame-parameter (selected-frame) 'internal-border-width ,margin)

        (setq use-dialog-box nil)
        (setq use-file-dialog nil)

        (setq window-divider-default-right-width ,margin)

        (window-divider-mode))
      #:early-init
      `((push '(menu-bar-lines . 0) default-frame-alist)
        (push '(tool-bar-lines . 0) default-frame-alist)
        (push '(vertical-scroll-bars) default-frame-alist)
        (push '(horizontal-scroll-bars) default-frame-alist)
        (push (cons 'left-fringe ,(or fringes 0)) default-frame-alist)
        (push (cons 'right-fringe ,(or fringes 0)) default-frame-alist)
        (push '(no-special-glyphs) default-frame-alist)
        (push '(undecorated) default-frame-alist)
        (setq menu-bar-mode nil
              tool-bar-mode nil
              scroll-bar-mode nil)

        (push '(internal-border-width . ,margin) default-frame-alist)
        ;; (setq-default fringes-outside-margins t)

        ,@(if (get-value 'emacs-advanced-user? config)
              '((setq inhibit-startup-screen t)
                (setq inhibit-startup-message t)
                (setq initial-scratch-message nil))
              '()))
      #:elisp-packages (list emacs-minions)
      #:keywords '(appearance mode-line faces accessibility)
      #:summary "Set more visually appealing defaults"
      #:commentary "\
The goal is to provide a non-distractive and safe visual design.

Modeline is simplified and moved to the top of the window.

Almost all visual elements are disabled.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-header-line-as-mode-line? . ,header-line-as-mode-line?)
             (emacs-mode-line-padding . ,mode-line-padding)
             (emacs-header-line-padding . ,header-line-padding)
             (emacs-tab-bar-padding . ,tab-bar-padding)
             (emacs-margin . ,margin)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-modus-themes
          #:key
          (emacs-modus-themes emacs-modus-themes)
          (extra-after-enable-theme-hooks '())
          (dark? #f)
          (deuteranopia? #t)
          (deuteranopia-red-blue-diffs? #f)
          (headings-scaling? #f)
          (extra-modus-themes-overrides '()))
  "Configure modus-themes, a set of elegant and highly accessible
themes for Emacs.  DEUTERANOPIA? replaces red/green tones with yellow/blue,
which helps people with color blindness.  If DEUTERANOPIA-RED-BLUE-DIFFS?  is
set, red/blue colors will be used instead.  If HEADINGS-SCALING? is set,
different level headings will have different size."
  (ensure-pred file-like? emacs-modus-themes)
  (ensure-pred list? extra-after-enable-theme-hooks)
  (ensure-pred boolean? dark?)
  (ensure-pred boolean? deuteranopia?)
  (ensure-pred boolean? headings-scaling?)
  (ensure-pred elisp-config? extra-modus-themes-overrides)

  (define emacs-f-name 'modus-themes)
  (define f-name (symbol-append 'emacs- emacs-f-name))
  (define dark-theme
    (if deuteranopia? 'modus-vivendi-deuteranopia 'modus-vivendi))
  (define light-theme
    (if deuteranopia? 'modus-operandi-deuteranopia 'modus-operandi))

  (define (get-home-services config)
    "Return home services related to modus-themes."
    (define mode-line-padding (get-value 'emacs-mode-line-padding config))
    (define header-line-padding (get-value 'emacs-header-line-padding config))
    (define tab-bar-padding (get-value 'emacs-tab-bar-padding config))
    (define theme
      (if dark? dark-theme light-theme))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
          (require 'modus-themes)
          (require 'cl-seq))
        (require ',(symbol-append theme '-theme))
        (eval-when-compile
         (enable-theme ',theme))
        (defgroup rde-modus-themes nil
          "Configuration related to `modus-themes'."
          :group 'rde)
        (defcustom rde-modus-themes-mode-line-padding 1
          "The padding of the mode line."
          :type 'number
          :group 'rde-modus-themes)
        (defcustom rde-modus-themes-tab-bar-padding 1
          "The padding of the tab bar."
          :type 'number
          :group 'rde-modus-themes)
        (defcustom rde-modus-themes-header-line-padding 1
          "The padding of the header line."
          :type 'number
          :group 'rde-modus-themes)
        (defcustom rde-modus-themes-after-enable-theme-hook nil
          "Normal hook run after enabling a theme."
          :type 'hook
          :group 'rde-modus-themes)

        (defun rde-modus-themes-run-after-enable-theme-hook (&rest _args)
          "Run `rde-modus-themes-after-enable-theme-hook'."
          (run-hooks 'rde-modus-themes-after-enable-theme-hook))

        (defun rde-modus-themes-set-custom-faces (&optional _theme)
          "Set faces based on the current theme."
          (interactive)
          (when (modus-themes--current-theme)
            (modus-themes-with-colors
              (custom-set-faces
               `(window-divider ((,c :foreground ,bg-main)))
               `(window-divider-first-pixel ((,c :foreground ,bg-main)))
               `(window-divider-last-pixel ((,c :foreground ,bg-main)))
               `(vertical-border ((,c :foreground ,bg-main)))
               `(tab-bar
                 ((,c :background ,bg-dim
                      :box (:line-width ,rde-modus-themes-tab-bar-padding
                            :color ,bg-dim
                            :style unspecified))))
               `(mode-line
                 ((,c :box (:line-width ,rde-modus-themes-mode-line-padding
                            :color ,bg-mode-line-active))))
               `(mode-line-inactive
                 ((,c :box (:line-width ,rde-modus-themes-mode-line-padding
                            :color ,bg-mode-line-inactive))))
               `(header-line
                 ((,c :box (:line-width ,rde-modus-themes-header-line-padding
                            :color ,bg-dim))))
               `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe
                                          :background ,bg-main)))
               `(git-gutter-fr:deleted ((,c :foreground ,bg-removed-fringe
                                            :background ,bg-main)))
               `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe
                                             :background ,bg-main)))
               `(aw-leading-char-face ((,c :height 1.0
                                           :foreground ,blue-cooler)))))))

        (defun rde-modus-themes--dark-theme-p (&optional theme)
          "Indicate if there is a curently-active dark THEME."
          (if theme
              (eq theme ',light-theme)
              (eq (car custom-enabled-themes) ',dark-theme)))

        (setq rde-modus-themes-header-line-padding ,header-line-padding)
        (setq rde-modus-themes-tab-bar-padding ,tab-bar-padding)
        (setq rde-modus-themes-mode-line-padding ,mode-line-padding)
        (advice-add 'enable-theme
                    :after 'rde-modus-themes-run-after-enable-theme-hook)
        ,@(map (lambda (hook)
                 `(add-hook 'rde-modus-themes-after-enable-theme-hook ',hook))
               (append
                '(rde-modus-themes-set-custom-faces)
                 extra-after-enable-theme-hooks))

        (with-eval-after-load 'rde-keymaps
          (define-key rde-toggle-map (kbd "t") 'modus-themes-toggle))

        (with-eval-after-load 'modus-themes
          (setq modus-themes-common-palette-overrides
                '((border-mode-line-active unspecified)
                  (border-mode-line-inactive unspecified)
                  (fringe unspecified)
                  (fg-line-number-inactive "gray50")
                  (fg-line-number-active fg-main)
                  (bg-line-number-inactive unspecified)
                  (bg-line-number-active unspecified)
                  (bg-region bg-ochre)
                  (fg-region unspecified)
                  ,@extra-modus-themes-overrides))
          ,@(if deuteranopia-red-blue-diffs?
                `((setq modus-operandi-deuteranopia-palette-overrides
                        '((bg-changed         "#ffdfa9")
                          (bg-changed-faint   "#ffefbf")
                          (bg-changed-refine  "#fac090")
                          (bg-changed-fringe  "#d7c20a")
                          (fg-changed         "#553d00")
                          (fg-changed-intense "#655000")

                          (bg-removed         "#ffd8d5")
                          (bg-removed-faint   "#ffe9e9")
                          (bg-removed-refine  "#f3b5af")
                          (bg-removed-fringe  "#d84a4f")
                          (fg-removed         "#8f1313")
                          (fg-removed-intense "#aa2222")))

                  (setq modus-vivendi-deuteranopia-palette-overrides
                        '((bg-changed         "#363300")
                          (bg-changed-faint   "#2a1f00")
                          (bg-changed-refine  "#4a4a00")
                          (bg-changed-fringe  "#8a7a00")
                          (fg-changed         "#efef80")
                          (fg-changed-intense "#c0b05f")

                          (bg-removed         "#4f1119")
                          (bg-removed-faint   "#380a0f")
                          (bg-removed-refine  "#781a1f")
                          (bg-removed-fringe  "#b81a1f")
                          (fg-removed         "#ffbfbf")
                          (fg-removed-intense "#ff9095"))))
                '())
          (setq modus-themes-to-toggle '(,light-theme ,dark-theme))
          (setq modus-themes-italic-constructs t)
          (setq modus-themes-bold-constructs t)
          (setq modus-themes-mixed-fonts t)
          (setq modus-themes-org-blocks 'gray-background)
          ,@(if headings-scaling?
                `((setq modus-themes-headings (quote ((1 . (1.15))
                                                      (2 . (1.1))
                                                      (3 . (1.1))
                                                      (4 . (1.0))
                                                      (5 . (1.0))
                                                      (6 . (1.0))
                                                      (7 . (0.9))
                                                      (8 . (0.9))))))
                '()))
        (load-theme ',theme t))
      #:elisp-packages (list emacs-modus-themes)
      #:summary "Modus Themes extensions"
      #:commentary "Customizations to Modus Themes, the elegant,
highly legible Emacs themes.\

Modus operandi is light, high-contrast, calm, colorblind-friendly.
The light colorschemes are better for productivity according to
various researchs, more eye-friendly and works better with other apps
and media like PDFs, web pages, etc, which are also light by default.
Later here will be a link to rde manual with more in-depth explanation
with references to researches.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-modus-themes)
             (emacs-light-theme . ,light-theme)
             (emacs-dark-theme . ,dark-theme)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-circadian
          #:key
          (emacs-circadian emacs-circadian))
  "Configure the circadian.el Emacs package for theme-switching
based on the time of the day."
  (ensure-pred file-like? emacs-circadian)

  (define emacs-f-name 'circadian)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to circadian."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun rde-circadian--get-geolocation ()
          "Get current location coordinates through Mozilla's Geolocation API."
          (let ((response
                 (ignore-errors
                   (url-retrieve-synchronously
                    "https://location.services.mozilla.com/v1/geolocate?key=geoclue"
                    t))))
            (when response
              (with-current-buffer response
                (goto-char (point-min))
                (re-search-forward (rx bol "\n") nil t)
                (delete-region (point) (point-min))
                (let* ((location (car (cdr (json-parse-string
                                            (buffer-string)
                                            :object-type 'plist))))
                       (latitude (plist-get location :lat))
                       (longitude (plist-get location :lng)))
                  (cons longitude latitude))))))

        (with-eval-after-load 'solar
          (setq calendar-longitude (car (rde-circadian--get-geolocation)))
          (setq calendar-latitude (cdr (rde-circadian--get-geolocation))))
        ,@(if (get-value 'emacs-modus-themes config)
              '((add-hook 'circadian-after-load-theme-hook
                          'rde-modus-themes-set-custom-faces))
              '())
        (with-eval-after-load 'circadian-autoloads
          (setq circadian-themes
                '((:sunrise . ,(get-value 'emacs-light-theme config))
                  (:sunset . ,(get-value 'emacs-dark-theme config))))
          (circadian-setup)))
      #:elisp-packages (list emacs-circadian))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-circadian)))
   (home-services-getter get-home-services)))

;; TODO: Can be useful to have different presets for different
;; environments.  For easier and faster switching.
(define* (feature-emacs-faces
          #:key
          ;; Serif vs Sans-Serif
          ;; <https://geniusee.com/single-blog/font-readability-research-famous-designers-vs-scientists>
          ;; Picked Sans by default, as it works good enough and doesn't look
          ;; too outstanding.
          (use-sans-for-variable-pitch? #t))
  "Configure faces for GNU Emacs."

  (define emacs-f-name 'faces)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'fonts config)
    (define font-monospace (get-value 'font-monospace config))
    (define font-sans      (get-value 'font-sans      config))
    (define font-serif     (get-value 'font-serif     config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load
         'faces
         (let* ((mono-fn ,(font-name font-monospace))
                (sans-fn ,(font-name font-sans))
                (serif-fn ,(font-name font-serif))
                (mono (font-spec
                       :name ,(font-name font-monospace)
                       ;; For some reason pgtk emacs has much smaller
                       ;; font than alacritty with the same size value
                       :size   ,(+ 3 (font-size font-monospace))
                       :weight ',(or (font-weight font-monospace) 'normal)))
                ;; For people coming here years later, only
                ;; face which can contain size or integer
                ;; height is default, everything else should
                ;; set only family or relative height
                ;; (decimal value), the font-spec even
                ;; without height/size shouldn't be used.
                ;; Otherwise text-adjust and other stuff can
                ;; be broken.
                (faces `((default ((t (:font ,mono))))
                         (fixed-pitch ((t (:family ,mono-fn))))
                         (button ((t (:inherit (fixed-pitch)))))
                         (variable-pitch ((t (:family
                                              ,,(if use-sans-for-variable-pitch?
                                                    'sans-fn
                                                    'serif-fn))))))))
           (dolist (face faces)
                   (custom-set-faces face))

           (dolist (face faces)
                   (put (car face) 'saved-face nil)))))
      #:summary "\
Font and face settings"
      #:commentary "\
Values are sourced from feature-fonts."
      #:keywords '(convenience faces))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-which-key
          #:key
          (min-height 1)
          (idle-delay 1.0))
  "Configure which-key. MIN-HEIGHT can be used to adjust the look of which-key
popup, when there are not many items in it, can be easier to look through
available options.  IDLE-DELAY is the amount of seconds to wait for the
which-key buffer to popup."
  (ensure-pred integer? min-height)
  (ensure-pred number? idle-delay)

  (define emacs-f-name 'which-key)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'which-key)
        (setq which-key-min-display-lines ,min-height)
        ;; … takes the space of two characters, which missaligns some popups
        (setq which-key-ellipsis "...")
        (setq which-key-idle-delay ,idle-delay)
        (which-key-mode 1)
        (define-key global-map (kbd "C-h C-k") 'which-key-show-top-level))
      #:summary "\
Tooltips for keychords"
      #:commentary "\
Keybinding for top level chords and small appearance adjustments."
      #:keywords '(convenience faces)
      #:elisp-packages (list emacs-which-key))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-keycast
          #:key
          (turn-on? #f)
          (emacs-keycast emacs-keycast))
  "Show keybindings and related functions as you type.  When TURN-ON?
enable rde-keycast-mode on configure-keycast package load."

  (define emacs-f-name 'keycast)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'keycast))
        (with-eval-after-load
         'keycast
         (require 'moody)
         (setq keycast-mode-line-window-predicate 'moody-window-active-p)
         (setq keycast-mode-line-format "%k%c%r ")
         (add-to-list 'global-mode-string keycast-mode-line))

        (autoload 'keycast--update "keycast")
        ;; <https://github.com/tarsius/keycast/issues/7#issuecomment-627604064>
        (define-minor-mode rde-keycast-mode
          "Show current command and its key binding in the mode line."
          :global t
          (if rde-keycast-mode
              (add-hook 'post-command-hook 'keycast--update t)
              (progn
               (setq keycast--this-command nil)
               (setq keycast--this-command-keys nil)
               (setq keycast--command-repetitions 0)
               (remove-hook 'post-command-hook 'keycast--update))))
        ,@(if turn-on?
              '((rde-keycast-mode 1))
              '())
        (with-eval-after-load 'rde-keymaps
          (define-key rde-toggle-map (kbd "k") 'rde-keycast-mode)
          (define-key rde-toggle-map (kbd "K") 'rde-keycast-mode)))
      #:summary "\
Show commands and keybindings in header-line"
      #:commentary "\
Make `keycast-mode' work with header line instead of modeline, provides
keybindings and adjust some minor settings."
      #:keywords '(convenience)
      #:elisp-packages (list emacs-moody emacs-keycast))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-keycast)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-all-the-icons
          #:key
          (emacs-all-the-icons emacs-all-the-icons)
          (emacs-all-the-icons-completion emacs-all-the-icons-completion))
  "Configure all-the-icons, a collection of fonts for Emacs."
  (ensure-pred file-like? emacs-all-the-icons)
  (ensure-pred file-like? emacs-all-the-icons-completion)

  (define emacs-f-name 'all-the-icons)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to all-the-icons."
    (list
     (let ((emacs-completion (get-value 'emacs-completion config)))
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((with-eval-after-load 'all-the-icons
            (setq all-the-icons-scale-factor 1.0)
            (setq all-the-icons-default-adjust 0)
            (setq all-the-icons-octicon-scale-factor 0.9))
          ,@(if emacs-completion
                '((autoload 'all-the-icons-completion-mode
                            "all-the-icons-completion")
                  (all-the-icons-completion-mode)
                  (add-hook 'marginalia-mode-hook
                            'all-the-icons-completion-marginalia-setup))
                '()))
        #:elisp-packages (append
                          (list emacs-all-the-icons)
                          (if emacs-completion
                              (list emacs-all-the-icons-completion)
                              '()))))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-all-the-icons)))
   (home-services-getter get-home-services)))


;;;
;;; Generic.
;;;

(define --Generic--)

(define* (feature-emacs-input-methods
          #:key
          (toggle-key "s-SPC")
          (enable-reverse-im #f)
          (default-input-method "cyrillic-dvorak")
          (input-method-packages (list emacs-cyrillic-dvorak-im)))
  "Configure input-method for GNU Emacs.  Allows to use other layouts
with emacs, whithout losing ability to use keybindings.  Supported
both Emacsy toggle-input-method (C-\\) and system layout switching by
utilizing reverse-im package."

  (define emacs-f-name 'input-method)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load
         'mule
         (setq-default mode-line-mule-info nil)
         ;; Feels a little hacky, but mule-related hooks are
         ;; inconsistent and cursor not changed back in some cases.
         (add-hook 'post-command-hook
                   '(lambda ()
                      (set-cursor-color
                       (if current-input-method "DarkOrange1" "black"))))

         ,@(map (lambda (x) `(require ',(strip-emacs-name x)))
                input-method-packages)

         (setq default-input-method ,default-input-method)
         (define-key global-map (kbd ,toggle-key) 'toggle-input-method))

        ,@(if enable-reverse-im
              `((with-eval-after-load
                 'reverse-im
                 (setq reverse-im-input-methods ,default-input-method))
                (reverse-im-mode 1))
              '()))
      #:summary "\
Better keyboard layouts handling"
      #:commentary "\
Input methods allows to input characters from different languages and other
character sets, but preserving all keybindings in place using builtin emacs
capabilities by toggling input method instead of system keyboard layout.

Reverse input methods works in a similiar way, but for system keyboard layout,
by mapping characters to default emacs keybindings."
      #:keywords '(convenience faces)
      #:elisp-packages `(,@(if enable-reverse-im (list emacs-reverse-im) '())
                         ,@input-method-packages))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-battery
          #:key
          (battery-format "%b%p%% "))
  "Configure the display of battery status in Emacs."
  (ensure-pred string? battery-format)

  (define emacs-f-name 'battery)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Battery mode."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if (get-value 'emacs-all-the-icons config)
              '((defun rde-battery-add-unicode-indicator (data)
                  "Add a unicode indicator to the battery status."
                  (let* ((bat (car (read-from-string (cdr (assq ?p data)))))
                         (index (cl-position-if
                                 (lambda (e)
                                   (> bat e))
                                 '(75 50 25 10 -1)))
                         (symbol
                          (nth index
                               (list
                                (all-the-icons-faicon "battery-full")
                                (all-the-icons-faicon "battery-three-quarters")
                                (all-the-icons-faicon "battery-half")
                                (all-the-icons-faicon "battery-quarter")
                                (all-the-icons-faicon "battery-empty")))))
                    (setq battery-mode-line-string
                          (format "%s %s" symbol battery-mode-line-string))))
                (add-hook 'battery-update-functions
                          'rde-battery-add-unicode-indicator))
              '())

        (setq battery-mode-line-format ,battery-format)
        (display-battery-mode)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define %rde-default-timezones
  '(("America/Los_Angeles" "Los Angeles")
    ("America/Boise" "Boise")
    ("America/New_York" "New York")
    ("UTC" "UTC")
    ("Europe/London" "London")
    ("Europe/Paris" "Paris")
    ("Europe/Helsinki" "Helsinki")
    ("Europe/Moscow" "Moscow")
    ("Asia/Tbilisi" "Tbilisi")
    ("Asia/Tokyo" "Tokyo")))

(define* (feature-emacs-time
          #:key
          (world-clock-timezones %rde-default-timezones)
          (world-clock-key "C")
          (world-clock-time-format "%A %d %B %R %Z")
          (display-time? #f)
          (display-time-24hr? #f)
          (display-time-date? #f))
  "Configure time.el, an Emacs library to display the time.
Choose the timezones you'll be prompted with upon calling @code{world-clock}
with WORLD-CLOCK-TIMEZONES and change its format with WORLD-CLOCK-TIME-FORMAT
(see @code{format-time-string} for information on the format strings).
If you want to display time on the mode line, set DISPLAY-TIME? to #t, and
accordingly set its appearance with DISPLAY-TIME-24HR? and DISPLAY-TIME-DATE?."
  (ensure-pred maybe-list? world-clock-timezones)
  (ensure-pred string? world-clock-key)
  (ensure-pred string? world-clock-time-format)
  (ensure-pred boolean? display-time?)
  (ensure-pred boolean? display-time-24hr?)
  (ensure-pred boolean? display-time-date?)

  (define emacs-f-name 'time)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define emacs-rde-time-service-type
    (make-home-elisp-service-type 'rde-time))
  (define (get-home-services config)
    "Return home services related to time.el."
    (list
     (simple-service
      'rde-time-extension
      emacs-rde-time-service-type
      (home-elisp-extension
       (config `(,#~";; hello there"
                 (setq something 'huiamthing)
                 ,#~";; unhello there"))))
     (service
      emacs-rde-time-service-type
      (home-elisp-configuration
       (name 'rde-time)
       (config
        `((eval-when-compile
           (require 'time))
          (with-eval-after-load 'rde-keymaps
            (define-key rde-app-map (kbd ,world-clock-key) 'world-clock))
          ,@(if world-clock-timezones
                `((setq world-clock-list ',world-clock-timezones))
                '())
          (setq display-time-world-time-format ,world-clock-time-format)
          (setq display-time-default-load-average nil)
          (setq display-time-load-average-threshold 0)
          ,@(if display-time-date?
                '((setq display-time-day-and-date t))
                '())
          ,@(if display-time-24hr?
                '((setq display-time-24hr-format t))
                '())
          ,@(if display-time?
                '((display-time-mode))
                '())))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-rde-time-service-type . ,emacs-rde-time-service-type)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-calendar
          #:key
          (diary-file "~/docs/diary")
          (calendar-date-style 'iso)
          (week-numbers? #t)
          (calendar-key "c")
          (appt-key "A"))
  "Configure the calendar and diary facilities in Emacs."
  (ensure-pred path? diary-file)
  (ensure-pred symbol? calendar-date-style)
  (ensure-pred boolean? week-numbers?)
  (ensure-pred string? calendar-key)
  (ensure-pred string? appt-key)

  (define emacs-f-name 'calendar)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the Emacs Calendar/Diary."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defvar rde-calendar-appt-map nil
          "Map to bind `appt' commands under.")
        (define-prefix-command 'rde-calendar-appt-map)
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,calendar-key) 'calendar))
        (with-eval-after-load 'calendar
          (setq diary-file ,diary-file)
          (setq calendar-week-start-day 1)
          (setq calendar-view-diary-initially-flag t)
          (setq calendar-date-style ',calendar-date-style)
          (setq calendar-mark-diary-entries-flag t)
          ,@(if week-numbers?
                '((setq calendar-intermonth-header
                        (propertize "WK" 'font-lock-face
                                    'font-lock-function-name-face))
                  (setq calendar-intermonth-text
                        '(propertize
                          (format "%2d"
                                  (car
                                   (calendar-iso-from-absolute
                                    (calendar-absolute-from-gregorian
                                     (list month day year)))))
                          'font-lock-face 'font-lock-function-name-face)))
                '()))
        (appt-activate 1)
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,appt-key) 'rde-calendar-appt-map)
          (let ((map rde-calendar-appt-map))
            (define-key map "a" 'appt-add)
            (define-key map "d" 'appt-delete)))
        (with-eval-after-load 'appt
          (setq appt-display-format 'echo)
          (setq appt-audible nil)
          (setq appt-message-warning-time 10)
          (setq appt-display-interval 2)
          (setq appt-display-diary nil))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-tramp
          #:key
          (default-method "ssh")
          (tramp-key "R"))
  "Configure TRAMP, a remote file editing tool for Emacs."
  (ensure-pred string? default-method)
  (ensure-pred string? tramp-key)

  (define emacs-f-name 'tramp)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'tramp)
         (require 'cl-lib))
        (defvar rde-tramp-map nil
          "Map to bind `tramp' commands under.")
        (define-prefix-command 'rde-tramp-map)

        (defun rde-tramp--parse-sconfig-hosts ()
          "Parse SSH configuration file and return a list of host definitions."
          (when-let ((file (expand-file-name "~/.ssh/config")))
            (with-temp-buffer
              (insert-file-contents-literally file)
              (goto-char (point-min))
              (delete
               nil
               (cl-loop
                while (not (eobp))
                when (re-search-forward
                      (rx bol (* space) "Host" space
                          (group (+ (any "a-z" "A-Z" "0-9" "_.%*" "-"))))
                      (pos-eol) t)
                collect (match-string 1)
                unless (> (skip-chars-forward "\t") 0)
                do (forward-line 1))))))

        (cl-defun rde-tramp-run (command &rest args &key thing &allow-other-keys)
          "Execute COMMAND with ARGS in TRAMP session and manipulate remote THING."
          (let* ((host (completing-read
                        "SSH host: " (rde-tramp--parse-sconfig-hosts)))
                 (read-thing
                  (if thing
                      (pcase thing
                        (:directory (read-directory-name
                                     (format "Directory (%s): " host)
                                     (format "/-:%s:" host)))
                        (:file (read-file-name
                                (format "File (%s): " host)
                                (format "/-:%s:" host))))
                    (format "/-:%s:" host)))
                 (default-directory read-thing))
            (cond
             ((and args thing)
              (apply command read-thing (cddr args)))
             (args (apply command args))
             (t (funcall command)))))

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((autoload 'tramp-list-remote-buffers "tramp-cmd")
                (with-eval-after-load 'tramp
                  (defvar rde-tramp-buffer-source
                    `(:name "Tramp"
                      :narrow ?r
                      :category buffer
                      :state ,'consult--buffer-state
                      :items ,(lambda ()
                                (mapcar 'buffer-name
                                        (tramp-list-remote-buffers))))
                    "Source for TRAMP buffers to be set in \
`consult-buffer-sources'.")
                  (with-eval-after-load 'consult
                    (add-to-list 'consult-buffer-sources
                                 rde-tramp-buffer-source))))
              '())

        (defun rde-tramp-shell (&optional arg)
          "Open a shell buffer inside a TRAMP host with ARG."
          (interactive "P")
          (rde-tramp-run 'shell arg))

        (defun rde-tramp-eshell (&optional arg)
          "Open an eshell buffer inside a TRAMP host with ARG."
          (interactive "P")
          (rde-tramp-run 'eshell arg))

        (defun rde-tramp-dired ()
          "Open a Dired buffer inside a TRAMP host."
          (interactive)
          (rde-tramp-run 'dired :thing :directory))

        (defun rde-tramp-find-file ()
          "Open a file inside a TRAMP host."
          (interactive)
          (rde-tramp-run 'find-file :thing :file))

        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,tramp-key) 'rde-tramp-map))
        (let ((map rde-tramp-map))
          (define-key map "f" 'rde-tramp-find-file)
          (define-key map "d" 'rde-tramp-dired)
          (define-key map "s" 'rde-tramp-shell)
          (define-key map "e" 'rde-tramp-eshell))

        (with-eval-after-load 'tramp
          (setq tramp-verbose 1)
          ,#~";; Should be faster for small files."
          (setq tramp-default-method ,default-method)
          ,#~";; Obtain remote machine's PATH from login shell."
          (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
          ,#~";; Allows to use /sudo:HOST:/path if the user in sudoers."
          (set-default 'tramp-default-proxies-alist
                       '((".*" "\\`root\\'" "/ssh:%h:")))))
      #:summary "\
Transparently accessing remote files from within Emacs."
      #:commentary "\
Various settings for `tramp'.

Make sure tramp works on remote guix machines and allow to use
path /sudo:HOST:/path if the user in sudoers.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-tramp)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-dired
          #:key
          (emacs-all-the-icons-dired emacs-all-the-icons-dired)
          (emacs-dired-rsync emacs-dired-rsync)
          (kill-when-opening-new-buffer? #f)
          (group-directories-first? #f))
  (ensure-pred boolean? kill-when-opening-new-buffer?)
  (ensure-pred boolean? group-directories-first?)
  (ensure-pred file-like? emacs-dired-rsync)

  (define emacs-f-name 'dired)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Dired."
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         "--eval"
         (string-append
          "(dired \"" (car (cdr (command-line))) "\")")))
    (define dired-listing-switches
      (string-join
       (list "-l -h"
             (if (get-value 'emacs-advanced-user? config)
                 "-A --time-style=long-iso"
                 "-a")
             (if group-directories-first?
                 "--group-directories-first"
                 ""))
       " "))
    (define zip (get-value 'zip config (@ (gnu packages compression) zip)))
    (define rsync (get-value 'rsync config (@ (gnu packages rsync) rsync)))
    (define emacs-all-the-icons (get-value 'emacs-all-the-icons config))

    (list
     (simple-service
      'add-dired-cli-utils
      home-profile-service-type
      (list zip rsync))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'dired))

        ,@(if (get-value 'emacs-embark config)
              '((defun rde-dired-open-externally ()
                  "Open marked files in Dired through an external program."
                  (interactive)
                  (let ((files (dired-get-marked-files)))
                    (mapc 'embark-open-externally files)))
                (with-eval-after-load 'dired
                  (define-key dired-mode-map "V" 'rde-dired-open-externally)))
              '())

        (define-key global-map (kbd "s-d") 'dired-jump)
        (with-eval-after-load 'dired
          ,@(if emacs-all-the-icons
                '((add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
                  (with-eval-after-load 'all-the-icons-dired
                    (setq all-the-icons-dired-monochrome nil)))
                '())
          (let ((map dired-mode-map))
            (define-key map (kbd "C-c C-r") 'dired-rsync)
            (define-key map "q" 'kill-current-buffer))
         (setq dired-dwim-target t)
         (setq dired-listing-switches ,dired-listing-switches)
         ,@(if kill-when-opening-new-buffer?
               '((setq dired-kill-when-opening-new-dired-buffer t))
               '())
         ,@(if (get-value 'emacs-advanced-user? config)
               `((add-hook 'dired-mode-hook 'dired-hide-details-mode))
               '())

         (add-hook 'dired-mode-hook 'toggle-truncate-lines)
         (setq dired-hide-details-hide-symlink-targets nil)
         (setq delete-by-moving-to-trash nil)
         (setq dired-recursive-deletes 'always)
         (setq dired-clean-confirm-killing-deleted-buffers nil)
         (setq dired-recursive-copies 'always))

        (with-eval-after-load 'dired-rsync
          (setq dired-rsync-options
                "--exclude .git/ --exclude .gitignore -az --info=progress2 --delete"))
        (with-eval-after-load 'ls-lisp
          (setq ls-lisp-use-insert-directory-program nil)))
      #:elisp-packages (append
                        (list emacs-dired-rsync)
                        (if emacs-all-the-icons
                            (list emacs-all-the-icons-dired)
                            '()))
      #:summary "\
Configurations for emacs built-in file manager"
      #:commentary "\
Small tweaks, xdg entry for openning directories in emacs client."
      #:keywords '(convenience dired files))
     (emacs-xdg-service emacs-f-name "Emacs (Client) [file:]" xdg-gexp
                        #:default-for '(inode/directory))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: Integrate with eat https://codeberg.org/akib/emacs-eat
(define* (feature-emacs-eshell
          #:key
          (emacs-eshell-syntax-highlighting
           emacs-eshell-syntax-highlighting)
          (emacs-eshell-promt-extras emacs-eshell-prompt-extras))
  "Configure Eshell, the Emacs shell."
  (ensure-pred file-like? emacs-eshell-syntax-highlighting)
  (ensure-pred file-like? emacs-eshell-prompt-extras)

  (define emacs-f-name 'eshell)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'eshell)
         (require 'em-alias)
         (require 'em-hist)
         (require 'project))
        (defgroup rde-eshell nil
          "Eshell customizations for a better integration with Emacs tooling."
          :group 'rde)

        (define-minor-mode rde-eshell-mode-setup
          "Set up environment on `eshell-mode' invocation."
          :group 'rde-eshell
          (if rde-eshell-mode-setup
              (progn
                (if (and (boundp 'envrc-global-mode) envrc-global-mode)
                    (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
                    (setenv "PAGER" ""))

                (eshell/alias "e" "find-file $1")
                (eshell/alias "ee" "find-file-other-window $1")
                (eshell/alias "d" "dired $1")
                (with-eval-after-load 'magit
                  (eshell/alias "gd" "magit-diff-unstaged"))

                (define-key eshell-mode-map (kbd "C-c M-o") 'eshell/clear)
                (define-key eshell-mode-map (kbd "s-e")
                            'switch-to-prev-buffer-or-eshell))
              (local-unset-key 'eshell/clear)))

        (defun rde-project-eshell-or-eshell (&optional arg)
          "If there is a project open project-eshell"
          (interactive "P")
          (if (project-current)
              (project-eshell)
              (eshell arg)))

        (define-key global-map (kbd "s-e") 'eshell)
        (add-hook 'eshell-mode-hook 'rde-eshell-mode-setup)
        (with-eval-after-load 'eshell
          (setq eshell-banner-message "")
          (autoload 'eshell-syntax-highlighting-global-mode
                    "eshell-syntax-highlighting")
          (eshell-syntax-highlighting-global-mode)

          ,@(if (get-value 'emacs-consult config)
                '((add-hook
                   'eshell-hist-mode-hook
                   (lambda ()
                     (define-key eshell-hist-mode-map (kbd "M-r")
                       'consult-history))))
                '())

         ;;; <https://www.emacswiki.org/emacs/AnsiColor#h5o-2>
         (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

         (with-eval-after-load 'em-prompt
          (autoload 'epe-theme-lambda "eshell-prompt-extras")
          (setq eshell-prompt-function 'epe-theme-lambda)
          (setq eshell-highlight-prompt nil))

         (defun switch-to-prev-buffer-or-eshell (arg)
           (interactive "P")
           (if arg
               (eshell arg)
               (switch-to-buffer (other-buffer (current-buffer) 1))))))
      #:elisp-packages (list emacs-eshell-syntax-highlighting
                             emacs-eshell-prompt-extras)
      #:summary "\
Eshell configurations, aliases, tweaks"
      #:commentary "\
Aliases, keybindings, small hack and tweaks."
      #:keywords '(convenience eshell))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-calc
          #:key
          (emacs-calc-currency emacs-calc-currency)
          (currency 'EUR)
          (exchange-update-interval 7))
  "Configure Calc, an advanced desk calculator and mathematical tool for Emacs.
You can compute the current exchange rate for your preferred CURRENCY and update
it every EXCHANGE-UPDATE-INTERVAL days."
  (ensure-pred file-like? emacs-calc-currency)
  (ensure-pred symbol? currency)
  (ensure-pred number? exchange-update-interval)

  (define emacs-f-name 'calc)
  (define f-name (symbol-append 'emacs emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Calc."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'calc-currency-autoloads
          (add-hook 'calc-start-hook 'calc-currency-load))
        (with-eval-after-load 'calc-currency
          (require 'xdg)
          (setq calc-currency-exchange-rates-file
                (expand-file-name "emacs/calc-currency-rates.el"
                                  (or (xdg-cache-home) "~/.cache")))
          (setq calc-currency-base-currency ',currency)
          (setq calc-currency-update-interval ,exchange-update-interval)))
      #:elisp-packages (list emacs-calc-currency))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-calc-currency . ,emacs-calc-currency)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-re-builder
          #:key
          (re-syntax 'rx)
          (re-builder-key "r"))
  "Configure re-builder, an Emacs mode to build Regexps with visual feedback.
RE-SYNTAX can be either 'read, 'string, or 'rx."
  (ensure-pred symbol? re-syntax)
  (ensure-pred string? re-builder-key)

  (define emacs-f-name 're-builder)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to re-builder."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,re-builder-key) 're-builder))
        (with-eval-after-load 're-builder
          (setq reb-re-syntax ',re-syntax)
          (setq reb-blink-delay 0)
          (setq reb-auto-match-limit nil))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-comint)
  "Configure the general command-intepreter-in-a-buffer (comint) for
process-in-a-buffer derived packages like shell, REPLs, etc."

  (define emacs-f-name 'comint)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Comint."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar rde-comint-buffer-source
                  `(:name "Comint"
                          :narrow ?c
                          :category buffer
                          :state ,'consult--buffer-state
                          :items ,(lambda ()
                                    (mapcar 'buffer-name
                                            (rde-completion--mode-buffers
                                             'comint-mode))))
                  "Source for `comint-mode' buffers to be set in \
`consult-buffer-sources'.")
                (with-eval-after-load 'consult
                  (add-to-list 'consult-buffer-sources
                               rde-comint-buffer-source))
                (with-eval-after-load 'rde-completion
                  (add-to-list 'rde-completion-initial-narrow-alist
                               '(comint-mode . ?c))))
              '())
        (add-hook 'comint-preoutput-filter-functions 'ansi-color-apply nil t)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-shell)
  "Configure shell-scripting tooling for Emacs."

  (define emacs-f-name 'shell)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to shell-scripting in Emacs."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'sh-script
          (setq sh-basic-offset 2)
          (setq sh-indentation 2)
          (setq sh-indent-comment nil)
          (setq sh-first-lines-indent nil))
        (add-to-list 'display-buffer-alist
                     `(,(rx "*Async Shell Command" (* any) "*")
                       (display-buffer-no-window)))
        ,@(if (get-value 'emacs-org config)
              '((with-eval-after-load 'org
                  (add-to-list 'org-structure-template-alist
                               '("sh" . "src sh")))
                (with-eval-after-load 'ob-core
                  (require 'ob-shell)))
              '())
        ,@(if (get-value 'emacs-project config)
              '((with-eval-after-load 'project
                  (define-key project-prefix-map "s" 'project-shell)
                  (add-to-list 'project-switch-commands
                               '(project-shell "Start an inferior shell"))))
              '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-browse-url
          #:key
          (extra-url-mappings '()))
  "Configure and extend the browse-url library to enhance the handling of URLs
to browsers in Emacs.  You can set URL mappings to rewrite URLs in Emacs
buffers, open sites with cookies, make sure URLs use HTTPS, among other."
  (ensure-pred elisp-config? extra-url-mappings)

  (define emacs-f-name 'browse-url)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'cl-lib))
        (defgroup rde-browse-url nil
          "Generic utilities to enhance `browse-url'."
          :group 'rde)
        (defcustom rde-browse-url-mappings '()
          "URL mapping alist.
It has the form (SERVICE . ALT) where SERVICE is the original hostname of
the service and ALT is the alternative service host to rewrite urls to, and
viceversa."
          :type 'list
          :group 'rde-browse-url)

        (cl-defun rde-browse-url--transform-url (url &key (alt t))
          "Transform URL to its alternative in `rde-browse-url-mappings'.
If ALT is non-nil, URL is assumed to be an alternative so the logic is reversed."
          (string-match (rx (group (+ any) "://" (* (not "/"))) (* any)) url)
          (if-let* ((service-url (match-string 1 url))
                    (mapping (if alt
                                 (cl-rassoc service-url rde-browse-url-mappings
                                            :test 'string=)
                               (assoc-string service-url
                                             rde-browse-url-mappings))))
              (if alt
                  (replace-regexp-in-string
                   service-url
                   (car mapping)
                   url)
                (replace-regexp-in-string
                 service-url
                 (cdr mapping)
                 url))
            url))

        (defun rde-browse-url-bookmark-make-record (url title)
          "Create a bookmark record from a browser buffer with URL and TITLE."
          (let* ((defaults (delq nil (list title url)))
                 (bookmark
                  `(,title
                    ,@(bookmark-make-record-default 'no-file)
                    ,(cons 'browser-url url)
                    ,(cons 'filename url)
                    ,(cons 'handler 'rde-browse-url-bookmark-jump)
                    ,(cons 'defaults defaults))))
            bookmark))

        (defun rde-browse-url-bookmark-jump (bookmark)
          "Jump to BOOKMARK in the default browser."
          (let ((location (bookmark-prop-get bookmark 'browser-url)))
            (browse-url-default-browser location)))

        (defun rde-browse-url-alt-bookmark-jump (bookmark)
          "Jump to BOOKMARK in an alternative browser."
          (cl-letf (((symbol-function 'browse-url-can-use-xdg-open) 'ignore))
            (rde-browse-url-bookmark-jump bookmark)))

        ,@(if (get-value 'emacs-embark config)
              '((defun rde-browse-url-open-with-cookies (cookies &optional url)
                  "Open URL with COOKIES in corresponding external application."
                  (interactive "\nsURL: ")
                  (let ((url-request-extra-headers
                         `(("Cookie"
                            ,(cl-loop for (field cookie) in cookies
                                      collect (format " %s=%s;" field cookie)
                                      into headers
                                      finally (return (string-join headers))))))
                        (filename (concat temporary-file-directory
                                          (car (last (split-string url "/"))))))
                    (unless (file-exists-p filename)
                      (with-current-buffer
                          (url-retrieve-synchronously url t)
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (forward-line 1)
                        (delete-region (point) (point-min))
                        (write-region (point-min) (point-max) filename)))
                    (embark-open-externally filename)))

                (with-eval-after-load 'embark
                  (define-key embark-bookmark-map "c"
                              'rde-browse-url-alt-bookmark-jump)))
              '())

        (defun rde-browse-url-add-scheme (fun url &rest args)
          "Add https scheme to URL if missing and invoke FUN and ARGS with it."
          (let ((link (if (string-match (rx bol (+ (in (?A . ?Z))) ":") url)
                          url
                        (concat "https:" url))))
            (apply fun link args)))

        (defun rde-browse-url-trace-url (fun url &rest args)
          "Transform URL to its original form and invoke FUN and ARGS with it."
          (let ((link (rde-browse-url--transform-url url)))
            (apply fun link args)))

        (setq rde-browse-url-mappings
              (append
               (list
                ,@(if (get-value 'youtube-frontend config)
                      `((cons "https://www.youtube.com"
                              ,(get-value 'youtube-frontend config)))
                      '())
                ,@(if (get-value 'reddit-frontend config)
                      `((cons "https://www.reddit.com"
                              ,(get-value 'reddit-frontend config)))
                      '())
                ,@(if (get-value 'quora-frontend config)
                      `((cons "https://quora.com"
                              ,(get-value 'quora-frontend config)))
                      '())
                ,@(if (get-value 'twitter-frontend config)
                      `((cons "https://twitter.com"
                              ,(get-value 'twitter-frontend config)))
                      '())
                ,@(if (get-value 'imgur-frontend config)
                      `((cons "https://imgur.com"
                              ,(get-value 'imgur-frontend config)))
                      '())
                ,@(if (get-value 'google-frontend config)
                      `((cons "https://www.google.com"
                              ,(get-value 'google-frontend config)))
                      '())
                ,@(if (get-value 'medium-frontend config)
                      `((cons "https://medium.com"
                              ,(get-value 'medium-frontend config)))
                      '()))
               ',extra-url-mappings))
        (advice-add 'browse-url-xdg-open :around 'rde-browse-url-add-scheme)
        (with-eval-after-load 'browse-url
          (setq browse-url-browser-function 'browse-url-xdg-open))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-tab-bar
          #:key
          (modules-left '())
          (modules-center '())
          (modules-right '())
          (tab-bar-format '(rde-tab-bar-format-left
                            rde-tab-bar-format-center
                            rde-tab-bar-format-right)))
  "Configure the Emacs Tab Bar.  Add the appropriate formatters via
TAB-BAR-FORMAT.  The default ones allow you to place \"modules\" (i.e. menu
items constructed by the helper @code{make-rde-tab-bar-module}) arbitrarily
on each side of the bar, but you can also include built-in formatters such as
@code{tab-bar-format-tabs}.

The examples below show different types of modules:

@lisp
(make-rde-tab-bar-module
 :id 'text
 :label \"My arbitrary text\")
(make-rde-tab-bar-module
 :id 'battery
 :label 'battery-mode-line-string)
(make-rde-tab-bar-module
 :id 'notifications
 :label '(:eval (rde-ednc--notify)))
@end lisp"
  (ensure-pred elisp-config? modules-left)
  (ensure-pred elisp-config? modules-center)
  (ensure-pred elisp-config? modules-right)
  (ensure-pred list? tab-bar-format)

  (define emacs-f-name 'tab-bar)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the Emacs Tab Bar."
    (define emacs-all-the-icons (get-value 'emacs-all-the-icons config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'cl-lib))
        (defgroup rde-tab-bar nil
          "Configure the tab bar via menu items."
          :group 'rde)
        (cl-defstruct rde-tab-bar-module id label help action)
        (defcustom rde-tab-bar-modules-left '()
          "List of modules on the left-hand side of the tab bar."
          :type '(repeat rde-tab-bar-module)
          :group 'rde-tab-bar)

        (defcustom rde-tab-bar-modules-center '()
          "List of modules in the center of the tab bar."
          :type '(repeat rde-tab-bar-module)
          :group 'rde-tab-bar)

        (defcustom rde-tab-bar-modules-right '()
          "List of modules on the right-hand side of the tab bar."
          :type '(repeat rde-tab-bar-module)
          :group 'rde-tab-bar)

        (defun rde-tab-bar-build-formatter (modules)
          "Build a tab bar formatter with MODULES."
          (mapcar (lambda (item)
                    (if (rde-tab-bar-module-p item)
                        (let ((label (rde-tab-bar-module-label item)))
                          `(,(rde-tab-bar-module-id item) menu-item
                            ,(cond
                              ((symbolp label)
                               `(when (boundp ',label)
                                  ,label))
                              ((and (listp label) (equal (car label) :eval))
                               `(format-mode-line ',label))
                              (t label))
                            ,(or (rde-tab-bar-module-action item) 'nil)
                            ,@(when (rde-tab-bar-module-help item)
                                `(:help ,(rde-tab-bar-module-help item)))))
                      item))
                  modules))

        (defun rde-tab-bar-format-left ()
          "Return modules for the left-hand side of the tab bar."
          (rde-tab-bar-build-formatter rde-tab-bar-modules-left))

        (defun rde-tab-bar-format-center ()
          "Return modules for the center of the tab bar."
          (let* ((modules (mapconcat
                           (lambda (module)
                             (let ((label (rde-tab-bar-module-label module)))
                               (if (symbolp label)
                                   (symbol-value label)
                                 label)))
                           rde-tab-bar-modules-center ""))
                 (str (concat (propertize
                               " " 'display
                               `(space :align-to
                                       (- center
                                          ,(/ (length modules) 2.0)))))))
            (cons
             `(align-center menu-item ,str nil)
             (rde-tab-bar-build-formatter rde-tab-bar-modules-center))))

        (defun rde-tab-bar-format-right ()
          "Return modules for the right-hand side of the tab bar."
          (let* ((labels (mapconcat
                          (lambda (module)
                            (let ((label (rde-tab-bar-module-label module)))
                              (if (symbolp label)
                                  (symbol-value label)
                                label)))
                          rde-tab-bar-modules-right ""))
                 (n-icons (cl-loop with nprops = 1
                                   for i from 0 to (- (length labels) 1)
                                   when (get-text-property
                                         i 'rear-nonsticky labels)
                                   do (cl-incf nprops)
                                   finally (cl-return nprops)))
                 (hpos (+ (length labels) n-icons))
                 (str (propertize " " 'display
                                  `(space :align-to (- right ,hpos)))))
            (cons
             `(align-right menu-item ,str nil)
             (rde-tab-bar-build-formatter rde-tab-bar-modules-right))))

        (tab-bar-mode)
        ,@(if emacs-all-the-icons
              `((eval-when-compile
                 (require 'all-the-icons))
                (setq rde-tab-bar-modules-left (list ,@modules-left))
                (setq rde-tab-bar-modules-center (list ,@modules-center))
                (setq rde-tab-bar-modules-right (list ,@modules-right)))
              '())
        (with-eval-after-load 'tab-bar
          (setq tab-bar-format ',tab-bar-format)
          (setq tab-bar-border nil)
          (setq tab-bar-close-button-show nil)
          (setq tab-bar-show t)))
      #:elisp-packages (or (and=> emacs-all-the-icons list) '())
      #:summary "Extensions to Emacs's Tab Bar"
      #:commentary "Provide extensions to the Emacs Tab Bar, allowing you to \
supply custom menu items in the form of modules.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; Completion.
;;;

(define --Completion--)

(define (consult-initial-narrowing _)
  `((defcustom rde-completion-initial-narrow-alist '()
      "Alist of MODE . KEY to present an initial completion narrowing via
 `consult'."
      :group 'rde-completion
      :type 'list)

    (defun rde-completion--mode-buffers (&rest modes)
      "Return a list of buffers that are derived from MODES in `buffer-list'."
      (cl-remove-if-not
       (lambda (buffer)
         (with-current-buffer buffer
                              (cl-some 'derived-mode-p modes)))
       (buffer-list)))

    (defun rde-completion-initial-narrow ()
      "Set initial narrow source for buffers under a specific mode."
      (let* ((buffer-mode-assoc rde-completion-initial-narrow-alist)
             (key (and (eq this-command 'consult-buffer)
                       (or (alist-get
                            (buffer-local-value
                             'major-mode
                             (window-buffer (minibuffer-selected-window)))
                                      buffer-mode-assoc)
                           (cdr (cl-find-if
                                 (lambda (mode)
                                   (with-current-buffer
                                    (window-buffer (minibuffer-selected-window))
                                    (derived-mode-p (car mode))))
                                 buffer-mode-assoc))))))
        (when key
          (setq unread-command-events
                (append unread-command-events (list key 32))))))
    (add-hook 'minibuffer-setup-hook 'rde-completion-initial-narrow)))

(define* (feature-emacs-completion
          #:key
          (mini-frame? #f)
          (marginalia-align 'left)
          (consult-initial-narrowing? #t)
          (emacs-orderless emacs-orderless)
          (emacs-cape emacs-cape)
          (emacs-consult emacs-consult)
          (emacs-embark emacs-embark)
          (emacs-marginalia emacs-marginalia))
  "Configure completion system for GNU Emacs."

  (define (marginalia-align? marginalia-align)
    (memq marginalia-align '(left right)))
  (ensure-pred marginalia-align? marginalia-align)
  (ensure-pred boolean? consult-initial-narrowing?)
  (ensure-pred file-like? emacs-orderless)
  (ensure-pred file-like? emacs-cape)
  (ensure-pred file-like? emacs-consult)
  (ensure-pred file-like? emacs-embark)
  (ensure-pred file-like? emacs-marginalia)

  (define emacs-f-name 'completion)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define ripgrep (get-value 'ripgrep config
                               (@ (gnu packages rust-apps) ripgrep)))
    (define fd (get-value 'fd config
                          (@ (gnu packages rust-apps) fd)))
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'marginalia)
         (require 'consult))
        (defgroup rde-completion nil
          "Tweaks to the built-in Emacs completion."
          :group 'rde)

        (with-eval-after-load
         'minibuffer

         ,#~"\n;; It's a little easier to press C-i than C-M-i"
         (setq tab-always-indent 'complete)

         (setq minibuffer-prompt-properties
               '(read-only t cursor-intangible t face minibuffer-prompt))
         (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)
         (setq completion-show-help nil)
         (setq completions-format 'one-column)
         (setq completions-header-format nil)

         (let ((map minibuffer-mode-map))
           (define-key map (vector 'remap 'next-line)
             'minibuffer-next-completion)
           (define-key map (vector 'remap 'previous-line)
             'minibuffer-previous-completion))
         (let ((map completion-in-region-mode-map))
           (define-key map (kbd "C-n") 'minibuffer-next-completion)
           (define-key map (kbd "C-p") 'minibuffer-previous-completion))

         ;; Shows hidden prefix when completing candidates with partial style
         ;; (setq file-name-shadow-properties
         ;;       '(invisible t intangible t face file-name-shadow field shadow))

         ;; Will work if vertico is available, won't affect if it doesn't
         (add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

         ;; (advice-add 'completing-read-multiple
         ;;             :override 'consult-completing-read-multiple)

         ,#~"\n;; Needed for orderless in default completion UI."
         (let ((map minibuffer-local-completion-map))
           (define-key map (kbd "SPC") nil)
           (define-key map (kbd "?") nil))

         ,#~"\n;; Allows to use \\SPC instead of \\s-"
         (setq orderless-component-separator
               'orderless-escapable-split-on-space)

         (defun rde-orderless-literal-dispatcher (pattern _index _total)
           "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
           (cond
            ((equal "=" pattern)
             '(orderless-literal . "="))
            ((string-suffix-p "=" pattern)
             (cons 'orderless-literal (substring pattern 0 -1)))))

         (defun rde-orderless-without-literal-dispatcher (pattern _index _total)
           "Literal without style dispatcher using the exclamation mark as a
suffix.  It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
           (cond
            ((equal "!" pattern)
             '(orderless-literal . "!"))
            ((string-suffix-p "!" pattern)
             (cons 'orderless-without-literal (substring pattern 0 -1)))))

         (defun rde-orderless-initialism-dispatcher (pattern _index _total)
           "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
           (cond
            ((equal "," pattern)
             '(orderless-literal . ","))
            ((string-suffix-p "," pattern)
             (cons 'orderless-initialism (substring pattern 0 -1)))))

         (defun rde-orderless-flex-dispatcher (pattern _index _total)
           "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
           (cond
            ((equal "~" pattern)
             '(orderless-literal . "~"))
            ((string-suffix-p "~" pattern)
             (cons 'orderless-flex (substring pattern 0 -1)))))

         ,@(if consult-initial-narrowing?
               (consult-initial-narrowing config)
              '())

         (setq orderless-style-dispatchers
          '(rde-orderless-literal-dispatcher
            rde-orderless-without-literal-dispatcher
            rde-orderless-initialism-dispatcher
            rde-orderless-flex-dispatcher))


         (setq completion-styles '(orderless basic))
         ;; (setq completion-category-defaults nil)
         (setq completion-category-overrides
               ;; basic is required for /ssh: completion to work, but
               ;; keep the same values for project-file too.
               '((project-file (styles . (orderless partial-completion basic)))
                 (file (styles . (orderless partial-completion basic)))))
         (setq enable-recursive-minibuffers t)

         ;; (setq resize-mini-windows nil)

         ;; MAYBE: Make transient use child-frame:
         ;; https://github.com/magit/transient/issues/102
         ,@(if mini-frame?
               `((with-eval-after-load
                  'mini-frame
                  (custom-set-faces
                   '(child-frame-border
                     ;; TODO: inherit ,(face-attribute 'default :foreground)
                     ((t (:background "#000000")))))
                  (put 'child-frame-border 'saved-face nil)

                  (setq
                   mini-frame-show-parameters
                   `((top . 0.2)
                     (width . 0.8)
                     (left . 0.5)
                     (child-frame-border-width . 1)))
                  (setq mini-frame-detach-on-hide nil)
                  (setq mini-frame-color-shift-step 0)
                  (setq mini-frame-advice-functions
                        '(read-from-minibuffer
                          read-key-sequence
                          save-some-buffers yes-or-no-p))
                  (setq mini-frame-ignore-commands '()))

                 (autoload 'mini-frame-mode "mini-frame")
                 (if after-init-time
                     (mini-frame-mode 1)
                     (add-hook 'after-init-hook 'mini-frame-mode)))
             '()))

        (setq history-length 10000)
        (setq
         savehist-file
         (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                "/emacs/history"))

        ;; (savehist-mode 1)
        ;; (run-with-idle-timer 30 t 'savehist-save)

        (define-key global-map (kbd "s-.") 'embark-act)
        (define-key global-map (kbd "s->") 'embark-become)


        (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
        (define-key global-map (kbd "M-y") 'consult-yank-pop)
        (define-key global-map (kbd "s-B") 'consult-buffer)
        (define-key global-map (kbd "C-x C-r") 'consult-recent-file)
        (define-key minibuffer-local-map (kbd "s-g") 'embark-become)
        ;; (define-key global-map (kbd "M-.") 'embark-dwim)

        (let ((map goto-map))
          (define-key map (kbd "g") 'consult-goto-line)
          (define-key map (kbd "M-g") 'consult-goto-line)
          (define-key map (kbd "l") 'consult-line)
          (define-key map (kbd "o") 'consult-outline)
          (define-key map (kbd "i") 'consult-imenu)
          (define-key map (kbd "m") 'consult-mark)
          (define-key map (kbd "M") 'consult-global-mark)
          (define-key map (kbd "b") 'consult-bookmark))

        (defun rde-goto-line-relative ()
          "Just a wrapper around `consult-goto-line', which uses
relative line numbers, when narrowing is active."
          (interactive)
          (let ((consult-line-numbers-widen nil))
            (call-interactively 'consult-goto-line)))

        (define-key narrow-map (kbd "g") 'rde-goto-line-relative)

        (let ((map search-map))
          (define-key map (kbd "f") 'consult-find)
          (define-key map (kbd "g") 'consult-ripgrep)
          (define-key map (kbd "e") 'consult-isearch)
          (define-key map (kbd "l") 'consult-line))
        ;; (define-key global-map (kbd "C-S-s") 'consult-line)

        (autoload 'consult-isearch-history "consult")
        (let ((map isearch-mode-map))
          (define-key map (kbd "M-e") 'consult-isearch-history)
          (define-key map (kbd "M-s e") 'consult-isearch-history)
          (define-key map (kbd "M-s l") 'consult-line))
        ;; (define-key isearch-mode-map (kbd "C-S-s") 'consult-line)

        ;; MAYBE: Share this keybinding with switch-to-buffer?
        (define-key minibuffer-local-map (kbd "s-b") 'exit-minibuffer)

        (autoload 'consult-customize "consult" "" nil 'macro)
        (autoload 'consult--customize-set "consult")

        (autoload 'embark-open-externally "embark")
        (with-eval-after-load
         'embark
         (require 'embark-consult))

        (with-eval-after-load
            'xref
          (setq xref-show-xrefs-function 'consult-xref))

        (with-eval-after-load
         'consult
         (require 'embark-consult)

         (setq consult-ripgrep-args
               (replace-regexp-in-string "^rg" ,(file-append ripgrep "/bin/rg")
                                         consult-ripgrep-args))
         (setq consult-find-args
               (replace-regexp-in-string "^find" ,(file-append fd "/bin/fd")
                                         consult-find-args))
         (consult-customize consult-buffer :preview-key "M-.")
         (consult-customize consult-history :category 'consult-history)
         (consult-customize consult-line :inherit-input-method t))

        (with-eval-after-load
         'marginalia
         (setq marginalia-align ',marginalia-align))

        (autoload 'marginalia-mode "marginalia")
        (marginalia-mode 1))
      #:summary "\
General settings related to completion"
      #:commentary "\
Different commands providing various useful lists of candidates and
alternatives to builtins.

Matching rules configurations for filtering candidates, using orderless
package and builtin emacs capabilities.

Actions on candidate or list of candidates using embark.

Annotations for completion candidates using marginalia."
      #:keywords '(convenience completion)
      #:elisp-packages
      (append
       (if mini-frame?
           (list emacs-mini-frame)
           '())
       (list emacs-orderless emacs-marginalia
             emacs-pcmpl-args emacs-cape
             emacs-consult emacs-embark)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-cape . ,emacs-cape)
             (emacs-embark . ,emacs-embark)
             (emacs-consult . ,emacs-consult)
             (emacs-consult-initial-narrowing? . ,consult-initial-narrowing?)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-vertico
          #:key
          (emacs-vertico emacs-vertico)
          (completion-in-region? #t))
  "Configure vertico completion UI for GNU Emacs."
  (ensure-pred file-like? emacs-vertico)
  (ensure-pred boolean? completion-in-region?)

  (define emacs-f-name 'vertico)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'vertico)
         (require 'vertico-multiform))
        ,@(if (and (get-value 'emacs-consult config) completion-in-region?)
              '((with-eval-after-load
                 'minibuffer
                 (setq completion-in-region-function
                       (lambda (&rest args)
                         (apply (if vertico-mode
                                    'consult-completion-in-region
                                    'completion--in-region)
                                args)))))
                '())

        (with-eval-after-load
         'vertico

         (advice-add
          'vertico--format-candidate :around
          (lambda (orig cand prefix suffix index _start)
            (let ((cand (funcall orig cand prefix suffix index _start)))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                   "  ")
               cand))))

         (define-key global-map (kbd "s-s") 'vertico-repeat)
         ;; TODO: Bind vertico-next/previous-group to more usual keys?

         (require 'vertico-repeat)
         (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
         (setq vertico-cycle t)

         ;; (defvar rde--vertico-monocle-previous-window-configuration nil
         ;;   "Window configuration for restoring on vertico monocle exit.")

         ;; (defun rde-vertico-toggle-monocle ()
         ;;   "Zoom in/out completion list."
         ;;   (interactive)
         ;;   (require 'vertico-buffer)
         ;;   (if (and rde--vertico-monocle-previous-window-configuration
         ;;            vertico-buffer--window)
         ;;       (if rde--vertico-monocle-previous-window-configuration
         ;;           (let ((cur-buffer (current-buffer)))
         ;;             (set-window-configuration
         ;;              rde--vertico-monocle-previous-window-configuration)
         ;;             (setq rde--vertico-monocle-previous-window-configuration nil)
         ;;             (switch-to-buffer cur-buffer)))
         ;;       (unless vertico-buffer--window
         ;;         (vertico-buffer--setup))
         ;;       (setq rde--vertico-monocle-previous-window-configuration
         ;;             (current-window-configuration))
         ;;       (with-selected-window vertico-buffer--window
         ;;                             (delete-other-windows))))

         ;; (keymap-set vertico-map "<remap> <rde-toggle-monocle>"
         ;;             'rde-vertico-toggle-monocle))

         (require 'vertico-directory)
         (defun rde-vertico-kill-region-dwim (&optional count)
           "The function kills region if mark is active, otherwise
calls `vertico-directory-delete-word'.  Prefix argument can be used to
kill a few words or directories."
           (interactive "p")
           (if (use-region-p)
               (kill-region (region-beginning) (region-end) 'region)
               (vertico-directory-delete-word count)))
         (define-key vertico-map (kbd "C-w") 'rde-vertico-kill-region-dwim)

         ,@(if (get-value 'emacs-header-line-as-mode-line? config)
               `((defun rde--vertico-prepare-header-line ()
                   "The same as `rde--move-mode-line-to-header', but also increase
vertico-count by 1 to show one more candidate, which is hidden
otherwise because mode line is expected to be present by height
calculation function for vertico buffer."
                   (setq-local header-line-format mode-line-format)
                   (setq-local mode-line-format nil))

                 (advice-add 'vertico--setup :after
                             'rde--vertico-prepare-header-line))
               '())

         ;; TODO: Need to be more specific not to pollute histories.
         ;; (defadvice vertico-insert
         ;;   (after vertico-insert-add-history activate)
         ;;   "Make vertico-insert add to the minibuffer history."
         ;;   (unless (eq minibuffer-history-variable t)
         ;;     (add-to-history minibuffer-history-variable (minibuffer-contents))))

         (setq vertico-multiform-categories
               '((consult-grep buffer)
                 (imenu buffer)
                 (buffer)
                 ;; (file buffer)
                 ;; (project-file buffer)
                 (info-menu buffer)
                 (consult-org-heading buffer)
                 (consult-history buffer)
                 (consult-lsp-symbols buffer)
                 (consult-xref buffer)
                 (embark-keybinding buffer)
                 (consult-location buffer)))

         (setq vertico-multiform-commands
               '((telega-chat-with buffer)
                 (magit:--author flat)
                 ;; For some reason it doesn't have an info-menu
                 ;; category and also setting
                 ;; marginalia-command-categories doesn't help
                 ;; (org-roam-node-find buffer)
                 (Info-goto-node buffer)
                 (info-lookup-symbol buffer)
                 (Info-follow-reference buffer)
                 (consult-yank-pop buffer)))

         (autoload 'vertico-multiform-mode "vertico-multiform")
         (vertico-multiform-mode))

        (autoload 'vertico-mode "vertico")
        (if after-init-time
            (vertico-mode 1)
            (add-hook 'after-init-hook 'vertico-mode)))
      #:summary "\
Flexible minibuffer completion interface"
      #:commentary "\
Some completions open in a buffer on the side to make a list of candidates
higher and easier to navigate."
      #:keywords '(convenience completion)
      #:elisp-packages (list emacs-vertico))))

  (feature
   (name f-name)
   (values (make-feature-values emacs-vertico))
   (home-services-getter get-home-services)))


(define* (feature-emacs-mct
          #:key
          (emacs-mct emacs-mct))
  "Configure mct completion UI for GNU Emacs."
  (define emacs-f-name 'mct)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'mct))
        (with-eval-after-load
         'mct
         (setq mct-live-update-delay 0)
         (setq mct-minimum-input 3)

         (let ((map mct-minibuffer-local-completion-map))
           ;; (define-key map (kbd "<RET>") 'mct-complete-and-exit)
           (define-key map (kbd "<tab>") 'minibuffer-force-complete)
           (define-key map (kbd "C-v") 'switch-to-completions))

         (defvar rde-completion-categories-other-window
           '(imenu)
           "Completion categories that has to be in other window than
current, otherwise preview functionallity will fail the party.")

         (defvar rde-completion-categories-not-show-candidates-on-setup
           '(command variable function)
           "Completion categories that has to be in other window than
current, otherwise preview functionallity will fail the party.")

         (defun rde-display-mct-buffer-pop-up-if-apropriate (buffer alist)
           "Call `display-buffer-pop-up-window' if the completion category
one of `rde-completion-categories-other-window', it will make
sure that we don't use same window for completions, which should
be in separate window."
           (if (memq (mct--completion-category)
                     rde-completion-categories-other-window)
               (display-buffer-pop-up-window buffer alist)
               nil))

         (defun rde-display-mct-buffer-apropriate-window (buffer alist)
           "Displays completion buffer in the same window, where completion
was initiated (most recent one), but in case, when compeltion
buffer should be displayed in other window use least recent one."
           (let* ((window (if (memq (mct--completion-category)
                                    rde-completion-categories-other-window)
                              (get-lru-window (selected-frame) nil nil)
                              (get-mru-window (selected-frame) nil nil))))
             (window--display-buffer buffer window 'reuse alist)))

         (setq mct-display-buffer-action
               (quote ((display-buffer-reuse-window
                        rde-display-mct-buffer-pop-up-if-apropriate
                        rde-display-mct-buffer-apropriate-window))))

         (defun rde-mct-show-completions ()
           "Instantly shows completion candidates for categories listed in
`rde-completion-categories-show-candidates-on-setup'."
           (unless (memq (mct--completion-category)
                         rde-completion-categories-not-show-candidates-on-setup)
             (setq-local mct-minimum-input 0)
             (mct--live-completions)))

         ;; (add-hook 'minibuffer-setup-hook 'rde-mct-show-completions)
         ,@(if (get-value 'emacs-consult config)
               `((autoload 'consult-preview-at-point-mode "consult")
                 (setq rde-completion-categories-other-window
                       (append
                        '(consult-location
                          consult-grep consult-yank
                          consult-history consult-completion)
                        rde-completion-categories-other-window))
                 (add-hook 'completion-list-mode-hook
                           'consult-preview-at-point-mode))
               '()))
        (add-hook 'after-init-hook 'mct-minibuffer-mode))
      #:summary "\
Alternative minibuffer completion interface using buffers."
      #:commentary "\
This configuration packages is not actively maintained right now."
      #:keywords '(convenience completion)
      #:elisp-packages (list emacs-mct
                             (get-value 'emacs-consult config emacs-consult)))))

  (feature
   (name f-name)
   (values (make-feature-values emacs-mct))
   (home-services-getter get-home-services)))


(define* (feature-emacs-corfu
          #:key
          (emacs-corfu emacs-corfu)
          (emacs-corfu-doc emacs-corfu-doc)
          (turn-on? #t)
          (corfu-auto #t)
          (corfu-doc-auto #f))
  "Configure corfu completion UI for GNU Emacs."
  (ensure-pred file-like? emacs-corfu)
  (ensure-pred file-like? emacs-corfu-doc)
  (ensure-pred boolean? turn-on?)
  (ensure-pred boolean? corfu-auto)
  (ensure-pred boolean? corfu-doc-auto)

  (define emacs-f-name 'corfu)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'corfu))

        (with-eval-after-load
         'corfu
         (setq corfu-min-width 60)
         (setq corfu-cycle t)

         (setq corfu-auto ,(if corfu-auto 't 'nil))
         ;; '((define-key corfu-map (kbd "SPC") 'corfu-insert-separator))

         ;; (define-key corfu-map (kbd "C-e") 'corfu-insert)
         (defun corfu-move-to-minibuffer ()
           (interactive)
           (let ((completion-extra-properties corfu--extra)
                 completion-cycle-threshold completion-cycling)
             (apply 'consult-completion-in-region completion-in-region--data)))
         (define-key corfu-map (kbd "M-m") 'corfu-move-to-minibuffer)

         (defun corfu-enable-in-minibuffer ()
           "Enable Corfu in the minibuffer if `completion-at-point' is bound."
           (when (where-is-internal 'completion-at-point
                                    (list (current-local-map)))
             (corfu-mode 1)))
         (add-hook 'minibuffer-setup-hook 'corfu-enable-in-minibuffer)

         (setq corfu-doc-auto ,(if corfu-doc-auto 't 'nil))

         ;; (define-key corfu-map (kbd "M-n") 'corfu-doc-scroll-up)
         ;; (define-key corfu-map (kbd "M-p") 'corfu-doc-scroll-down)
         (define-key corfu-map (kbd "M-D") 'corfu-doc-toggle)

         (add-hook 'corfu-mode-hook 'corfu-doc-mode))

        (autoload 'global-corfu-mode "corfu")
        ;; FIXME: Fix override of vertico completion in region.
        ,@(if turn-on?
              '((if after-init-time
                    (global-corfu-mode 1)
                    (add-hook 'after-init-hook 'global-corfu-mode)))
              '()))
      #:summary "\
Flexible in-buffer (overlay) completion interface"
      #:commentary "\
It shows `completion-at-point' candidates in overlay frame."
      #:keywords '(convenience completion)
      #:elisp-packages (list (get-value 'emacs-consult config emacs-consult)
                             emacs-corfu emacs-corfu-doc))))

  (feature
   (name f-name)
   (values (append
            `((emacs-corfu . ,emacs-corfu)
              (emacs-completion-at-point? . #t))))
   (home-services-getter get-home-services)))


(define* (feature-emacs-tempel
          #:key
          (emacs-tempel emacs-tempel)
          (tempel-capf-hooks '(prog-mode-hook
                               text-mode-hook
                               conf-mode-hook
                               fundamental-mode))
          (default-templates? #t)
          (templates '())
          (tempel-trigger-prefix "<"))
  "Configure TempEL for emacs.  To extend a list of templates from other
features use `home-emacs-tempel-service-type'."
  (ensure-pred file-like? emacs-tempel)
  (ensure-pred string? tempel-trigger-prefix)
  (ensure-pred list? tempel-capf-hooks)

  (define emacs-f-name 'tempel)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (service
      home-emacs-tempel-service-type
      (home-emacs-tempel-configuration
       (templates
        (if default-templates?
            `(,#~"fundamental-mode ;; Available everywhere\n"
              (today (format-time-string "%Y-%m-%d"))
              (copyright
               (if (derived-mode-p 'lisp-data-mode 'clojure-mode 'scheme-mode)
                   ";;;"
                   comment-start)
               (if (string-suffix-p " " comment-start) "" " ")
               "Copyright © " (format-time-string "%Y") " "
               (format "%s <%s>" user-full-name user-mail-address)
               comment-end))
            '()))))

     (simple-service
      'emacs-tempel-user-templates
      home-emacs-tempel-service-type
      templates)

     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'tempel))
        (with-eval-after-load
         'tempel
         (setq tempel-trigger-prefix ,tempel-trigger-prefix)
         (defun rde-tempel-setup-capf ()
           "Prepends `tempel-complete' to `completion-at-point-functions'."
           (setq-local completion-at-point-functions
                       (cons 'tempel-complete
                             completion-at-point-functions)))

         (mapcar
          (lambda (mode)
            (add-hook mode 'rde-tempel-setup-capf))
          ',tempel-capf-hooks))

        (define-key global-map (kbd "M-+") 'tempel-insert)

        (autoload 'global-tempel-abbrev-mode "tempel")
        (if after-init-time
             (global-tempel-abbrev-mode 1)
             (add-hook 'after-init-hook 'global-tempel-abbrev-mode)))
      #:elisp-packages (list emacs-tempel)
      #:summary "\
Simple templates based on tempo syntax."
      #:commentary "\
Integrates well with CAPF and abbrev.  Use `expand-abbrev', `tempel-insert' or
just start typing `tempel-trigger-prefix' (default is \"<\") and use
`completion-at-point'.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-tempel)))
   (home-services-getter get-home-services)))


;;;
;;; Focus.
;;;

(define --Focus--)

(define* (feature-emacs-monocle
          #:key
          (olivetti-body-width 85))
  "Configure olivetti and helper functions for focused editing/reading."
  (define emacs-f-name 'monocle)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         ;; FIXME: Byte-compilation fails
         ;; (require 'olivetti)
         (require 'hide-mode-line))

        (setq olivetti-body-width ,olivetti-body-width
              olivetti-margin-width 0)

        (with-eval-after-load
         'org-agenda
         (defun ensure-olivetti (orig-fun &rest r)
           "Don't lose olivetti mode on `org-agenda-redo-all'."
           (let ((olivetti-p (if olivetti-mode 1 0)))
             (apply orig-fun r)
             (olivetti-mode olivetti-p)))
         (advice-add 'org-agenda-redo-all :around 'ensure-olivetti))

        (with-eval-after-load
         'hide-mode-line
         (setq hide-mode-line-excluded-modes '()))

        (defun rde--match-modes (modes)
          "Check if current mode is derived from one of the MODES."
          (seq-filter 'derived-mode-p modes))

        (defvar global-olivetti-ignore-modes
          '(minibuffer-mode
            which-key-mode
            minibuffer-inactive-mode)
          "A LIST of SYM representing `modes' to ignore in
`rde--turn-on-olivetti-mode'.")

        (defvar global-olivetti-ignore-buffers
          '(" *which-key*")
          "A LIST of STRING/REGEXP representing `buffer' names to ignore in
`rde--turn-on-olivetti-mode'.")

        (defun rde--turn-on-olivetti-mode ()
          "Apply `olivetti-mode' buffer-wise, upon `global-olivetti-mode',
  unless mode is `global-olivetti-ignore-modes' or buffer is
`global-olivetti-ignore-buffers'."
          (unless (or (memq major-mode global-olivetti-ignore-modes)
                      (seq-filter
                       (lambda (s) (string-match (buffer-name) s))
                       global-olivetti-ignore-buffers))
            (olivetti-mode 1)))

        (define-globalized-minor-mode global-olivetti-mode
          olivetti-mode rde--turn-on-olivetti-mode
          :require 'olivetti-mode
          :group 'olivetti)

        (defvar rde--monocle-previous-window-configuration nil
          "Window configuration for restoring on monocle exit.")

        (defun rde-toggle-monocle (arg)
          "Make window occupy whole frame if there are many windows. Restore
previous window layout otherwise.  With universal argument toggles
`global-olivetti-mode'."
          (interactive "P")

          (if arg
              (if (and global-olivetti-mode global-hide-mode-line-mode)
                  (progn
                   (global-hide-mode-line-mode -1)
                   (global-olivetti-mode -1))
                  (progn
                   (global-hide-mode-line-mode 1)
                   (global-olivetti-mode 1)))
              (if (one-window-p)
                  (if rde--monocle-previous-window-configuration
                      (let ((cur-buffer (current-buffer)))
                        (set-window-configuration
                         rde--monocle-previous-window-configuration)
                        (setq rde--monocle-previous-window-configuration nil)
                        (switch-to-buffer cur-buffer)))
                  (setq rde--monocle-previous-window-configuration
                        (current-window-configuration))
                  (delete-other-windows))))

        (with-eval-after-load 'rde-keymaps
          (define-key rde-toggle-map (kbd "o") 'olivetti-mode)
          (define-key rde-toggle-map (kbd "O") 'global-olivetti-mode)
          (define-key rde-toggle-map (kbd "m") 'hide-mode-line-mode)
          (define-key rde-toggle-map (kbd "M") 'global-hide-mode-line-mode))
        (define-key global-map (kbd "s-f") 'rde-toggle-monocle))
      #:summary "\
Focused editing and reading"
      #:commentary "\
Various functions, keybindings and settings for creating distraction free
working environemnt."
      #:keywords '(convenience reading editing)
      #:elisp-packages (list emacs-olivetti emacs-hide-header-line))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (olivetti-body-width . ,olivetti-body-width)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-project
          #:key
          (project-extra-dominating-files
           '(".project.el" ".dir-locals.el" ".gitignore")))
  "Configure project.el, a library to perform operations
on the current project."
  (ensure-pred list? project-extra-dominating-files)

  (define emacs-f-name 'project)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      ;; TODO: https://github.com/muffinmad/emacs-ibuffer-project
      ;; MAYBE: Rework the binding approach
      `((eval-when-compile
         (require 'project)
         (require 'cl-lib))
        (defgroup rde-project nil
          "Custom `project.el' enhancements."
          :group 'rde)
        (defcustom rde-project-dominating-files '()
          "List of root files that indicate a directory is a project."
          :group 'rde-project
          :type '(repeat string))

        (cl-defmethod project-root ((project (head explicit)))
          "Determine the PROJECT root."
          (cdr project))

        (defun rde-project-custom-root (dir)
          "Search in project's DIR for a set of project dominating files."
          (let* ((files rde-project-dominating-files)
                 (root (cl-find-if (lambda (file)
                                     (locate-dominating-file dir file))
                                   files)))
            (when root
              (cons 'explicit (locate-dominating-file dir root)))))

        (defun rde-project-org-capture ()
          "Run `org-capture' in the current project root."
          (interactive)
          (when-let ((default-dir (project-root (project-current t))))
            (dir-locals-read-from-dir default-dir)
            (org-capture)))

        (defun rde-project-compile (&optional comint)
          "Compile current project and choose if buffer will be in COMINT mode."
          (interactive "P")
          (let ((default-directory (project-root (project-current t)))
                (compilation-buffer-name-function
                 (or project-compilation-buffer-name-function
                     compilation-buffer-name-function)))
            (call-interactively 'compile nil (and comint (vector (list 4))))))

        (setq rde-project-dominating-files ',project-extra-dominating-files)
        (add-hook 'project-find-functions 'project-try-vc)
        (add-hook 'project-find-functions 'rde-project-custom-root)
        (advice-add 'project-compile :override 'rde-project-compile)

        (define-key global-map (kbd "s-p") project-prefix-map)

        (with-eval-after-load 'project
          (require 'xdg)
          (setq project-switch-use-entire-map t)
          ,@(if (get-value 'emacs-consult config)
                '((eval-when-compile
                   (require 'consult))
                  (with-eval-after-load 'consult
                    (define-key project-prefix-map "F" 'consult-find)
                    (define-key project-prefix-map "R" 'consult-ripgrep)
                    (add-to-list 'project-switch-commands
                                 '(consult-find "Find file consult"))
                    (add-to-list 'project-switch-commands
                                 '(consult-ripgrep "Search for regexp with rg"))
                    (setq consult-project-root-function
                          (lambda ()
                            (when-let (project (project-current))
                                      (car (project-roots project)))))))
                '())
          (setq project-list-file
                (expand-file-name "emacs/projects"
                                  (or (xdg-cache-home) "~/.cache")))
          (add-to-list 'project-switch-commands '(project-compile "Compile") t)))
      #:summary "\
Enchancements for project management with project.el"
      #:commentary "\
Keybinding for `project-prefix-map', integration with consult and minor
adjustments."
      #:keywords '(convenience project)
      #:elisp-packages (list (get-value 'emacs-consult config emacs-consult)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: Migrate to beframe
;; <https://git.sr.ht/~protesilaos/beframe/tree/main/item/beframe.el>
(define* (feature-emacs-perspective
          #:key
          (emacs-perspective emacs-perspective)
          (persp-show-modestring? #t))
  "Configure perspective.el to group/isolate buffers per frames.  Make
emacsclient feels more like a separate emacs instance."

  (define emacs-f-name 'perspective)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(;; TODO: Show current perspective in some global space (tab-bar?).

        ;; Should be defined before perspective loaded
        (setq persp-mode-prefix-key (kbd "C-x P"))

        (with-eval-after-load
         'perspective
         (setq persp-show-modestring ,(if persp-show-modestring? 't 'nil))
         (setq persp-modestring-dividers '(" [" "]" "|")))

        ,@(if (get-value 'emacs-project config)
              `((defun rde-persp-switch-project (dir)
                   "Switch to a project in its own perspective."
                   (interactive (list (project-prompt-project-dir)))
                   (let ((name (file-name-nondirectory
                                (directory-file-name
                                 (file-name-directory dir)))))
                     (persp-switch name)
                     (project-switch-project dir)))
                (with-eval-after-load
                 'project
                 (define-key project-prefix-map
                   (kbd "P")
                   'rde-persp-switch-project)))
              '())

        (autoload 'persp-mode "perspective")
        (if after-init-time
            (persp-mode 1)
            (add-hook 'after-init-hook 'persp-mode)))
      #:summary "\
Buffer isolation for separate project and emacs clients"
      #:commentary "\
Provide basic adjustments and integration with project.el."
      #:keywords '(convenience)
      #:elisp-packages (list emacs-perspective))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ednc
          #:key
          (emacs-ednc emacs-ednc)
          (notifications-icon #f)
          (notifications-application-name "Emacs")
          (ednc-key "n"))
  "Configure the Emacs Desktop Notification Center (EDNC)."
  (ensure-pred file-like? emacs-ednc)
  (ensure-pred maybe-path? notifications-icon)
  (ensure-pred string? notifications-application-name)
  (ensure-pred string? ednc-key)

  (define emacs-f-name 'ednc)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EDNC."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defvar rde-ednc-map nil)
        (define-prefix-command 'rde-ednc-map)
        (defun rde-ednc--notify ()
          "Display the latest EDNC notification."
          (when (ednc-notifications)
            (ednc-format-notification (car (ednc-notifications)))))

        (defun rde-ednc-show-notification-log ()
          "Switch to the EDNC log buffer."
          (interactive)
          (when (bufferp (get-buffer ednc-log-name))
            (switch-to-buffer ednc-log-name)))

        (defun rde-ednc-close-last-notification ()
          "Close the latest notification provided by the notification daemon."
          (interactive)
          (when-let* ((notification (car (ednc-notifications))))
            (ednc--close-notification notification 2)))

        (defun rde-ednc-close-all-notifications ()
          "Dismiss all EDNC notifications."
          (interactive)
          (mapc 'ednc-dismiss-notification (cdr ednc--state)))

        (defun rde-ednc-update-notifications (&rest _)
          "Update the display of EDNC notifications."
          (interactive)
          (force-mode-line-update t))

        (add-hook 'after-init-hook 'ednc-mode)
        (add-hook 'ednc-notification-presentation-functions
                  'rde-ednc-update-notifications)
        (add-hook 'ednc-notification-presentation-functions
                  'ednc--update-log-buffer)
        (with-eval-after-load 'notifications
          (setq notifications-application-name ,notifications-application-name)
          ,@(if notifications-icon
                `((setq notifications-application-icon ,notifications-icon))
                '()))
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,ednc-key) 'rde-ednc-map))
        (let ((map rde-ednc-map))
          (define-key map "c" 'rde-ednc-close-last-notification)
          (define-key map "l" 'rde-ednc-show-notification-log)
          (define-key map "d" 'rde-ednc-close-all-notifications)))
      #:elisp-packages (list emacs-ednc))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-ednc)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ace-window
          #:key
          (emacs-ace-window emacs-ace-window)
          (ace-window-key "M-o"))
  "Configure ace-window, an Emacs package to quickly switch between windows."
  (ensure-pred file-like? emacs-ace-window)
  (ensure-pred string? ace-window-key)

  (define emacs-f-name 'window)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Emacs's windows."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((define-key global-map (kbd ,ace-window-key) 'ace-window)
        (with-eval-after-load 'ace-window
          (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
          (setq aw-background nil)
          (setq aw-scope 'frame)
          (setq aw-ignore-current nil)
          (setq aw-display-mode-overlay nil)))
      #:elisp-packages (list emacs-ace-window))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-ace-window . ,emacs-ace-window)))
   (home-services-getter get-home-services)))


;;;
;;; Development.
;;;

(define --Development--)

(define* (feature-emacs-smartparens
          #:key
          (emacs-smartparens emacs-smartparens)
          (smartparens-hooks '(prog-mode-hook))
          (smartparens-strict-hooks '(prog-mode-hook))
          (show-smartparens? #f)
          (paredit-bindings? #f))
  "Configure smartparens for structured code navigation, automatic string escape
and pair management."
  (ensure-pred file-like? emacs-smartparens)
  (ensure-pred list? smartparens-hooks)
  (ensure-pred list? smartparens-strict-hooks)
  (ensure-pred boolean? show-smartparens?)
  (ensure-pred boolean? paredit-bindings?)

  (define emacs-f-name 'smartparens)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'smartparens))
        ,@(if smartparens-hooks
              `((mapcar (lambda (hook)
                          (add-hook hook 'smartparens-mode))
                        ',smartparens-hooks))
              '())
        ,@(if smartparens-strict-hooks
              `((mapcar (lambda (hook)
                          (add-hook hook 'smartparens-strict-mode))
                        ',smartparens-strict-hooks))
              '())

        (with-eval-after-load 'smartparens
          ,@(if paredit-bindings?
                '((sp-use-paredit-bindings))
                '((sp-use-smartparens-bindings)))

          (with-eval-after-load 'paren
            (setq show-paren-style 'mixed)
            ,@(if show-smartparens?
                  '((show-paren-mode -1)
                    (show-smartparens-global-mode 1))
                  '((show-paren-mode 1))))

          (require 'smartparens-config)
          (define-key smartparens-mode-map (kbd "M-s") nil)
          (setq sp-highlight-pair-overlay nil)
          (define-key smartparens-mode-map (kbd "M-S") 'sp-forward-slurp-sexp)))
      #:summary "\
Structured editing and navigation, automatic string escaping and pair management"
      #:commentary "\
Various tweaks and settings for `smartparents.el'.

By default it calls `sp-use-smartparens-bindings' to set basic keybindings.

Use `sp-cheat-sheet' to get more information about available commands and
their behavior."
      #:keywords '(convenience editing)
      #:elisp-packages (list emacs-smartparens))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-smartparens)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-eglot
          #:key
          (emacs-consult-eglot emacs-consult-eglot-sans-eglot))
  "Configure eglot, an LSP package for emacs."

  (define emacs-f-name 'eglot)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((define-key goto-map (kbd "s") 'consult-eglot-symbols)
        (with-eval-after-load
         'eglot
         ;; MAYBE: Move to other feature?
         (setq eldoc-echo-area-use-multiline-p nil)
         (setq eglot-confirm-server-initiated-edits nil)
         (add-hook 'eglot-managed-mode-hook
                   (lambda () (setq consult-imenu--cache nil)))
         ;; Potentially can speed up eglot:
         ;; (setq eglot-events-buffer-size 0)
         (setq eglot-extend-to-xref t)))
      #:summary "\
Refactoring, completion, navigation, documentation via LSP"
      #:commentary "\
Mostly workarounds and integratios with other packages."
      #:keywords '(convenience completion lsp editing languages)
      #:elisp-packages (list emacs-consult-eglot))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-flymake)
  "Set up Flymake, the built-in on-the-fly syntax checker for Emacs."

  (define emacs-f-name 'flymake)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Flymake."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'flymake
          (let ((map flymake-mode-map))
            (define-key map (kbd "M-n") 'flymake-goto-next-error)
            (define-key map (kbd "M-p") 'flymake-goto-prev-error)))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-elisp
          #:key
          (ielm-key "I"))
  "Configure tooling for Emacs Lisp, the programming
language for GNU Emacs."
  (ensure-pred string? ielm-key)

  (define emacs-f-name 'elisp)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Emacs Lisp."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if (get-value 'emacs-flymake config)
              '((add-hook 'emacs-lisp-mode-hook 'flymake-mode))
              '())
        (with-eval-after-load 'elisp-mode
          (let ((map emacs-lisp-mode-map))
            (define-key map (kbd "C-x C-e") 'pp-eval-last-sexp)
            (define-key map (kbd "M-:") 'pp-eval-expression)
            (define-key map (kbd "C-c C-m") 'pp-macroexpand-last-sexp)
            (define-key map (kbd "C-c C-b") 'eval-buffer)
            ,@(if (get-value 'emacs-embark config)
                  '((autoload 'embark-pp-eval-defun "embark")
                    (define-key map (kbd "C-c C-c") 'embark-pp-eval-defun))
                  '())))
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,ielm-key) 'ielm))
        (with-eval-after-load 'ielm
          (setq ielm-header "")
          (setq ielm-noisy nil))
        ,@(if (get-value 'emacs-org config)
              '((with-eval-after-load 'org
                  (add-to-list 'org-structure-template-alist
                               '("el" . "src elisp")))
                (with-eval-after-load 'ob-emacs-lisp
                  (setq org-babel-default-header-args:elisp
                        '((:lexical . "t")
                          (:results . "scalar")))))
              '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; MAYBE: Merge it with feature-git
(define* (feature-emacs-git
          #:key
          (git-gutter-transient-key "s-g")
          (project-directory #f)
          (emacs-magit emacs-magit)
          (emacs-magit-todos emacs-magit-todos)
          (emacs-git-timemachine emacs-git-timemachine)
          (emacs-git-link emacs-git-link)
          (emacs-git-gutter-fringe emacs-git-gutter-fringe)
          (emacs-git-gutter-transient emacs-git-gutter-transient))
  "Configure git-related utilities for GNU Emacs, including magit,
git-link, git-timemachine."
  ;; MAYBE: Declare it as a feature value?
  (ensure-pred maybe-string? project-directory)
  (ensure-pred file-like? emacs-magit)
  (ensure-pred file-like? emacs-magit-todos)
  (ensure-pred file-like? emacs-git-timemachine)
  (ensure-pred file-like? emacs-git-link)
  (ensure-pred file-like? emacs-git-gutter-fringe)
  (ensure-pred file-like? emacs-git-gutter-transient)

  (define emacs-f-name 'git)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun rde-git-link ()
          "Same as `git-link', but with commit hash specified."
          (interactive)
          (defvar git-link-use-commit) ;; dynamically bind
          (let ((git-link-use-commit t))
            (call-interactively 'git-link)))

        (with-eval-after-load 'rde-keymaps
          (define-key rde-toggle-map (kbd "g") 'git-gutter-mode)
          (define-key rde-toggle-map (kbd "G") 'global-git-gutter-mode))
        (define-key global-map (kbd ,git-gutter-transient-key)
          'git-gutter-transient)

        (with-eval-after-load
         'git-gutter-transient
         (transient-insert-suffix
          'git-gutter-transient "Q"
          '(,git-gutter-transient-key
            "Quit and disable" git-gutter-transient:quit-and-disable
            :transient transient--do-exit)))

        (with-eval-after-load
         'magit
         (defvar rde-projects-directory ,(or project-directory 'nil)
           "Directory where project repositories are stored.")

         (autoload 'git-link--parse-remote "git-link")
         (defun rde-get-local-repo-path-from-url (url)
           "Get directory from repository url and suggest it to
`magit-clone-default-directory'."
           (let* ((path (cadr (git-link--parse-remote url)))
                  (dir (file-name-directory (directory-file-name path))))
             (if rde-projects-directory
                 (expand-file-name dir rde-projects-directory)
                 dir)))

         (setq magit-clone-default-directory 'rde-get-local-repo-path-from-url))

        (setq git-gutter:lighter " GG")

        (with-eval-after-load
         'git-gutter
         (require 'git-gutter-fringe)

         (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
         (add-to-list 'git-gutter:update-commands 'other-window)

         (add-hook 'magit-post-stage-hook 'git-gutter:update-all-windows)
         (add-hook 'magit-post-unstage-hook 'git-gutter:update-all-windows)

         (defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
           (cl-letf (((symbol-function 'yes-or-no-p) 'y-or-n-p))
                    (apply orig-fun r)))

         (dolist (fn '(git-gutter:stage-hunk git-gutter:revert-hunk))
                 (advice-add fn :around 'yes-or-no-p->-y-or-n-p))

         (defadvice git-gutter:stage-hunk (around auto-confirm compile activate)
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t)))
                    ad-do-it))

         (dolist (fringe '(git-gutter-fr:added
                           git-gutter-fr:modified))
                 (define-fringe-bitmap fringe (vector 8) nil nil '(top repeat)))
         (define-fringe-bitmap 'git-gutter-fr:deleted
           (vector 8 12 14 15)
           nil nil 'bottom)))
      #:summary "\
Combination of packages for git-related operations"
      #:commentary "\
Navigating history with git-time-machine, getting link to web interface of
origin repo with git-link, manipulation and fast navigation through unstaged
changes using git-gutter-transient.

Almost all other operations are covered by magit."
      #:keywords '(convenience faces)
      #:elisp-packages (list emacs-magit emacs-magit-todos
                             emacs-git-link emacs-git-timemachine
                             emacs-git-gutter-fringe
                             emacs-git-gutter-transient))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-geiser
          #:key
          (emacs-geiser emacs-geiser-latest)
          (emacs-gider emacs-gider-latest)
          (emacs-geiser-guile emacs-geiser-guile-latest)
          (emacs-geiser-eros emacs-geiser-eros-latest))
  "Configure geiser for emacs."
  (ensure-pred file-like? emacs-geiser)
  (ensure-pred file-like? emacs-gider)
  (ensure-pred file-like? emacs-geiser-guile)
  (ensure-pred file-like? emacs-geiser-eros)

  (define emacs-f-name 'geiser)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'geiser-repl
          (require 'xdg)
          (setq geiser-repl-query-on-kill-p nil)
          (setq geiser-repl-history-filename
                (expand-file-name "emacs/geiser_history"
                                  (or (xdg-cache-home) "~/.cache")))
          (setq geiser-repl-add-project-paths nil))
        (with-eval-after-load 'geiser-mode
          (geiser-eros-mode)
          (gider-mode))
        (with-eval-after-load 'geiser-impl
          (setq geiser-default-implementation 'guile)
          (setq geiser-active-implementations '(guile))
          (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

        ,@(if (get-value 'emacs-org config)
              '((with-eval-after-load 'org
                  (add-to-list 'org-structure-template-alist
                               '("sc" . "src scheme")))
                (with-eval-after-load 'ob-core
                  (require 'ob-scheme))
                (with-eval-after-load 'ob-scheme
                  (setq org-babel-default-header-args:scheme
                        '((:results . "scalar")))))
              '()))
      #:elisp-packages
      (list emacs-geiser emacs-geiser-guile emacs-geiser-eros emacs-gider)
      #:summary "\
Scheme interpreter, giving access to a REPL and live metadata."
      #:commentary "\
Geiser is configured for the Guile scheme implementation.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-geiser)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-guix
          #:key
          (emacs-guix emacs-guix-latest)
          (guix-key "s-G")
          (guix-directory "~/work/gnu/guix"))
  "Configure emacs for guix usage and development."
  (ensure-pred maybe-path? guix-directory)

  (define emacs-f-name 'guix)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,#~"\
;; Extend info-lookup-alist with Guix Manual node to
;; make `C-h S' find guix services and other items."
        (with-eval-after-load
         'info-look
         (info-lookup-add-help
          :mode 'scheme-mode
          :regexp "[^()`',\"        \n]+"
          :ignore-case t
          :doc-spec '(("(r5rs)Index" nil "^[ 	]+-+ [^:]+:[ 	]*" "\\b")
                      ;; TODO: Check what rest nil arguments do
                      ("(Guile)Procedure Index" nil nil nil)
                      ("(Guile)Variable Index" nil nil nil)
                      ("(Guix)Programming Index" nil nil nil))))

        (eval-when-compile (require 'guix))
        (autoload 'global-guix-prettify-mode "guix-prettify")
        (autoload 'guix-prettify-mode "guix-prettify")
        (define-key rde-toggle-map (kbd "p") 'guix-prettify-mode)
        (define-key rde-toggle-map (kbd "P") 'global-guix-prettify-mode)
        (if after-init-time
            (global-guix-prettify-mode 1)
            (add-hook 'after-init-hook 'global-guix-prettify-mode))

        (with-eval-after-load 'daemons
          (setq daemons-init-system-submodules '(daemons-shepherd))
          (setq daemons-always-sudo nil))

        (with-eval-after-load
         'guix
         (if ,guix-directory
             '((setq guix-directory ,guix-directory))
             '()))

        (global-set-key (kbd ,guix-key) 'guix))
      #:elisp-packages (list emacs-guix
                             emacs-daemons)
      #:summary "\
Configure emacs for guix usage and development."
      #:commentary "\
Add keybindings, extend info-lookup-alist with (Guix)Programming Index for C-h
S to show services and other guix items.")))

  (feature
   (name f-name)
   (values `((,f-name . 'emacs-guix)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-xref)
  "Configure Xref, an Emacs mechanism to find definitions
and references in your programs."
  (define emacs-f-name 'xref)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Xref."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'xref
          (setq xref-auto-jump-to-first-definition 'move)
          (setq xref-auto-jump-to-first-xref 'move)
          (setq xref-prompt-for-identifier
                '(not xref-find-definitions-other-window
                      xref-find-definitions-other-frame))
          ,@(if (get-value 'emacs-consult config)
                '((setq xref-show-xrefs-function 'consult-xref)
                  (setq xref-show-definitions-function 'consult-xref))
                '()))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; Reading.
;;;

(define --Reading--)

(define* (feature-emacs-pdf-tools
          #:key
          (emacs-pdf-tools emacs-pdf-tools)
          (emacs-saveplace-pdf-view emacs-saveplace-pdf-view))
  "Configure pdf-tools, to work with pdfs inside Emacs."
  (ensure-pred file-like? emacs-pdf-tools)
  (ensure-pred file-like? emacs-saveplace-pdf-view)

  (define emacs-f-name 'pdf-tools)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         (car (cdr (command-line)))))
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((autoload 'pdf-view-mode "pdf-view" "")

        (defun rde-pdf-tools--list-buffers ()
          "List all currently-opened `pdf-view' mode buffers."
          (cl-remove-if-not
           (lambda (buffer)
             (with-current-buffer buffer
               (derived-mode-p 'pdf-view-mode)))
           (buffer-list)))

        (defun rde-pdf-tools-update-buffers (&optional _theme)
          "Apply `rde-pdf-tools-mode' to currently open `pdf-view' mode buffers."
          (dolist (buffer (rde-pdf-tools--list-buffers))
            (with-current-buffer buffer
              (rde-pdf-tools-mode 1))))

        ,@(if (get-value 'emacs-modus-themes config)
              '((defgroup rde-pdf-tools nil
                  "Custom tweaks for PDF Tools."
                  :group 'rde)
                (define-minor-mode rde-pdf-tools-mode
                  "Apply `pdf-tools' settings based on the current theme."
                  :group 'rde-pdf-tools
                  (if rde-pdf-tools-mode
                      (if (rde-modus-themes--dark-theme-p)
                          (pdf-view-themed-minor-mode 1)
                        (pdf-view-themed-minor-mode -1))
                    (pdf-view-themed-minor-mode -1)))
                (add-hook 'pdf-view-mode-hook 'rde-pdf-tools-mode))
            '())

        ,@(if (get-value 'emacs-circadian config)
              '((add-hook 'circadian-after-load-theme-hook
                          'rde-pdf-tools-update-buffers))
            '())

        (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
        (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
        (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
        (with-eval-after-load 'pdf-view
         (setq pdf-view-use-scaling t)
         (setq pdf-view-display-size 'fit-page)
         (setq pdf-view-resize-factor 1.025))
        (with-eval-after-load 'saveplace
         (require 'saveplace-pdf-view)))
      #:summary "\
PDF reading in Emacs"
      #:commentary "\
A few adjustments for pdf-tools and xdg entry for opening PDF files in emacs
client."
      #:keywords '(convenience reading)
      #:elisp-packages (list emacs-pdf-tools emacs-saveplace-pdf-view))
     (emacs-xdg-service emacs-f-name "Emacs (Client) [PDF]" xdg-gexp
                        #:default-for '(application/pdf))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-nov-el
          #:key
          (emacs-nov-el emacs-nov-el))
  "Configure nov.el for GNU Emacs."
  (define emacs-f-name 'nov-el)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

        (with-eval-after-load
         'nov
         (add-hook 'nov-mode-hook (lambda () (olivetti-mode 1)))

         (setq nov-text-width t)

         (autoload 'pj-line-width "justify-kp")

         ;; <https://depp.brause.cc/nov.el#Rendering>
         (defun rde-nov-window-configuration-change-hook ()
           (rde-nov-post-html-render-hook)
           (remove-hook 'window-configuration-change-hook
                        'rde-nov-window-configuration-change-hook
                        t))

         (defun rde-nov-post-html-render-hook ()
           "Adds local hook to listen to resize events
`rde-nov-window-configuration-change-hook'"
           (if (get-buffer-window)
               (let ((max-width (pj-line-width))
                     buffer-read-only)
                 (save-excursion
                  (goto-char (point-min))
                  (while (not (eobp))
                    (when (not (looking-at "^[[:space:]]*$"))
                      (goto-char (line-end-position))
                      (when (> (shr-pixel-column) max-width)
                        (goto-char (line-beginning-position))
                        (pj-justify)))
                    (forward-line 1))))
               (add-hook 'window-configuration-change-hook
                         'rde-nov-window-configuration-change-hook
                         nil t)))

         (add-hook 'nov-post-html-render-hook 'rde-nov-post-html-render-hook)))
      #:summary "\
Improved rendering for epub reading via nov.el"
      #:commentary "\
Integrates `olivetti-mode' for focused reading, justify-kp for better
rendering.

application/epub+zip mime-type will be openned with emacs client."
      #:keywords '(convenience hypermedia multimedia epub reading)
      #:elisp-packages (list emacs-nov-el
                            (get-value 'emacs-olivetti config emacs-olivetti)
                            emacs-justify-kp))
     (emacs-xdg-service
      emacs-f-name "Emacs (Client) [EPUB]"
      #~(apply system*
               #$(get-value 'emacs-client-create-frame config)
               (cdr (command-line)))
      #:default-for '(application/epub+zip))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-elfeed
          #:key
          (emacs-elfeed emacs-elfeed)
          (emacs-elfeed-org emacs-elfeed-org)
          (elfeed-org-files '())
          (capture-key "e"))
  "Setup and configure Elfeed for Emacs."
  (ensure-pred list-of-strings? elfeed-org-files)
  (define (not-empty? x) (not (null? x)))
  (ensure-pred not-empty? elfeed-org-files)
  (ensure-pred file-like? emacs-elfeed)
  (ensure-pred file-like? emacs-elfeed-org)
  (ensure-pred string? capture-key)

  (define emacs-f-name 'elfeed)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((with-eval-after-load
           'rde-keymaps
           (define-key rde-app-map (kbd "e") 'elfeed))

          (with-eval-after-load
           'elfeed-org
           (setq rmh-elfeed-org-files ',elfeed-org-files))

          (with-eval-after-load
           'org-capture
           (add-to-list
            'org-capture-templates
            '(,capture-key "Elfeed" entry
              (file+headline ,(car elfeed-org-files)
                             "Untagged")
              "*** %:annotation\n"
              :immediate-finish t)))

          (with-eval-after-load
           'elfeed
           (setq elfeed-db-directory
                 (expand-file-name "elfeed" (getenv "XDG_STATE_HOME")))
           (elfeed-org)))
        #:summary "\
Elfeed Emacs interface"
        #:commentary "\
Keybinding in `rde-app-map', xdg entry for adding rss feed.
Configured with an elfeed-org storage for easier tagging.
capture-in-browser? needs to set
\"javascript:location.href='org-protocol://capture?%27 +new
URLSearchParams({template: %27r%27, url: window.location.href,title:
document.title, body: window.getSelection()});\" as a web bookmark."
        #:keywords '(convenience)
        #:elisp-packages
        (list emacs-elfeed emacs-elfeed-org)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-elfeed)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-help
          #:key
          (emacs-helpful emacs-helpful))
  "Configure the help system in Emacs.  Set up Helpful, an alternative to the
built-in help that provides much more contextual information."
  (ensure-pred file-like? emacs-helpful)

  (define emacs-f-name 'help)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the Emacs help system."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((let ((map global-map))
          (define-key map (vector 'remap 'describe-function) 'helpful-callable)
          (define-key map (vector 'remap 'describe-variable) 'helpful-variable)
          (define-key map (vector 'remap 'describe-key) 'helpful-key)
          (define-key map (vector 'remap 'describe-command) 'helpful-command)
          (define-key map (vector 'Info-goto-emacs-command-node)
            'helpful-function))
        (define-key help-map "o" 'helpful-at-point)
        ,@(if (get-value 'emacs-embark config)
              '((with-eval-after-load 'embark
                  (define-key embark-symbol-map (vector 'remap 'describe-symbol)
                    'helpful-symbol)
                  (let ((map embark-become-help-map))
                    (define-key map (vector 'remap 'describe-function)
                      'helpful-callable)
                    (define-key map (vector 'remap 'describe-variable)
                      'helpful-variable)
                    (define-key map (vector 'remap 'describe-symbol)
                      'helpful-symbol)
                    (define-key map (vector 'remap 'describe-command)
                      'helpful-command))))
              '())
        (add-hook 'helpful-mode-hook 'visual-line-mode)
        (with-eval-after-load 'helpful
          (define-key helpful-mode-map "q" 'kill-this-buffer))
        (with-eval-after-load 'help-mode
          (define-key help-mode-map "q" 'kill-this-buffer)
          (setq help-window-select t)))
      #:elisp-packages (list emacs-helpful))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-helpful)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-info
          #:key
          (emacs-info-plus emacs-info-plus))
  "Configure Info and Info+ for reading Info documents in Emacs."
  (ensure-pred file-like? emacs-info-plus)

  (define emacs-f-name 'info)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Info."
    (define theme (get-value 'emacs-light-theme config))
    (define emacs-modus-themes (get-value 'emacs-modus-themes config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if emacs-modus-themes
              `((eval-when-compile
                 (require 'modus-themes)
                 (require 'cl-seq))
                (require ',(symbol-append theme '-theme))
                (eval-when-compile
                 (enable-theme ',theme))
                (defun rde-info-set-custom-faces ()
                  "Apply more pleasant faces to `Info-mode' and `Info+-mode'."
                  (interactive)
                  (face-remap-add-relative 'default :inherit 'variable-pitch)
                  (when (modus-themes--current-theme)
                    (modus-themes-with-colors
                     (custom-set-faces
                      `(info-reference-item
                        ((,c :background unspecified)))
                      `(info-function-ref-item
                        ((,c :background unspecified)))
                      `(info-command-ref-item ((,c :background unspecified)))
                      `(info-macro-ref-item ((,c :background unspecified)))
                      `(info-variable-ref-item ((,c :background unspecified)))))))
                (add-hook 'Info-mode-hook 'rde-info-set-custom-faces))
              '())
        (with-eval-after-load 'info
          (define-key Info-mode-map "q" 'kill-this-buffer)
          (setq Info-use-header-line nil)
          (require 'info+)
          (add-hook 'Info-mode-hook 'visual-line-mode)
          (setq info-manual+node-buffer-name-mode t)
          (setq Info-persist-history-mode t)
          (setq Info-fontify-isolated-quote-flag nil)
          (setq Info-fontify-reference-items-flag t)
          (setq Info-fontify-quotations t)
          (setq Info-breadcrumbs-in-mode-line-mode nil)))
      #:elisp-packages (append
                         (list emacs-info-plus)
                         (or (and=> emacs-modus-themes list) '())))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-info-plus)))
   (home-services-getter get-home-services)))


;;;
;;; Notetaking.
;;;

(define --Notetaking--)

(define* (feature-emacs-org
          #:key
          (emacs-org-modern emacs-org-modern)
          (emacs-org-appear emacs-org-appear)
          (emacs-org-make-toc emacs-org-make-toc)
          (org-directory "~/org")
          (org-capture-templates #f)
          (org-todo-keywords #f)
          (org-tag-alist #f)
          (org-rename-buffer-to-title? #t)
          (org-indent? #t)
          (org-modern? #t)
          (auto-update-toc? #f))
  "Configure org-mode for GNU Emacs."
  (ensure-pred path? org-directory)
  (ensure-pred maybe-list? org-capture-templates)
  (ensure-pred maybe-list? org-todo-keywords)
  (ensure-pred maybe-list? org-tag-alist)
  (ensure-pred boolean? org-rename-buffer-to-title?)
  (ensure-pred boolean? org-indent?)
  (ensure-pred boolean? org-modern?)
  (ensure-pred boolean? auto-update-toc?)
  (ensure-pred file-like? emacs-org-modern)
  (ensure-pred file-like? emacs-org-appear)
  (ensure-pred file-like? emacs-org-make-toc)

  (define emacs-f-name 'org)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (when (get-value 'emacs-tempel config)
       (simple-service
        'emacs-org-templates
        home-emacs-tempel-service-type
        `(org-mode
          ,#~""
          (title "#+title: " p n "#+author: " user-full-name n
                 "#+language: en" n n)
          (quote "#+begin_quote" n> r> n> "#+end_quote")
          (example "#+begin_example" n> r> n> "#+end_example")
          (center "#+begin_center" n> r> n> "#+end_center")
          (comment "#+begin_comment" n> r> n> "#+end_comment")
          (verse "#+begin_verse" n> r> n> "#+end_verse")
          (src "#+begin_src " p n> r> n> "#+end_src"
               :post (org-edit-src-code))
          (elisp "#+begin_src emacs-lisp" n> r> n "#+end_src"
                 :post (org-edit-src-code)))))

     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'org)
         (require 'org-refile)
         (require 'org-modern))

        (define-key mode-specific-map (kbd "c") 'org-capture)

        (with-eval-after-load 'org-crypt
          (setq org-crypt-key user-mail-address))

        (with-eval-after-load 'org
         (setq org-adapt-indentation nil)
         (setq org-edit-src-content-indentation 0)
         (setq org-startup-indented ,(if org-indent? 't 'nil))

         (setq org-outline-path-complete-in-steps nil)
         (setq org-refile-use-outline-path 'full-file-path)
         (setq org-refile-allow-creating-parent-nodes 'confirm)
         (setq org-refile-targets `((nil . (:maxlevel . 3))
                                    (org-agenda-files . (:maxlevel . 3))))

         (setq org-ellipsis "⤵")
         (set-face-attribute 'org-ellipsis nil
                             :inherit '(font-lock-comment-face default)
                             :weight 'normal)
         (setq org-hide-emphasis-markers t)
         (setq org-log-into-drawer t)

         (setq org-directory ,org-directory)
         (setq org-default-notes-file (concat org-directory "/todo.org"))

         ,@(if org-capture-templates
               `((setq org-capture-templates ',org-capture-templates))
               '())

         ,@(if org-todo-keywords
               `((setq org-todo-keywords ',org-todo-keywords))
               '())

         ,@(if org-tag-alist
               `((setq org-tag-alist ',org-tag-alist))
               '())

         ;; <https://emacs.stackexchange.com/questions/54809/rename-org-buffers-to-orgs-title-instead-of-filename>
         (defun rde-buffer-name-to-title (&optional end)
           "Rename buffer to value of #+TITLE:.
If END is non-nil search for #+TITLE: at `point' and
delimit it to END.
Start an unlimited search at `point-min' otherwise."
           (interactive)
           (let ((case-fold-search t)
                 (beg (or (and end (point))
                          (point-min))))
             (save-excursion
              (when end
                (goto-char end)
                (setq end (line-end-position)))
              (goto-char beg)
              (when (re-search-forward
                     "^[[:space:]]*#\\+TITLE:[[:space:]]*\\(.*?\\)[[:space:]]*$"
                     end t)
                (rename-buffer (match-string 1)))))
           nil)

         (defun rde-buffer-name-to-title-config ()
           "Configure Org to rename buffer to value of #+TITLE:."
           (font-lock-add-keywords nil '(rde-buffer-name-to-title)))

         ,@(if org-rename-buffer-to-title?
               '((add-hook 'org-mode-hook 'rde-buffer-name-to-title-config))
               '())

         ,@(if auto-update-toc?
               `((eval-when-compile (require 'org-make-toc))
                 (add-hook 'org-mode-hook 'org-make-toc-mode))
               '())

         ;; TODO: Move to feature notmuch
         (with-eval-after-load 'notmuch (require 'ol-notmuch))

         (add-hook 'org-mode-hook 'org-appear-mode)
         (add-hook 'org-mode-hook 'olivetti-mode)

         (with-eval-after-load
          'org-modern
          (setq org-modern-todo nil)
          (setq org-modern-timestamp nil)
          (setq org-modern-statistics nil)
          (setq org-modern-tag nil)
          (setq org-modern-priority nil)
          (setq org-modern-hide-stars nil)
          ;; Instead of making leading stars really invisible, let's just use
          ;; the same color as bg and keep them accessible/editable.
          (setq org-hide-leading-stars t))

         (autoload 'global-org-modern-mode "org-modern")
         ,@(if org-modern?
               `((if after-init-time
                     (global-org-modern-mode)
                     (add-hook 'after-init-hook 'global-org-modern-mode)))
               '())))
      #:summary "\
Sensible defaults for org mode"
      #:commentary "\
Indentation and refile configurations, visual adjustment."
      #:keywords '(convenience org-mode org-modern)
      #:elisp-packages
      (append
       (list emacs-org emacs-org-contrib
             (get-value 'emacs-olivetti config emacs-olivetti)
             emacs-org-appear emacs-org-modern)
       (if auto-update-toc? (list emacs-org-make-toc) '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define %rde-org-agenda-custom-commands
  ``((,(kbd "C-d") "Agenda for the day"
      ((agenda
        ""
        ((org-agenda-span 1)
         (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
         (org-agenda-block-separator nil)
         (org-agenda-entry-types '(:scheduled :timestamp :sexp))
         (org-scheduled-past-days 0)
         ;; We don't need the `org-agenda-date-today'
         ;; highlight because that only has a practical
         ;; utility in multi-day views.
         (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
         ;; (org-agenda-skip-function
         ;;  '(org-agenda-skip-entry-if 'todo '("NEXT")))
         (org-agenda-format-date "%A %-e %B %Y")
         (org-agenda-overriding-header "\nAgenda for the day\n")))
       (todo
        "NEXT"
        ((org-agenda-block-separator nil)
         (org-agenda-overriding-header "\nCurrent Tasks\n")))))
     (,(kbd "C-o") "Overview"
      ;; TODO: Add A priority to the top.
      ((agenda
        ""
        ((org-agenda-time-grid nil)
         (org-agenda-start-on-weekday nil)
         (org-agenda-start-day "+1d")
         (org-agenda-span 14)
         (org-agenda-show-all-dates nil)
         (org-agenda-time-grid nil)
         (org-agenda-show-future-repeats nil)
         (org-agenda-block-separator nil)
         (org-agenda-entry-types '(:deadline))
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
         (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
       (agenda
        "*"
        ((org-agenda-block-separator nil)
         (org-agenda-span 14)
         (org-agenda-show-future-repeats nil)
         (org-agenda-skip-deadline-prewarning-if-scheduled t)
         (org-agenda-overriding-header "\nAgenda\n")))
       (alltodo
        ""
        ((org-agenda-block-separator nil)
         (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled))))
         (org-agenda-overriding-header "\nBacklog\n")))))))

(define* (feature-emacs-org-agenda
          #:key
          (org-agenda-files 'nil)
          (org-agenda-custom-commands %rde-org-agenda-custom-commands))
  "Configure org-agenda for GNU Emacs."
  (define emacs-f-name 'org-agenda)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-org config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'org-agenda))
        (define-key global-map (kbd "C-x C-a") 'org-agenda)
        (with-eval-after-load
         'org-agenda
         ;; Impressive agenda examples
         ;; https://github.com/fniessen/emacs-leuven/blob/master/org-leuven-agenda-views.txt
         ;; Clean agenda view
         ;; https://gist.github.com/rougier/ddb84c16c28f7cd75e27e50d4c3c43da
         ;; https://d12frosted.io/posts/2020-06-23-task-management-with-roam-vol1.html
         (setq org-agenda-custom-commands ,org-agenda-custom-commands)
         (setq org-agenda-tags-column
               ;; TODO: Name this value better
               ,(- (get-value 'olivetti-body-width config 85)))
         (setq org-agenda-window-setup 'current-window)
         (setq org-agenda-files ',org-agenda-files)))
      #:summary "\
Preconfigured agenda views"
      #:commentary "\
Reasonable keybindings, preconfigured agenda views and integration with
olivetti package."
      #:keywords '(convenience))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: rewrite to states
(define* (feature-emacs-org-roam
          #:key
          (emacs-org-roam emacs-org-roam)
          (org-roam-directory #f)
          (org-roam-dailies-directory #f)
          (org-roam-capture-templates #f)
          (use-node-types? #t))
  "Configure org-roam for GNU Emacs."
  (ensure-pred file-like? emacs-org-roam)
  (define (not-boolean? x) (not (boolean? x)))
  (ensure-pred not-boolean? org-roam-directory)
  (ensure-pred maybe-path? org-roam-dailies-directory)
  (ensure-pred maybe-list? org-roam-capture-templates)
  (ensure-pred boolean? use-node-types?)

  (define emacs-f-name 'org-roam)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (let ((org-roam-v2-ack t))
           (require 'org-roam)))
        (setq org-roam-v2-ack t)
        (setq org-roam-completion-everywhere t
              org-roam-directory ,org-roam-directory)

        (autoload 'org-roam-db-autosync-enable "org-roam")
        (with-eval-after-load
         'org-roam

         (cl-defmethod
          org-roam-node-type ((node org-roam-node))
          "Return the TYPE of NODE, where the TYPE is a directory of
the node, relative to `org-roam-directory'."
          (condition-case
           nil
           (file-name-nondirectory
            (directory-file-name
             (file-name-directory
              (file-relative-name (org-roam-node-file node)
                                  org-roam-directory))))
           (error "")))

         (setq org-roam-node-display-template
               (concat ,(if use-node-types? "${type:15} " "")
                       "${title:80} " (propertize "${tags:20}" 'face 'org-tag))
               org-roam-node-annotation-function
               (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))
         (org-roam-db-autosync-enable)

         ,@(if org-roam-capture-templates
               `((setq org-roam-capture-templates ',org-roam-capture-templates))
               '())

         ,@(if org-roam-dailies-directory
               `((setq org-roam-dailies-directory ,org-roam-dailies-directory))
               '()))

        (let ((map mode-specific-map))
          (define-key map (kbd "n t") 'org-roam-dailies-goto-today)
          (define-key map (kbd "n d") 'org-roam-dailies-goto-date)
          (define-key map (kbd "n n") 'org-roam-buffer-toggle)
          (define-key map (kbd "n f") 'org-roam-node-find)
          (define-key map (kbd "n i") 'org-roam-node-insert)))

      #:summary "\
Knowlede base, note-taking set up and ready"
      #:commentary "\
Set roam directory, basic keybindings, reasonable defaults and adjust
marginalia annotations."
      #:keywords '(convenience org-mode roam knowledgebase)
      #:elisp-packages (list emacs-org-roam))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-citar
          #:key
          (emacs-citar emacs-citar)
          (emacs-citar-org-roam emacs-citar-org-roam)
          (citar-library-paths (list "~/docs/library"))
          (citar-notes-paths (list "~/docs/bib/notes"))
          (global-bibliography (list "~/docs/bib/biblio.bib")))
  "Configure org-cite and citar for GNU Emacs."
  (ensure-pred file-like? emacs-citar)
  (ensure-pred file-like? emacs-citar-org-roam)
  (ensure-pred list? citar-library-paths)
  (ensure-pred list? citar-notes-paths)
  (ensure-pred list? global-bibliography)

  (define emacs-f-name 'citar)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define emacs-all-the-icons (get-value 'emacs-all-the-icons config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'citar)
         (require 'oc-biblatex)
         (require 'oc-csl))

        (with-eval-after-load 'oc
          (require 'oc-csl)
          (setq org-cite-global-bibliography (list ,@global-bibliography))
          (setq org-cite-insert-processor 'citar)
          (setq org-cite-follow-processor 'citar)
          (setq org-cite-activate-processor 'citar)
          (setq org-cite-export-processors
                '((latex biblatex)
                  (t csl))))

        (with-eval-after-load 'citar
          ,@(if emacs-all-the-icons
                '((eval-when-compile
                   (require 'all-the-icons))
                  (setq citar-symbols
                        `((file ,(all-the-icons-faicon
                                  "file-o"
                                  :face 'all-the-icons-green
                                  :v-adjust -0.1) . " ")
                          (note ,(all-the-icons-material
                                  "speaker_notes"
                                  :face 'all-the-icons-blue
                                  :v-adjust -0.3) . " ")
                          (link ,(all-the-icons-octicon
                                  "link"
                                  :face 'all-the-icons-orange
                                  :v-adjust 0.01) . " "))))
                '())
          (setq citar-library-paths (list ,@citar-library-paths))
          (setq citar-notes-paths (list ,@citar-notes-paths))
          (setq citar-bibliography org-cite-global-bibliography))

        (autoload 'citar-embark-mode "citar-embark")
        ,@(if (get-value 'emacs-embark config)
              `((with-eval-after-load 'embark (citar-embark-mode 1)))
              '())

        (autoload 'citar-org-roam-mode "citar-org-roam")
        ,@(if (get-value 'emacs-org-roam config)
              `((with-eval-after-load 'org-roam (citar-org-roam-mode 1)))
              '())

        (defun rde-find-main-bibliography ()
          "Find and open main bibliography file."
          (interactive) (find-file ,(car global-bibliography)))

        (let ((map mode-specific-map))
          (define-key map (kbd "b") 'org-cite-insert)
          (define-key map (kbd "n b") 'rde-find-main-bibliography)))
      #:summary "\
Reference management with emacs and citar"
      #:commentary "\
Set org-cite processors and citar configuration, basic keybindings, reasonable
defaults."
      #:keywords
      '(convenience org-mode org-cite citar references roam knowledgebase)
      #:elisp-packages
      (append
       (if (get-value 'emacs-org-roam config) (list emacs-citar-org-roam) '())
       (or (and=> emacs-all-the-icons list) '())
       (list emacs-citar)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-citar)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-protocol)
  "Setup and configure Org-Protocol for Emacs."

  (define emacs-f-name 'org-protocol)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs config)
    (define emacs-cmd (get-value 'emacs-client config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((if after-init-time
            (require 'org-protocol)
            (add-hook 'after-init-hook (lambda () (require 'org-protocol)))))
      #:summary "\
Org Protocol Emacs"
      #:commentary "\
Adding xdg-mime-entry and loading org-protocol.
This integrates well with elfeed for now."
      #:keywords '(convenience)
      #:elisp-packages '())

     (emacs-xdg-service
      emacs-f-name
      "Emacs (Client) [Org-protocol]"
      emacs-cmd
      #:default-for '(x-scheme-handler/org-protocol))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-spelling
          #:key
          (spelling-program (@ (gnu packages aspell) aspell))
          (spelling-dictionaries (list (@ (gnu packages aspell) aspell-dict-en)))
          (flyspell-hooks #f)
          (flyspell-prog-hooks #f)
          (ispell-program-name (file-append spelling-program "/bin/"
                                            (package-name spelling-program)))
          (ispell-standard-dictionary #f)
          (ispell-personal-dictionary #f)
          (dictionary-server "dict.org")
          (dictionary-key "d"))
  "Configure spell-checking features in Emacs.
SPELLING-PROGRAM will be used to detect spelling mistakes based on
SPELLING-DICTIONARIES inside buffers of modes defined in FLYSPELL-HOOKS
(prose) and in docstrings of modes defined in FLYSPELL-PROG-HOOKS (programming)."
  (ensure-pred file-like? spelling-program)
  (ensure-pred list-of-file-likes? spelling-dictionaries)
  (ensure-pred maybe-list? flyspell-hooks)
  (ensure-pred maybe-list? flyspell-prog-hooks)
  (ensure-pred file-like-or-path? ispell-program-name)
  (ensure-pred maybe-string? ispell-standard-dictionary)
  (ensure-pred maybe-path? ispell-personal-dictionary)
  (ensure-pred string? dictionary-key)

  (define emacs-f-name 'spelling)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to spelling in Emacs."
    (list
     (simple-service
      'emacs-spelling-packages
      home-profile-service-type
      `(,spelling-program
        ,@spelling-dictionaries))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if flyspell-hooks
              `((mapcar (lambda (hook)
                          (add-hook hook 'flyspell-mode))
                        ',flyspell-hooks))
              '())
        ,@(if flyspell-prog-hooks
              `((mapcar (lambda (hook)
                          (add-hook hook 'flyspell-prog-mode))
                        ',flyspell-prog-hooks))
              '())

        (with-eval-after-load 'ispell
          (setq ispell-program-name ,ispell-program-name)
          ,@(if ispell-standard-dictionary
                `((setq ispell-dictionary ,ispell-standard-dictionary))
                '())
          ,@(if ispell-personal-dictionary
                `((setq ispell-personal-dictionary ,ispell-personal-dictionary))
                '()))

        (with-eval-after-load 'flyspell
          (setq flyspell-issue-welcome-flag nil)
          (setq flyspell-issue-message-flag nil))

        ;; TODO: It either should be in a separate feature or feature should
        ;; be called differently, because word definitions is not about
        ;; spelling, however spelling seems related to dictionaries.
        (with-eval-after-load 'dictionary
          (setq dictionary-server ,dictionary-server))
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,dictionary-key) 'dictionary-search))))))

  (feature
   (name f-name)
   (values
    (append
      `((,f-name . #t))
      (make-feature-values
       spelling-program
       spelling-dictionaries flyspell-hooks flyspell-prog-hooks
       ispell-standard-dictionary ispell-personal-dictionary)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-recur
          #:key
          (emacs-org-recur emacs-org-recur))
  "Configure org-recur, a simple mode for recurring org-mode tasks."
  (ensure-pred file-like? emacs-org-recur)

  (define f-name 'org-recur)
  (define emacs-f-name (symbol-append 'emacs- f-name))

  (define (get-home-services config)
    "Return home services related to org-recur."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((add-hook 'org-mode-hook 'org-recur-mode)
        (add-hook 'org-agenda-mode-hook 'org-recur-agenda-mode)
        (with-eval-after-load 'org-recur
          (let ((map org-recur-mode-map))
            (define-key map (kbd "C-c d") 'org-recur-finish)
            (define-key map (kbd "C-c 0") 'org-recur-schedule-today))
          (let ((map org-recur-agenda-mode-map))
            (define-key map (kbd "d") 'org-recur-finish)
            (define-key map (kbd "C-c d") 'org-recur-finish))
          (setq org-recur-finish-done t)
          (setq org-recur-finish-archive t)))
      #:elisp-packages (list emacs-org-recur))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-org-recur)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-graphviz
          #:key
          (emacs-graphviz-dot-mode emacs-graphviz-dot-mode))
  "Configure Graphviz, the open source graph visualization software."
  (ensure-pred file-like? emacs-graphviz-dot-mode)

  (define emacs-f-name 'graphviz)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Graphviz."
    (define graphviz (@ (gnu packages graphviz) graphviz))

    (list
     (simple-service
      'add-graphviz-home-package
      home-profile-service-type
      (list graphviz))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun rde-graphviz-fix-inline-images ()
          "Display inline images automatically."
          (interactive)
          (when org-inline-image-overlays
            (org-redisplay-inline-images)))

        (add-hook 'org-babel-after-execute-hook
                  'rde-graphviz-fix-inline-images)
        (require 'graphviz-dot-mode)
        ,@(if (get-value 'emacs-org config)
              '((with-eval-after-load 'ob-core
                  (require 'ob-dot))
                (with-eval-after-load 'ob-dot
                  (add-to-list 'org-babel-default-header-args:dot
                               '(:cmdline . "-Kdot -Tpng"))))
              '()))
      #:elisp-packages (list emacs-graphviz-dot-mode))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-graphviz-dot-mode)))
   (home-services-getter get-home-services)))


;;;
;;; Communication.
;;;

(define --Communication--)

(define* (feature-emacs-telega
          #:key
          (emacs-telega emacs-telega)
          (emacs-telega-contrib emacs-telega-contrib)
          (notify? #f))
  "Configure telega.el for GNU Emacs"
  (define emacs-f-name 'telega)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         "--eval"
         (string-append
          "(progn
(set-frame-name \"Telega - Emacs Client\")
(if (and (boundp 'telega--status) (equal telega--status \"Ready\"))
 (telega-browse-url \"" (car (cdr (command-line))) "\")"
 "
 (telega)
 (add-hook 'telega-ready-hook
  (lambda ()
   (telega-browse-url \"" (car (cdr (command-line))) "\")))"
   "))")))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'telega)
         (require 'cape))

        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd "t") 'telega))

        ,@(if notify?
               '((add-hook 'telega-load-hook 'telega-notifications-mode))
               '())

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar rde-telega-buffer-source
                        `(:name "Telega"
                          :narrow ?t
                          :category buffer
                          :state ,'consult--buffer-state
                          :items ,(lambda ()
                                    (mapcar 'buffer-name
                                            (rde-completion--mode-buffers
                                             'telega-chat-mode
                                             'telega-root-mode))))
                        "Source for Telega buffers to be set in \
`consult-buffer-sources'.")
                (with-eval-after-load 'consult
                  (add-to-list 'consult-buffer-sources
                               rde-telega-buffer-source))
                (with-eval-after-load 'rde-completion
                  (add-to-list 'rde-completion-initial-narrow-alist
                               '(telega-root-mode . ?t))
                  (add-to-list 'rde-completion-initial-narrow-alist
                               '(telega-chat-mode . ?t))))
              '())

        (with-eval-after-load 'telega
          (require 'xdg)
          (setq telega-directory
                (expand-file-name "emacs/telega" (or (xdg-cache-home) "~/.cache")))

         ;; The real telega width will be 81
         (setq telega-chat-fill-column 70)
         (define-key telega-chat-mode-map (kbd "s-B") 'telega-chat-with)
         (define-key telega-root-mode-map (kbd "s-B") 'telega-chat-with)
         ,@(if (get-value 'mpv config)
               `((setq telega-video-player-command
                       ,(file-append (get-value 'mpv config) "/bin/mpv")))
               '())

         ,@(if (get-value 'emacs-embark config)
               '((setq telega-open-file-function 'embark-open-externally))
               '())
         (setq telega-open-message-as-file
               '(video audio voice-note animation video-note))

         (autoload 'company-grab "company")

         (setq telega-emoji-company-backend 'telega-company-emoji)

         (defun rde-telega-chat-mode ()
           "Add completion at point functions made from company backends."
           (setq-local
            completion-at-point-functions
            (append
             (mapcar
              'cape-company-to-capf
              (append (list telega-emoji-company-backend
                            'telega-company-username
                            'telega-company-hashtag)
                      (when (telega-chat-bot-p telega-chatbuf--chat)
                        '(telega-company-botcmd))))
                    completion-at-point-functions)))
         (add-hook 'telega-chat-mode-hook 'rde-telega-chat-mode)

         (setq telega-completing-read-function completing-read-function)))
      #:summary "\
Telegram client in Emacs"
      #:commentary "\
A few keybindings and small adjustments."
      #:keywords '(convenience faces)
      #:elisp-packages (list emacs-telega emacs-telega-contrib
                             (get-value 'emacs-cape config emacs-cape)))

     (emacs-xdg-service emacs-f-name "Emacs (Client) [tg:]" xdg-gexp
                        #:default-for '(x-scheme-handler/tg))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ebdb
          #:key
          (emacs-ebdb emacs-ebdb)
          (ebdb-sources (list "~/docs/contacts"))
          (ebdb-popup-size 0.4)
          (ebdb-key "b"))
  "Configure the ebdb contact management package for Emacs.
EBDB-SOURCES is a list of filenames to retrieve database
information from.
You can control the size of ebdb popup windows via EBDB-POPUP-SIZE
with a floating-point value between 0 and 1."
  (ensure-pred file-like? emacs-ebdb)
  (ensure-pred list-of-strings? ebdb-sources)
  (ensure-pred number? ebdb-popup-size)
  (ensure-pred string? ebdb-key)

  (define emacs-f-name 'ebdb)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EBDB."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defvar rde-ebdb-map nil
          "Map to bind EBDB commands under.")
        (define-prefix-command 'rde-ebdb-map)
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,ebdb-key) 'rde-ebdb-map)
          (let ((map rde-ebdb-map))
            (define-key map "a" 'ebdb-display-all-records)
            (define-key map "c" 'ebdb-create-record-extended)))
        (with-eval-after-load 'ebdb
          (require 'ebdb-i18n)
          (require 'ebdb-vcard)
          ,@(if (get-value 'emacs-org config)
                '((require 'ebdb-org))
                '())
          ,@(if (get-value 'mail-accounts config)
                '((require 'ebdb-mua)
                  (with-eval-after-load 'ebdb-mua
                    (setq ebdb-mua-pop-up nil)))
                '())
          ,@(if (get-value 'notmuch config)
                `((require 'ebdb-notmuch))
                '())
          ,@(if (get-value 'emacs-message config)
                `((require 'ebdb-message))
                '())
          ,@(if (get-value 'emacs-spelling config)
                `((require 'ebdb-ispell))
                '())
          ,@(if (get-value 'emacs-gnus config)
                '((require 'ebdb-gnus))
                '())
          (setq ebdb-sources (list ,@ebdb-sources))
          (setq ebdb-default-country nil)
          (setq ebdb-default-window-size ,ebdb-popup-size)
          (setq ebdb-dedicated-window 'ebdb)
          (setq ebdb-mail-avoid-redundancy t)
          (setq ebdb-complete-mail 'capf)
          (setq ebdb-completion-display-record nil)
          (setq ebdb-complete-mail-allow-cycling nil)
          (setq ebdb-save-on-exit t)
          (define-key ebdb-mode-map "q" 'kill-this-buffer)))
      #:elisp-packages (list emacs-ebdb))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-ebdb)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-elpher)
  "Configure elpher, the Emacs' gemini and gopher browser."
  (define emacs-f-name 'elpher)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define openssl (get-value 'openssl config (@ (gnu packages tls) openssl)))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         "--eval"
         (string-append
          "(elpher-go \"" (car (cdr (command-line))) "\")")))
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((autoload 'elpher-go "elpher")
        (with-eval-after-load
         'elpher
         (setq elpher-openssl-command ,(file-append openssl "/bin/openssl"))))
      #:summary "\
Gemini and gopher browser"
      #:commentary "\
gemini:// links will be automatically openned in emacs client."
      #:keywords '(convenience)
      #:elisp-packages (list emacs-elpher))
     (emacs-xdg-service emacs-f-name "Emacs (Client) [gemini:]" xdg-gexp
                        #:default-for '(x-scheme-handler/gemini))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; Multimedia.
;;;

(define (file-like-or-path-or-symbol-or-boolean? x)
  (or (boolean? x) (symbol? x) (file-like-or-path? x)))

(define* (feature-emacs-dashboard
          #:key
          (emacs-dashboard emacs-dashboard)
          (show-on-startup? #t)
          (items #f)
          (item-generators #f)
          (item-shortcuts #f)
          (item-names #f)
          (navigator-buttons #f)
          (banner #f)
          (banner-max-height 0)
          (banner-max-width 0)
          (dashboard-agenda-weekly? #t)
          (dashboard-agenda-prefix-format #f)
          (path-max-length 70)
          (dashboard-key "h"))
  "Configure Emacs Dashboard, an extensible startup screen.
Choose whether to be prompted by the dashboard on startup by setting
SHOW-ON-STARTUP?.
Set up the visible sections via ITEMS, where each entry is
of the form (LIST-TYPE . LIST-SIZE).  See @code{dashboard-items} to get
an idea of the format.  For the aforementioned to work, you also need to
configure ITEM-GENERATORS, where each entry is of the form
(LIST-TYPE . LIST-GENERATOR-FUNCTION).  You can quickly navigate
to each section with ITEM-SHORTCUTS and set a custom name for each one via
ITEM-NAMES.

NAVIGATOR-BUTTONS are custom buttons that you can display below the BANNER, to
include quick shortcuts to things like web bookmarks.  BANNER can be either
`official' for the official Emacs logo, `logo' for an alternative Emacs logo,
#f to hide the banner, or a custom file path to an image whose dimensions you
can constrain with BANNER-MAX-HEIGHT and BANNER-MAX-WIDTH.

Remind yourself of tasks by setting DASHBOARD-AGENDA-WEEKLY? to #t and customize
the format of task entries with DASHBOARD-AGENDA-PREFIX-FORMAT (see
@code{org-agenda-prefix-format} for information on the format strings).

You can truncate paths whose character length is greater than PATH-MAX-LENGTH."
  (ensure-pred file-like? emacs-dashboard)
  (ensure-pred boolean? show-on-startup?)
  (ensure-pred maybe-list? items)
  (ensure-pred maybe-list? item-generators)
  (ensure-pred maybe-list? item-shortcuts)
  (ensure-pred maybe-list? item-names)
  (ensure-pred maybe-list? navigator-buttons)
  (ensure-pred file-like-or-path-or-symbol-or-boolean? banner)
  (ensure-pred number? banner-max-height)
  (ensure-pred number? banner-max-width)
  (ensure-pred boolean? dashboard-agenda-weekly?)
  (ensure-pred maybe-string? dashboard-agenda-prefix-format)
  (ensure-pred integer? path-max-length)
  (ensure-pred string? dashboard-key)

  (define emacs-f-name 'dashboard)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'dashboard)
        (defun rde-dashboard-open ()
          "Jump to a dashboard buffer, creating one if it doesn't exist."
          (interactive)
          (when (get-buffer-create dashboard-buffer-name)
            (switch-to-buffer dashboard-buffer-name)
            (dashboard-mode)
            (dashboard-insert-startupify-lists)
            (dashboard-refresh-buffer)))

        ,@(if show-on-startup?
              '((add-hook 'after-init-hook 'rde-dashboard-open))
              '())

        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,dashboard-key) 'rde-dashboard-open))
        (with-eval-after-load 'dashboard
          (setq dashboard-center-content t))
        (with-eval-after-load 'dashboard-widgets
          (setq dashboard-bookmarks-show-base nil)
          (setq dashboard-projects-backend 'project-el)
          (setq dashboard-path-max-length ,path-max-length)
          (setq dashboard-path-style 'truncate-beginning)
          (setq dashboard-set-init-info nil)
          (setq dashboard-set-heading-icons nil)
          (setq dashboard-set-file-icons nil)
          (setq dashboard-set-footer nil)
          ,@(if items
                `((setq dashboard-items ',items))
                '())
          ,@(if item-generators
                `((setq dashboard-item-generators ',item-generators))
                '())
          ,@(if item-shortcuts
                `((setq dashboard-item-generators ',item-shortcuts))
                '())
          ,@(if item-names
                `((setq dashboard-item-generators ',item-names))
                '())
          ,@(if (symbol? banner)
                `((setq dashboard-startup-banner ',banner))
                `((setq dashboard-startup-banner ,(match banner
                                                    (#f 'nil)
                                                    (e e)))))
          (setq dashboard-banner-logo-title "")
          (setq dashboard-image-banner-max-height ,banner-max-height)
          (setq dashboard-image-banner-max-width ,banner-max-width)
          ,@(if (get-value 'emacs-advanced-user? config)
                '((setq dashboard-show-shortcuts nil))
                '())
          ,@(if (and (get-value 'emacs-org-agenda config)
                     dashboard-agenda-weekly?)
                '((setq dashboard-week-agenda t))
                '())
          (setq dashboard-agenda-release-buffers t)
          ,@(if dashboard-agenda-prefix-format
                `((setq dashboard-agenda-prefix-format
                        ,dashboard-agenda-prefix-format))
                '())
          ,@(if navigator-buttons
                `((setq dashboard-set-navigator t)
                  (setq dashboard-navigator-buttons ',navigator-buttons))
                '())))
      #:summary "Minimalist defaults for the extensible Emacs dashboard"
      #:keywords '(applications)
      #:commentary "Removes icons and visual clutter for a simpler\
Emacs dashboard which allows you to focus on the section items."
      #:elisp-packages (list emacs-dashboard))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-dashboard)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-emms
          #:key
          (emacs-emms emacs-emms)
          (emms-info-method 'emms-info-libtag)
          (emms-key "E"))
  "Configure the Emacs MultiMedia System.  Choose what method to use to
retrieve information about tracks via EMMS-INFO-METHOD."
  (ensure-pred file-like? emacs-emms)
  (ensure-pred symbol? emms-info-method)
  (ensure-pred string? emms-key)

  (define emacs-f-name 'emms)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EMMS."
    (define atomicparsley-bin
      (file-append
       (get-value 'atomicparsley config
                  (@ (gnu packages video) atomicparsley))
       "/bin/AtomicParsley"))

    (require-value 'xdg-user-directories-configuration config)
    (define music-dir
      (home-xdg-user-directories-configuration-music
       (get-value 'xdg-user-directories-configuration config)))
    (define emacs-ytdl (get-value 'emacs-ytdl config))

    (append
     (list
      (rde-elisp-configuration-service
       emacs-f-name
       config
       `((eval-when-compile
           (require 'emms))
         (defvar rde-emms-map nil
          "Map to bind `emms' commands under.")
         (define-prefix-command 'rde-emms-map)

         (defun rde-emms-source-playlist ()
           "Load a playlist with media files from emms' default directory."
           (interactive)
           (require 'emms)
           (ignore-errors
             (if (not emms-playlist-buffer)
                 (progn
                   (emms-add-directory-tree emms-source-file-default-directory)
                   (emms-playlist-mode-go))
               (with-current-emms-playlist
                 (unless (emms-playlist-selected-track)
                   (emms-add-directory-tree
                    emms-source-file-default-directory))
                 (emms-playlist-mode-go)))))

         (defun rde-emms-source-track (url title &optional length play)
           "Append or PLAY track with URL, TITLE, and LENGTH to the playlist."
           (interactive "sURL: \nsTitle: \nP")
           (if length
               (emms-add-rde-emms-track url title length)
             (emms-add-rde-emms-track url title nil))
           (when play
             (emms-stop)
             (emms-playlist-current-select-last)
             (emms-start)))

         (defun rde-emms-toggle-random-repeat ()
           "Toggle the random and repeat state for the current EMMS playlist."
           (interactive)
           (emms-toggle-random-playlist)
           (if (and emms-repeat-track emms-random-playlist)
               (progn
                 (setq emms-repeat-track nil)
                 (message "Will play tracks randomly and repeat the track"))
             (setq emms-repeat-track t)
             (message "Will play tracks sequentially and repeat the track")))

         (defun rde-emms-seek-to-beginning ()
           "Seek to beginning of current EMMS track."
           (interactive)
           (emms-seek-to 0))

         (defun rde-emms-next ()
           "Move to the next track depending on the current playlist state."
           (interactive)
           (if emms-random-playlist
               (emms-random)
             (emms-next)))

         (defun rde-emms-previous ()
           "Move to the previous track based on the current playlist state."
           (interactive)
           (if emms-random-playlist
               (emms-random)
             (emms-previous)))

         (define-emms-source rde-emms-track (url title &optional length)
           (let ((emms-track (emms-track 'url url)))
             (emms-track-set emms-track 'info-title title)
             (when length
               (emms-track-set emms-track 'info-playing-time length))
             (emms-playlist-insert-track emms-track)))

         ,@(if emacs-ytdl
             '((eval-when-compile
                 (require 'ytdl))

               (with-eval-after-load 'emms-playlist-mode
                 (defun rde-emms-download-track ()
                   "Download EMMS track at point using `ytdl'."
                   (interactive)
                   (emms-playlist-ensure-playlist-buffer)
                   (with-current-emms-playlist
                     (let* ((dl-type (ytdl--get-download-type))
                            (track (emms-playlist-track-at))
                            (title (emms-track-get track 'info-title))
                            (source (emms-track-get track 'name)))
                       (if (equal (emms-track-get track 'type) 'url)
                           (ytdl--download-async
                            source
                            (expand-file-name
                             title (ytdl--eval-field (nth 1 dl-type)))
                            (ytdl--eval-list
                             (ytdl--eval-field (nth 2 dl-type)))
                            'ignore
                            (car dl-type))
                         (error "Track `%s' is not a remote track to download"
                                title)))))
                 (define-key emms-playlist-mode-map "m"
                   'rde-emms-download-track)))
             '())

         ,@(if (get-value 'emacs-dired config)
               '((with-eval-after-load 'dired
                   (define-key dired-mode-map "e" 'emms-play-dired)))
               '())

         (with-eval-after-load 'rde-keymaps
           (define-key rde-app-map (kbd ,emms-key) 'rde-emms-map))

         (let ((map rde-emms-map))
           (define-key map "b" 'emms-smart-browse)
           (define-key map "h" 'emms-history-save)
           (define-key map "q" 'emms-stop)
           (define-key map "s" 'emms-toggle-random-playlist)
           (define-key map "t" 'emms-seek-to)
           (define-key map (kbd "SPC") 'emms-pause)
           (define-key map "r" 'emms-toggle-repeat-track)
           (define-key map "R" 'emms-toggle-repeat-playlist)
           (define-key map "d" 'rde-emms-toggle-random-repeat)
           (define-key map "l" 'rde-emms-source-playlist)
           (define-key map "n" 'rde-emms-next)
           (define-key map "p" 'rde-emms-previous)
           (define-key map "a" 'rde-emms-seek-to-beginning))

         (with-eval-after-load 'emms
           (require 'emms-setup)
           (require 'xdg)
           (require 'env)
           (require ',emms-info-method)

           ,@(if (get-value 'mpv config)
                 '((require 'emms-player-mpv)
                   (setq emms-player-list '(emms-player-mpv))
                   (add-to-list 'emms-player-mpv-parameters
                                "--ytdl-format=best")
                   (add-to-list 'emms-player-mpv-parameters
                                "--force-window=no"))
                 '())

           (with-eval-after-load 'emms-tag-editor
             (let ((mp3-function (assoc "mp3"
                                        emms-tag-editor-tagfile-functions)))
               (add-to-list
                'emms-tag-editor-tagfile-functions
                `("aac" ,(cadr mp3-function) ,(caddr mp3-function))))
             (add-to-list
              'emms-tag-editor-tagfile-functions
              '("m4a" ,atomicparsley-bin
                ((info-artist . "--artist")
                 (info-title . "--title")
                 (info-album . "--album")
                 (info-tracknumber . "--tracknum")
                 (info-year . "--year")
                 (info-genre . "--genre")
                 (info-note . "--comment")
                 (info-albumartist . "--albumArtist")
                 (info-composer . "--composer")))))

           (setq emms-playlist-buffer-name "*EMMS Playlist*")
           (setq emms-playlist-mode-center-when-go t)
           (setq emms-history-file
                 (expand-file-name "emacs/emms-history"
                                   (or (xdg-cache-home) "~/.cache")))
           (setq emms-seek-seconds 15)
           (setq emms-source-file-default-directory
                 (substitute-env-vars ,music-dir))
           (setq emms-repeat-playlist t)
           (setq emms-info-functions '(,emms-info-method))
           (setq emms-mode-line-format "%s")
           (setq emms-mode-line-icon-enabled-p nil)

           (with-eval-after-load 'emms-browser
             (eval-when-compile
              (require 'emms-browser))
             (emms-browser-make-filter
              "all-files" (emms-browser-filter-only-type 'file))
             (emms-browser-make-filter
              "last-week" (emms-browser-filter-only-recent 7))
             (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
             (setq emms-browser-switch-to-playlist-on-add t)
             (setq emms-browser-thumbnail-small-size 64)
             (setq emms-browser-thumbnail-medium-size 128))))
       #:elisp-packages (append
                         (or (and=> emacs-ytdl list) '())
                         (list emacs-emms))
       #:summary "Sensible defaults for EMMS"
       #:commentary "Basic setup and extensions for EMMS, the Emacs MultiMedia \
System."))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-emms)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-pulseaudio-control
          #:key
          (emacs-pulseaudio-control emacs-pulseaudio-control)
          (volume-step "5%")
          (display-volume? #f)
          (pulseaudio-key "v"))
  "Configure pulseaudio-control for PulseAudio integration in Emacs."
  (ensure-pred file-like? emacs-pulseaudio-control)
  (ensure-pred string? volume-step)
  (ensure-pred boolean? display-volume?)
  (ensure-pred string? pulseaudio-key)

  (define emacs-f-name 'pulseaudio-control)
  (define f-name (symbol-append 'emacs emacs-f-name))

  (define (get-home-services config)
    "Return home services related to PulseAudio Control."
    (define pactl
      (file-append
       (get-value 'pulseaudio config
                  (@ (gnu packages pulseaudio) pulseaudio))
       "/bin/pactl"))

    (let ((emacs-all-the-icons (get-value 'emacs-all-the-icons config)))
      (list
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((with-eval-after-load 'rde-keymaps
            (define-key rde-app-map (kbd ,pulseaudio-key) 'pulseaudio-control-map))
          ,@(if display-volume?
                '((add-hook 'after-init-hook 'pulseaudio-control-display-mode))
                '())
          (with-eval-after-load 'pulseaudio-control
            (define-key pulseaudio-control-map "L"
              'pulseaudio-control-toggle-sink-input-mute-by-index)
            ,@(if emacs-all-the-icons
                  '((eval-when-compile
                     (require 'all-the-icons))
                    (with-eval-after-load 'all-the-icons
                      (let ((all-the-icons-default-adjust -0.15))
                        (setq pulseaudio-control-sink-mute-string
                              (all-the-icons-material "volume_off" :height 1))
                        (setq pulseaudio-control-sink-volume-strings
                              (list
                               (all-the-icons-material "volume_mute" :height 1)
                               (all-the-icons-material "volume_down" :height 1)
                               (all-the-icons-material "volume_up" :height 1)))
                        (setq pulseaudio-control-source-mute-string
                              (all-the-icons-material "mic_off" :height 1))
                        (setq pulseaudio-control-source-volume-strings
                              (list
                               (all-the-icons-material "mic_none" :height 1)
                               (all-the-icons-material "mic" :height 1))))))
                  '())
            (setq pulseaudio-control-pactl-path ,pactl)
            (setq pulseaudio-control--volume-maximum '(("percent" . 100)
                                                       ("decibels" . 10)
                                                       ("raw" . 98000)))
            (setq pulseaudio-control-volume-step ,volume-step)
            (setq pulseaudio-control-volume-verbose nil)
            (pulseaudio-control-default-sink-mode)
            (pulseaudio-control-default-source-mode)))
        #:elisp-packages (append (list emacs-pulseaudio-control)
                                 (or (and=> emacs-all-the-icons list) '()))))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-pulseaudio-control)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-webpaste
          #:key
          (emacs-webpaste emacs-webpaste)
          (webpaste-providers '("bpa.st" "paste.mozilla.org" "ix.io"))
          (webpaste-key "P"))
  "Configure Webpaste.el, a mode to paste whole buffers or
parts of buffers to pastebin-like services.
WEBPASTE-PROVIDERS denotes the list of providers that will
be used in descending order of priority."
  (ensure-pred file-like? emacs-webpaste)
  (ensure-pred list? webpaste-providers)
  (ensure-pred string? webpaste-key)

  (define emacs-f-name 'webpaste)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Webpaste."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defvar rde-webpaste-map nil
          "Map to bind `webpaste' commands under.")
        (define-prefix-command 'rde-webpaste-map)
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,webpaste-key) 'rde-webpaste-map))
        (let ((map rde-webpaste-map))
          (define-key map "b" 'webpaste-paste-buffer)
          (define-key map "r" 'webpaste-paste-region)
          (define-key map "p" 'webpaste-paste-buffer-or-region))
        (with-eval-after-load 'webpaste
          (setq webpaste-provider-priority ',webpaste-providers)
          (setq webpaste-paste-confirmation t)))
      #:elisp-packages (list emacs-webpaste))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-webpaste)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-display-wttr
          #:key
          (emacs-display-wttr emacs-display-wttr)
          (wttr-format "%c %t")
          (wttr-locations '())
          (wttr-interval 3600))
  "Configure the display of weather in Emacs.  If you don't provide
WTTR-LOCATIONS you will get a weather report based on your IP address."
  (ensure-pred file-like? emacs-display-wttr)
  (ensure-pred string? wttr-format)
  (ensure-pred list? wttr-locations)
  (ensure-pred integer? wttr-interval)

  (define emacs-f-name 'display-wttr)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to display-wttr."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((setq display-wttr-format ,wttr-format)
        (setq display-wttr-locations ',wttr-locations)
        (setq display-wttr-interval ,wttr-interval)
        (autoload 'display-wttr-mode "display-wttr")
        (display-wttr-mode))
      #:elisp-packages (list emacs-display-wttr))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-display-wttr)))
   (home-services-getter get-home-services)))

;;; emacs-xyz.scm end here
