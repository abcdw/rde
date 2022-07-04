;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
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

(define-module (rde features emacs)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)

  #:export (feature-emacs
            feature-emacs-portable
            feature-emacs-appearance
            feature-emacs-faces
            feature-emacs-completion
            feature-emacs-vertico
            feature-emacs-mct
            feature-emacs-input-methods
            feature-emacs-project
            feature-emacs-perspective
            feature-emacs-git
            feature-emacs-dired
            feature-emacs-eshell
            feature-emacs-monocle
            feature-emacs-org
            feature-emacs-org-roam
            feature-emacs-org-agenda
            feature-emacs-erc
            feature-emacs-elpher
            feature-emacs-telega
            feature-emacs-pdf-tools
            feature-emacs-nov-el
            feature-emacs-which-key
            feature-emacs-keycast
            feature-emacs-eglot
            feature-emacs-smartparens

            rde-elisp-configuration-service
            emacs-xdg-service))


;;;
;;; rde emacs utilities.
;;;

(define* (rde-emacs-configuration-package
          name
          #:optional (elisp-expressions '())
          #:key
          summary authors url keywords commentary
          (elisp-packages '())
          (autoloads? #f))
  "Returns a package, which configures emacs.  Can be used as a
dependency for other packages."
    (let* ((configure-package
          (elisp-configuration-package
           (string-append "configure-" (symbol->string name))
           elisp-expressions
           #:elisp-packages elisp-packages
           #:autoloads? autoloads?
           #:summary summary
           #:commentary commentary
           #:keywords keywords
           #:url (or url "https://trop.in/rde")
           #:authors (or authors '("Andrew Tropin <andrew@trop.in>")))))
      configure-package))

(define* (rde-elisp-configuration-service
          name config
          #:optional (elisp-expressions '())
          #:key
          summary authors url keywords commentary
          (early-init '())
          (elisp-packages '()))
  "Adds a configure-NAME package to the profile and emacs load path and if
emacs-portable? rde value is present adds autoloads cookies to each expression
of it, otherwise adds a require to @file{init.el}."
  (let* ((pkg-name (symbol-append 'configure- name))
         (autoloads? (get-value 'emacs-portable? config))
         (require-in-init? (not autoloads?))
         (configure-package
          (rde-emacs-configuration-package
           name elisp-expressions
           #:summary summary
           #:commentary commentary
           #:keywords keywords
           #:url url
           #:authors authors
           #:elisp-packages elisp-packages
           #:autoloads? autoloads?)))
    (simple-service
     (symbol-append 'emacs- name '-configurations)
     home-emacs-service-type
     (home-emacs-extension
      (init-el (if require-in-init? `((require ',pkg-name)) '()))
      (early-init-el early-init)
      ;; It's necessary to explicitly add elisp-packages here, because
      ;; we want to overwrite builtin emacs packages.  Propagated
      ;; inputs have lowest priority on collisions, that's why we have
      ;; to list those package here in addition to propagated-inputs.
      (elisp-packages (append elisp-packages (list configure-package)))))))

;; MAYBE: make handler to be actions instead of desktop entries?
(define* (emacs-xdg-service
          name xdg-name gexp
          #:key
          (default-for '())
          (exec-argument "%u"))
  (define file-name (string-append "emacs-" (symbol->string name)))
  (define file-file (file-append (program-file file-name gexp)
                                 (string-append " " exec-argument)))
  (define desktop-file (symbol-append 'emacs- name '.desktop))
  (simple-service
   (symbol-append 'emacs-xdg- name)
   home-xdg-mime-applications-service-type
   (home-xdg-mime-applications-configuration
    (default (map (lambda (m) (cons m desktop-file)) default-for))
    (desktop-entries
     (list
      (xdg-desktop-entry
       (file file-name)
       (name xdg-name)
       (config `((exec . ,file-file)
                 (icon . "emacs")))
       (type 'application)))))))


;;;
;;; Emacs features.
;;;

(define emacs-configure-rde-keymaps
  (rde-emacs-configuration-package
   'rde-keymaps
   `((defvar rde-app-map nil "Prefix keymap for applications.")
     (define-prefix-command 'rde-app-map nil)
     (defvar rde-toggle-map nil "\
Prefix keymap for binding various minor modes for toggling functionalitty.")
     (define-prefix-command 'rde-toggle-map nil))
   #:summary "Keymaps inteded for reuse among configure-* packages"))

;; "#f0d3ff" ;; magenta
;; "#c0efff" ;; cyan
;; "#b5d0ff" ;; blue
;; "#aecf90" ;; green
;; "#f2b0a2" ;; red

(define* (feature-emacs-portable
          #:key
          (emacs emacs-next-pgtk-latest)
          (status-line-bg-color "#b5d0ff")
          (additional-elisp-packages '()))
  (ensure-pred maybe-string? status-line-bg-color)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)
  (ensure-pred file-like? emacs)

  (define (emacs-home-services config)
    "Returns home services related to GNU Emacs, which usually used in development
environment outside of Guix Home."
    (list
     (service
      home-emacs-service-type
      (home-emacs-configuration
       (package emacs)
       (elisp-packages additional-elisp-packages)
       ;;; TODO: Rebuilding packages with emacs will be useful for
       ;;; native-comp, but for some reason dash.el fails to build,
       ;;; need to investigate the issue.
       ;; (rebuild-elisp-packages? #t)
       ))
     (rde-elisp-configuration-service
      'rde-emacs-portable
      config
      `(,@(if status-line-bg-color
              `((with-eval-after-load
                 'configure-appearance
                 (setq rde-status-line-bg-color ,status-line-bg-color)))
              '())))))
  (feature
   (name 'emacs)
   (values (append (make-feature-values emacs emacs-configure-rde-keymaps)
                   `((emacs-portable? . #t))))
   (home-services-getter emacs-home-services)))

(define* (feature-emacs
          #:key
          (emacs emacs-next-pgtk-latest)
          (emacs-server-mode? #t)
          (additional-elisp-packages '())
          (extra-init-el '())
          (extra-early-init-el '())
          (default-terminal? #t))
  "Setup and configure GNU Emacs."
  (ensure-pred boolean? emacs-server-mode?)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)
  (ensure-pred any-package? emacs)

  (define emacs-client (file-append emacs "/bin/emacsclient"))
  (define emacs-client-create-frame
    (program-file "emacs-client-create-frame"
                  #~(apply system*
                           #$(file-append emacs "/bin/emacsclient")
                           "--create-frame"
                           (cdr (command-line)))))
  (define emacs-client-no-wait
    (program-file "emacs-client-no-wait"
                  #~(apply system*
                           #$(file-append emacs "/bin/emacsclient")
                           "--no-wait"
                           (cdr (command-line)))))
  (define emacs-editor
    (program-file "emacs-editor"
                  #~(apply system*
                           #$(file-append emacs "/bin/emacs")
                           "--no-splash"
                           (cdr (command-line)))))

  (define (emacs-home-services config)
    "Returns home services related to GNU Emacs."
    (require-value 'full-name config)
    (require-value 'email config)
    (let* ((full-name (get-value 'full-name config))
           (email     (get-value 'email config)))
      (list
       (emacs-xdg-service 'emacs-q "Emacs (No init: -q)"
                          #~(system* "emacs" "-q"))
       (emacs-xdg-service 'emacs-Q "Emacs (No init, no site-lisp: -Q)"
                          #~(system* "emacs" "-Q"))

       (rde-elisp-configuration-service
        'rde-emacs
        config
        `((require 'configure-rde-keymaps)

          (setq user-full-name ,full-name)
          (setq user-mail-address ,email)

          ,#~""
          (setq custom-file
                (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                        "/emacs/custom.el"))
          (load custom-file t)

          (customize-set-variable
           'backup-directory-alist
           `(,(cons "." (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                "/emacs/backup"))))

          (customize-set-variable
           'recentf-save-file
           (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                   "/emacs/recentf"))

          ;; (add-hook 'after-init 'recentf-mode)
          (recentf-mode 1)
          (run-with-idle-timer 30 t 'recentf-save-list)

          ;; (customize-set-variable 'history-length 10000)
          (customize-set-variable
           'savehist-file (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                  "/emacs/history"))

          (savehist-mode 1)
          (run-with-idle-timer 30 t 'savehist-save)

          (customize-set-variable
           'bookmark-default-file
           (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                   "/emacs/bookmarks"))

          ,#~""
          (pixel-scroll-precision-mode 1)
          (column-number-mode 1)
          (save-place-mode 1)
          ;; MAYBE: Make it buffer local?
          (show-paren-mode 1)
          ,#~";; Treat camelCased parts as separate words."
          (subword-mode 1)
          ;; (add-hook 'prog-mode-hook 'subword-mode)

          (setq-default indent-tabs-mode nil)
          (setq save-interprogram-paste-before-kill t)
          (setq mouse-yank-at-point t)
          (setq require-final-newline t)
          (add-hook 'prog-mode-hook
                    (lambda () (setq show-trailing-whitespace t)))

          ;; Highlight zero-width whitespaces and other glypless characters.
          (set-face-background 'glyphless-char "red")
          ,#~""
          (define-key global-map (kbd "C-=") 'er/expand-region)

          ,#~""
          (defun rde-display-load-time ()
            (interactive)
            (message "\
rde emacs loaded in %s, C-h r i for search in emacs manual by topic. \
C-h C-a to open About Emacs buffer."
                     (emacs-init-time)))

          (defun display-startup-echo-area-message ()
            (rde-display-load-time))

          ,#~""
          ;; TODO: Move it to feature-isearch
          (setq search-whitespace-regexp ".*?")

          ,#~""
          ;; TODO: Move it to feature-compile
          (defun rde-compilation-colorizer ()
            "Prevent color escape sequences to popup in compilation buffer."
            (ansi-color-apply-on-region (point-min) (point-max)))
          (add-hook 'compilation-filter-hook 'rde-compilation-colorizer)

          (dolist (mode-hook '(prog-mode-hook compilation-mode-hook))
                  (add-hook mode-hook (lambda () (setq truncate-lines t))))
          (setq compilation-scroll-output 'first-error)
          (define-key global-map (kbd "s-r") 'recompile)

          ,#~""
          ;; TODO: Move it to feature-emacs-tramp
          (eval-when-compile (require 'tramp))
          (with-eval-after-load
           'tramp
           (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

          ,#~""
          ,@(if (get-value 'emacs-advanced-user? config)
                '((put 'narrow-to-page   'disabled nil)
                  (put 'narrow-to-region 'disabled nil))
                '())

          ,#~""
          ;; TODO: Extend info-lookup-alist with Guix Manual nodes to
          ;; make `C-h S' find guix services and other items.
          (eval-when-compile (require 'guix))
          (define-key rde-toggle-map (kbd "p") 'guix-prettify-mode)
          (define-key rde-toggle-map (kbd "P") 'global-guix-prettify-mode)
          (global-guix-prettify-mode 1)

          ,#~""
          (eval-when-compile
           (require 'time))
          (setq world-clock-list
                '(("America/Los_Angeles" "Los Angeles")
                  ("America/Boise" "Boise")
                  ("America/New_York" "New York")
                  ("UTC" "UTC")
                  ("Europe/London" "London")
                  ("Europe/Paris" "Paris")
                  ("Europe/Helsinki" "Helsinki")
                  ("Europe/Moscow" "Moscow")
                  ("Asia/Tokyo" "Tokyo")))
          (define-key rde-app-map (kbd "w") 'world-clock)

          ;; TODO: Move to feature-sane-bindings
          (let ((map goto-map))
            (define-key map "L" 'find-library)
            (define-key map "F" 'find-function)
            (define-key map "K" 'find-function-on-key)
            (define-key map "V" 'find-variable))

          (defun rde-kill-region-dwim (&optional count)
            "The function kills region if mark is active, otherwise kills a word.
Prefix argument can be used to kill a few words."
            (interactive "p")
            (if (use-region-p)
                (kill-region (region-beginning) (region-end) 'region)
                (backward-kill-word count)))

          ;; (define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
          (define-key global-map (kbd "M-K") 'kill-whole-line)
          (define-key global-map (kbd "M-c") 'capitalize-dwim)
          (define-key global-map (kbd "M-l") 'downcase-dwim)
          (define-key global-map (kbd "M-u") 'upcase-dwim)
          (define-key global-map (kbd "C-w") 'rde-kill-region-dwim)

          (define-key mode-specific-map (kbd "a")
            '("rde applications" . rde-app-map))
          (define-key mode-specific-map (kbd "t")
            '("rde toggles" . rde-toggle-map)))
        #:summary "General settings, better defaults"
        #:commentary "\
It can contain settings not yet moved to separate features."
        #:keywords '(convenience)
        #:elisp-packages (list (get-value 'emacs-configure-rde-keymaps config)
                               emacs-expand-region emacs-guix))

       (service
        home-emacs-service-type
        (home-emacs-configuration
         (package emacs)
         (elisp-packages additional-elisp-packages)
         (server-mode? emacs-server-mode?)
         (xdg-flavor? #t)
         (init-el extra-init-el)
         (early-init-el
          `(,(slurp-file-gexp (local-file "./emacs/early-init.el"))
            ,@extra-early-init-el))
         ;;; TODO: Rebuilding packages with emacs will be useful for
         ;;; native-comp, but for some reason dash.el fails to build,
         ;;; need to investigate the issue.
         ;; (rebuild-elisp-packages? #t)
         ))

       (simple-service 'emacs-set-default-editor
                       home-environment-variables-service-type
                       `(("ALTERNATE_EDITOR" . ,emacs-editor)
                         ("VISUAL" . ,emacs-client-no-wait)))
       (when (get-value 'sway config)
         (simple-service
          'emacs-update-environment-variables-on-sway-start
          home-sway-service-type
          `((,#~"")
            (exec_always "sleep 2s && " ;; Need to wait until emacs daemon loaded.
             ,(program-file
               "update-emacs-env-variables"
               #~(system*
                  #$emacs-client "--eval"
                  (string-append
                   "(mapcar (lambda (lst) (apply #'setenv lst)) '"
                   (let* ((port   ((@ (ice-9 popen) open-input-pipe)
                                   (string-append "env")))
                          (result ((@ (ice-9 rdelim) read-delimited) "" port))
                          (vars (map (lambda (x)
                                       (let ((si (string-index x #\=)))
                                         (list (string-take x si)
                                               (string-drop x (+ 1 si)))))
                                     ((@ (srfi srfi-1) remove)
                                      string-null? (string-split
                                                    result #\newline)))))
                     (close-port port)
                     (format #f "~s" vars))
                   ")"))))
            (for_window "[title=\".* - Emacs Client\"]"
                        floating enable,
                        resize set 80 ppt 80 ppt)))))))

  (feature
   (name 'emacs)
   (values (append
            (make-feature-values
             emacs
             emacs-editor emacs-client
             emacs-client-create-frame
             emacs-client-no-wait
             emacs-configure-rde-keymaps
             emacs-server-mode?)
            (if default-terminal?
                `((default-terminal . ,emacs-client-create-frame))
                '())))
   (home-services-getter emacs-home-services)))

(define* (feature-emacs-appearance
          #:key
          (margin 8)
          (deuteranopia? #t)
          (dark? #f)
          (header-line-as-mode-line? #t)
          (emacs-modus-themes emacs-modus-themes))
  "Make Emacs looks modern and minimalistic. `deuteranopia?' substitutes
red/green colors with red/blue, which helps people with colorblindness
and overall looks cool."
  (ensure-pred integer? margin)
  (ensure-pred boolean? deuteranopia?)

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

        (require 'modus-themes)
        ;; Doesn't see the effect.

        (setq modus-themes-mode-line '(borderless))
        (setq modus-themes-diffs 'desaturated)
        (setq modus-themes-deuteranopia ,(if deuteranopia? 't 'nil))
        (setq modus-themes-operandi-color-overrides
              `((fg-window-divider-inner . "#ffffff")
                (fg-window-divider-outer . "#ffffff")))
        (setq modus-themes-vivendi-color-overrides
              `((fg-window-divider-inner . "#000000")
                (fg-window-divider-outer . "#000000")))

        (defun rde-modus-themes-custom-faces ()
          (modus-themes-with-colors
           (custom-set-faces
            ;; Replace green with blue if you use `modus-themes-deuteranopia'.
            `(git-gutter-fr:added ((,class :foreground ,,(if deuteranopia?
                                                             'blue-fringe-bg
                                                             'green-fringe-bg)
                                     :background ,bg-main)))
            `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg
                                       :background ,bg-main)))
            `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg
                                        :background ,bg-main))))))

        (add-hook 'modus-themes-after-load-theme-hook
                  'rde-modus-themes-custom-faces)

        ,@(if dark?
              '((modus-themes-load-vivendi))
              '((modus-themes-load-operandi)))

        (with-eval-after-load
         'configure-rde-keymaps
         (define-key rde-toggle-map (kbd "t") 'modus-themes-toggle))

        (setq bookmark-set-fringe-mark nil)

        ;; (setq header-line-format (delete 'mode-line-modes header-line-format))
        (setq mode-line-modes
              (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
                (list (propertize "%[" 'help-echo recursive-edit-help-echo)
                      "("
                      `(:propertize ("" mode-name)
                                    help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                                    mouse-face mode-line-highlight
                                    local-map ,mode-line-major-mode-keymap)
                      '("" mode-line-process)
                      ")"
                      (propertize "%]" 'help-echo recursive-edit-help-echo)
                      " ")))

        ,@(if header-line-as-mode-line?
              `(,#~"\n;; Move modeline to the top"
                (setq-default header-line-format mode-line-format)
                (setq-default mode-line-format nil)

                (defun rde--move-mode-line-to-header ()
                  "Moves mode-line to header-line, the function is needed for
various modes, which setups mode-line late."
                  (setq-local header-line-format mode-line-format)
                  (setq-local mode-line-format nil))

                (add-hook 'calendar-initial-window-hook
                          'rde--move-mode-line-to-header))
              '())

        (defvar rde-status-line-bg-color
          nil
          "A string containing a color, which will be used to set initial
value of background color for mode/header-line.")

        (add-hook 'after-init-hook
                  (lambda ()
                    (when rde-status-line-bg-color
                      (set-face-attribute
                       ',(if header-line-as-mode-line?
                             'header-line
                             'mode-line)
                       nil :background rde-status-line-bg-color))))

        (menu-bar-mode 0)
        (tool-bar-mode 0)
        (scroll-bar-mode 0)

        (set-frame-parameter (selected-frame) 'internal-border-width ,margin)

        (setq use-dialog-box nil)
        (setq use-file-dialog nil)

        (customize-set-variable 'window-divider-default-right-width ,margin)
        (window-divider-mode))

      #:early-init
      `(,#~"\n;; Disable ui elements, add margins."
        (push '(menu-bar-lines . 0) default-frame-alist)
        (push '(tool-bar-lines . 0) default-frame-alist)
        (push '(vertical-scroll-bars) default-frame-alist)
        (setq menu-bar-mode nil
              tool-bar-mode nil
              scroll-bar-mode nil)

        ,#~""
        (push '(internal-border-width . ,margin) default-frame-alist)
        ;; (setq-default fringes-outside-margins t)
        ;; (setq-default left-margin-width 1)
        ;; (setq-default right-margin-width 1)

        ,#~""
        ,@(if (get-value 'emacs-advanced-user? config)
              '((setq inhibit-startup-screen t))
              '()))
      #:keywords '(appearance mode-line faces accessibility)
      #:summary "\
Sets theme, fonts, faces and provides different visual tweaks"
      #:commentary "\
The goal is to provide non-distractive and safe visual design.

Modus operandi is light, high-contrast, calm, colorblind-friendly.
The light colorschemes are better for productivity according to
various researchs, more eye-friendly and works better with other apps
and media like PDFs, web pages, etc, which are also light by default.
Later here will be a link to rde manual with more in-depth explanation
with references to researches.

Modeline is simplified and moved to the top of the window.

Almost all visual elements are disabled."
      #:elisp-packages
      (list emacs-modus-themes
            (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-header-line-as-mode-line? . ,header-line-as-mode-line?)
             (emacs-margin . ,margin)))
   (home-services-getter get-home-services)))


(define (strip-emacs-name p)
  (let ((name (package-name p)))
    (string->symbol
     (if (string-prefix? "emacs-" name)
         (string-drop name (string-length "emacs-"))
         name))))

(define* (feature-emacs-input-methods
          #:key
          (toggle-keybinding "s-SPC")
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
         (define-key global-map (kbd ,toggle-keybinding) 'toggle-input-method))

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


(define* (feature-emacs-erc
          #:key
          ;; (emacs-client? #f)
          (erc-server "irc.libera.chat")
          (erc-port 6697)
          (erc-nick #f)
          (erc-autojoin-channels-alist '())
          (erc-kill-buffers-on-quit #t)
          (align-nicknames? #t)
          (log? #f)
          (extra-config '()))
  "Configure GNU Emacs IRC client."
  (ensure-pred string? erc-server)
  (ensure-pred integer? erc-port)
  (ensure-pred maybe-string? erc-nick)
  (ensure-pred list? erc-autojoin-channels-alist)

  (define emacs-f-name 'erc)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'erc-join)
         (require 'erc-fill)
         (require 'erc-track))
        (with-eval-after-load
         'erc-status-sidebar
         (setq-default erc-status-sidebar-header-line-format
                       (concat " " erc-status-sidebar-mode-line-format))
         (setq-default erc-status-sidebar-mode-line-format nil)
         (setq erc-status-sidebar-width 18))

        (with-eval-after-load
         'erc-log
         (setq erc-log-insert-log-on-open t)
         (setq erc-log-channels-directory
               (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                       "/emacs/erc-logs")))

        (with-eval-after-load
         'erc
         (setq erc-server ,erc-server)
         (setq erc-port ,erc-port)
         ,@(if erc-nick `((setq erc-nick ,erc-nick)) '())
         (setq erc-autojoin-channels-alist
               ',erc-autojoin-channels-alist)

         ,@(if align-nicknames?
               '((setq erc-fill-static-center 14)
                 (setq erc-fill-function 'erc-fill-static)
                 (setq erc-fill-column 82))
               '())

         (setq erc-hide-list '())
         (setq erc-track-exclude-types
               '("324" ; channel mode is
                 "329" ; creation time
                 ;; "332" "333" "353" "477"
                 ;; "MODE"
                 "JOIN" "PART" "QUIT"))
         ,@(if erc-kill-buffers-on-quit
               '((setq erc-kill-server-buffer-on-quit t)
                 ;; (setq erc-kill-buffer-on-part t)
                 (setq erc-kill-queries-on-quit t))
               '())

         ,@(if log? '((add-to-list 'erc-modules 'log)) '())
         ;; (erc-update-modules) Probably not needed, because the module
         ;; added before erc starts.

         (setq erc-header-line-format " %n on %t (%m,%l)"))

        ,@extra-config)
            #:summary "\
Reasonable defaults for ERC and some enhancements"
      #:commentary "\
Less verbose output, nicks highlighted with different colors."
      #:keywords '(convenience)
      #:elisp-packages (list emacs-erc-hl-nicks))
     (emacs-xdg-service
      emacs-f-name
      "Emacs (Client) [IRC]"
      #~(system* #$emacs-cmd "--eval" "(erc-tls)"))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-telega
          #:key
          (emacs-telega emacs-telega-latest)
          (emacs-telega-contrib emacs-telega-contrib-latest))
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
         (require 'company))

        (require 'configure-rde-keymaps)
        (define-key rde-app-map (kbd "t")
          (cons "telega-prefix" telega-prefix-map))

        (with-eval-after-load
         'telega

         (define-key telega-chat-mode-map (kbd "s-B") 'telega-chat-with)
         (define-key telega-root-mode-map (kbd "s-B") 'telega-chat-with)
         (setq telega-emoji-company-backend 'telega-company-emoji)
         ,@(if (get-value 'mpv config)
               `((setq telega-video-player-command
                       ,(file-append (get-value 'mpv config) "/bin/mpv")))
               '())
         (defun rde-telega-chat-mode ()
           (set (make-local-variable 'company-backends)
                (append (list telega-emoji-company-backend
                              'telega-company-username
                              'telega-company-hashtag)
                        (when (telega-chat-bot-p telega-chatbuf--chat)
                          '(telega-company-botcmd))))
           (company-mode 1))
         (add-hook 'telega-chat-mode-hook 'rde-telega-chat-mode)

         (setq telega-completing-read-function completing-read-function)))
            #:summary "\
Telegram client in Emacs"
      #:commentary "\
A few keybindings and small adjustments."
      #:keywords '(convenience faces)
      #:elisp-packages (list emacs-telega emacs-telega-contrib
                             (get-value 'emacs-configure-rde-keymaps config)))

     (emacs-xdg-service emacs-f-name "Emacs (Client) [tg:]" xdg-gexp
                        #:default-for '(x-scheme-handler/tg))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-pdf-tools)
  "Configure pdf-tools, to work with pdfs inside Emacs."
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
      `((customize-set-variable 'pdf-view-use-scaling t)
        (autoload 'pdf-view-mode "pdf-view" "")
        (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
        (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
        (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
        (with-eval-after-load
         'saveplace
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
         (customize-set-variable 'hide-mode-line-excluded-modes '()))

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

        (require 'configure-rde-keymaps)
        (define-key rde-toggle-map (kbd "o") 'olivetti-mode)
        (define-key rde-toggle-map (kbd "O") 'global-olivetti-mode)
        (define-key rde-toggle-map (kbd "m") 'hide-mode-line-mode)
        (define-key rde-toggle-map (kbd "M") 'global-hide-mode-line-mode)
        (define-key global-map (kbd "s-f") 'rde-toggle-monocle))
      #:summary "\
Focused editing and reading"
      #:commentary "\
Various functions, keybindings and settings for creating distraction free
working environemnt."
      #:keywords '(convenience reading editing)
      #:elisp-packages (list emacs-olivetti emacs-hide-header-line
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (olivetti-body-width . ,olivetti-body-width)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-dired)
  "Configure dired, the Emacs' directory browser and editor."
  (define emacs-f-name 'dired)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         "--eval"
         (string-append
          "(dired \"" (car (cdr (command-line))) "\")")))
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile (require 'dired))
        (with-eval-after-load
         'dired
         (setq dired-dwim-target t)
         ,@(if (get-value 'emacs-advanced-user? config)
               '((add-hook 'dired-mode-hook 'dired-hide-details-mode)
                 (setq dired-listing-switches "-l --time-style=long-iso -h -A"))
               '())

         (add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))
         (setq dired-hide-details-hide-symlink-targets nil)))
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

(define* (feature-emacs-eshell)
  "Configure Eshell, the Emacs shell."
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
        (defun rde-project-eshell-or-eshell (&optional arg)
          "If there is a project open project-eshell"
          (interactive "P")
          (if (project-current)
              (project-eshell)
              (eshell arg)))

        (define-key global-map (kbd "s-e") 'eshell)
        (with-eval-after-load
         'eshell
         (add-hook
          'eshell-hist-mode-hook
          (lambda ()
            (when (fboundp 'consult-history)
              (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history))))

         ;;; <https://www.emacswiki.org/emacs/AnsiColor#h5o-2>
         (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

         (defun switch-to-prev-buffer-or-eshell (arg)
           (interactive "P")
           (if arg
               (eshell arg)
               (switch-to-buffer (other-buffer (current-buffer) 1))))

         (add-hook
          'eshell-mode-hook
          (lambda ()
            (if envrc-global-mode
                (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
                (setenv "PAGER" ""))

            (eshell/alias "e" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            (eshell/alias "d" "dired $1")
            (with-eval-after-load
             'magit (eshell/alias "gd" "magit-diff-unstaged"))

            (define-key eshell-mode-map (kbd "s-e")
              'switch-to-prev-buffer-or-eshell)))))
      #:summary "\
Eshell configurations, aliases, tweaks"
      #:commentary "\
Aliases, keybindings, small hack and tweaks."
      #:keywords '(convenience eshell))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org
          #:key
          (org-directory "~/org")
          (org-rename-buffer-to-title #t))
  "Configure org-mode for GNU Emacs."
  (define emacs-f-name 'org)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'org)
         (require 'org-refile))

        (define-key mode-specific-map (kbd "c") 'org-capture)

        (with-eval-after-load
         'org
         (setq org-adapt-indentation nil)
         (setq org-edit-src-content-indentation 0)
         (setq org-startup-indented t)

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

         ,@(when org-rename-buffer-to-title
             '((add-hook 'org-mode-hook 'rde-buffer-name-to-title-config)))

         (with-eval-after-load 'notmuch (require 'ol-notmuch))))
      #:summary "\
Sensible defaults for org mode"
      #:commentary "\
Indentation and refile configurations, visual adjustment."
      #:keywords '(convenience org-mode)
      #:elisp-packages (list emacs-org emacs-org-contrib))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-agenda
          #:key
          (org-agenda-files 'nil))
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
         (setq org-agenda-custom-commands
               `((,(kbd "C-d") "Agenda for the day"
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
                     (org-deadline-warning-days 0)
                     (org-agenda-block-separator nil)
                     (org-agenda-entry-types '(:deadline))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
                     (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
                   (agenda
                    "*"
                    ((org-agenda-block-separator nil)
                     (org-agenda-span 14)
                     (org-agenda-overriding-header "\nAgenda\n")))
                   (alltodo
                    ""
                    ((org-agenda-block-separator nil)
                     (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled deadline))))
                     (org-agenda-overriding-header "\nBacklog\n")))))))
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

(define* (feature-emacs-git
          #:key
          (emacs-magit emacs-magit)
          (emacs-magit-todos emacs-magit-todos)
          (emacs-git-timemachine emacs-git-timemachine)
          (emacs-git-link emacs-git-link)
          (emacs-git-gutter-fringe emacs-git-gutter-fringe)
          (emacs-git-gutter-transient emacs-git-gutter-transient))
  "Configure git-related utilities for GNU Emacs, including magit,
git-link, git-timemachine."
  (define emacs-f-name 'git)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'git-gutter))

        (defun rde-git-link ()
          "Same as `git-link', but with commit hash specified."
          (interactive)
          (let ((git-link-use-commit t))
            (call-interactively 'git-link)))

        (customize-set-variable 'git-gutter:lighter " GG")

        (require 'configure-rde-keymaps)
        (define-key rde-toggle-map (kbd "g") 'git-gutter-mode)
        (define-key rde-toggle-map (kbd "G") 'global-git-gutter-mode)
        (define-key global-map (kbd "s-g") 'git-gutter-transient)

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
                             (get-value 'emacs-configure-rde-keymaps config)
                             emacs-git-link emacs-git-timemachine
                             emacs-git-gutter-fringe
                             emacs-git-gutter-transient))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-which-key
          #:key
          (min-height 1))
  "Configure which-key. MIN-HEIGHT can be used to adjust the look of which-key
popup, when there are not many items in it, can be easier to look through
available options."
  (ensure-pred integer? min-height)

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


;; TODO: Move font record to apropriate module
(use-modules (rde features fontutils))

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


(define* (feature-emacs-completion
          #:key
          (mini-frame? #f)
          (emacs-orderless emacs-orderless)
          (emacs-cape emacs-cape)
          (emacs-consult emacs-consult)
          (emacs-embark emacs-embark)
          (emacs-marginalia emacs-marginalia-latest))
  "Configure completion system for GNU Emacs."
  (define emacs-f-name 'completion)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (define ripgrep (get-value 'ripgrep config
                               (@ (gnu packages rust-apps) ripgrep)))
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'marginalia)
         (require 'consult))

        (with-eval-after-load
         'minibuffer

         (setq minibuffer-prompt-properties
               '(read-only t cursor-intangible t face minibuffer-prompt))
         (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

         (setq file-name-shadow-properties
               '(invisible t intangible t face file-name-shadow field shadow))
         ;; (advice-add 'completing-read-multiple
         ;;             :override 'consult-completing-read-multiple)

         (setq completion-styles '(orderless))
         (setq completion-category-overrides
               ;; basic is required for /ssh: completion to work, but
               ;; keep the same values for project-file too.
               '((file (styles . (partial-completion basic)))
                 (project-file (styles . (partial-completion basic)))))
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

                  (customize-set-variable
                   'mini-frame-show-parameters
                   (lambda ()
                     `((top . 0.2)
                       (width . 0.8)
                       (left . 0.5)
                       (child-frame-border-width . 1))))
                  (customize-set-variable 'mini-frame-detach-on-hide nil)
                  (customize-set-variable 'mini-frame-color-shift-step 0)
                  (customize-set-variable 'mini-frame-advice-functions
                                          '(read-from-minibuffer
                                            read-key-sequence
                                            save-some-buffers yes-or-no-p))
                  (customize-set-variable 'mini-frame-ignore-commands '()))
                 (mini-frame-mode 1))
             '()))

        (customize-set-variable 'history-length 10000)
        (customize-set-variable
         'savehist-file (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                "/emacs/history"))

        ;; (savehist-mode 1)
        ;; (run-with-idle-timer 30 t 'savehist-save)

        (define-key global-map (kbd "s-.") 'embark-act)
        (define-key global-map (kbd "s->") 'embark-become)


        (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
        (define-key global-map (kbd "M-y") 'consult-yank-pop)
        (define-key global-map (kbd "s-B") 'consult-buffer)
        (define-key global-map (kbd "C-x C-r") 'consult-recent-file)
        (define-key minibuffer-local-map (kbd "s-B") 'embark-become)
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

        ;; MAYBE: Move to feature-emacs-buffers/windows
        (define-key minibuffer-local-map (kbd "s-b") 'exit-minibuffer)
        (define-key global-map (kbd "s-b") 'switch-to-buffer)
        (define-key global-map (kbd "s-w") 'kill-current-buffer)
        (define-key global-map (kbd "s-W") 'kill-buffer-and-window)
        (define-key global-map (kbd "s-o") 'other-window)

        (autoload 'consult-customize "consult" "" nil 'macro)
        (autoload 'consult--customize-set "consult")

        (with-eval-after-load
         'embark
         (require 'embark-consult))

        (with-eval-after-load
         'consult
         (require 'embark-consult)

         (customize-set-variable
          'consult-ripgrep-args
          (concat ,(file-append ripgrep "/bin/rg")
                  " --null --line-buffered --color=never --max-columns=1000 \
--path-separator / --smart-case --no-heading --line-number ."))
         (consult-customize consult-history :category 'consult-history)
         (consult-customize consult-line :inherit-input-method t))

        ;; (with-eval-after-load
        ;;  'marginalia
        ;;  (setq marginalia-annotator-registry
        ;;        (assq-delete-all 'library marginalia-annotator-registry)))
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
             (emacs-consult . ,emacs-consult)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-vertico
          #:key
          (emacs-vertico emacs-vertico-latest)
          (completion-in-region? #t))
  "Configure vertico completion UI for GNU Emacs."
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
         (define-key global-map (kbd "s-s") 'vertico-repeat)
         ;; TODO: Bind vertico-next/previous-group to more usual keys?

         (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
         (customize-set-variable 'vertico-cycle t)

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

                 (advice-add 'vertico-buffer--setup :after
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
                 (consult-history buffer)
                 (consult-lsp-symbols buffer)
                 (embark-keybinding buffer)
                 (consult-location buffer)))

         (setq vertico-multiform-commands
               '((telega-chat-with buffer)
                 ;; For some reason it doesn't have an info-menu
                 ;; category and also setting
                 ;; marginalia-command-categories doesn't help
                 ;; (org-roam-node-find buffer)
                 (Info-goto-node buffer)
                 (info-lookup-symbol buffer)
                 (Info-follow-reference buffer)
                 (consult-yank-pop buffer)))

         (vertico-multiform-mode))

        (vertico-mode 1))
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

(define* (feature-emacs-project)
  "Configure project.el for GNU Emacs."

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
         (require 'consult))

        (define-key global-map (kbd "s-p") project-prefix-map)

        (with-eval-after-load
         'project
         (add-to-list 'project-switch-commands '(project-compile "Compile") t)
         (setq project-switch-use-entire-map t)

         (with-eval-after-load
          'consult
          (setq consult-project-root-function
                (lambda ()
                  (when-let (project (project-current))
                            (car (project-roots project))))))))
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
      `((add-hook 'after-init-hook 'persp-mode)
        ;; TODO: Show current perspective in some global space (tab-bar?).
        (customize-set-variable
         'persp-show-modestring ,(if persp-show-modestring? 't 'nil))
        (customize-set-variable 'persp-modestring-dividers '(" [" "]" "|"))

        ;; MAYBE: Move it to feature-project
        ;; This is mostly a project-related functionality, which uses
        ;; perspective as isolation mechanism.
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
              '()))
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

;; TODO: rewrite to states
(define* (feature-emacs-org-roam
          #:key
          (org-roam-directory #f))
  "Configure org-roam for GNU Emacs."
  (define (not-boolean? x) (not (boolean? x)))
  (ensure-pred not-boolean? org-roam-directory)

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
         (setq org-roam-node-display-template
               (concat "${title:80} " (propertize "${tags:20}" 'face 'org-tag))
               org-roam-node-annotation-function
               (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))
         (org-roam-db-autosync-enable))

        (define-key global-map (kbd "C-c n n") 'org-roam-buffer-toggle)
        (define-key global-map (kbd "C-c n f") 'org-roam-node-find)
        (define-key global-map (kbd "C-c n i") 'org-roam-node-insert))
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
        (require 'configure-rde-keymaps)
        (define-key rde-toggle-map (kbd "k") 'rde-keycast-mode)
        (define-key rde-toggle-map (kbd "K") 'rde-keycast-mode))
      #:summary "\
Show commands and keybindings in header-line"
      #:commentary "\
Make `keycast-mode' work with header line instead of modeline, provides
keybindings and adjust some minor settings."
      #:keywords '(convenience)
      #:elisp-packages (list emacs-moody emacs-keycast
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-keycast)))
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


(define* (feature-emacs-eglot
          #:key
          (emacs-eglot emacs-eglot)
          (emacs-consult-eglot emacs-consult-eglot))
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
         (add-hook 'eglot-managed-mode-hook
                   (lambda () (setq consult-imenu--cache nil))))
        (customize-set-variable 'eglot-extend-to-xref t))
      #:summary "\
Refactoring, completion, navigation, documentation via LSP"
      #:commentary "\
Mostly workarounds and integratios with other packages."
      #:keywords '(convenience completion lsp editing languages)
      #:elisp-packages (list emacs-eglot emacs-consult-eglot))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-eglot)))
   (home-services-getter get-home-services)))


(define* (feature-emacs-smartparens
          #:key
          (emacs-smartparens emacs-smartparens)
          (enable-in-prog-mode? #t)
          (show-smartparens? #f)
          (smartparens-bindings? #t))
  "Configure smartparens for structured code navigation, automatic string escape
and pair management."

  (define emacs-f-name 'smartparens)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if enable-in-prog-mode?
              `((add-hook 'prog-mode-hook 'smartparens-mode))
              '())
        (with-eval-after-load
         'smartparens
         (require 'smartparens-config)
         ,@(if smartparens-bindings? '((sp-use-smartparens-bindings)) '())
         ,@(if show-smartparens?
               '((show-paren-mode 0)
                 (show-smartparens-global-mode 1))
               '())))
      #:elisp-packages (list emacs-smartparens)
      #:summary "\
Structured editing and navigation, automatic string escaping and pair management"
      #:commentary "\
Various tweaks and settings for `smartparents.el'.

By default it calls `sp-use-smartparens-bindings' to set basic keybindings.

Use `sp-cheat-sheet' to get more information about available commands and
their behavior.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-smartparens)))
   (home-services-getter get-home-services)))

;;; emacs.scm end here
