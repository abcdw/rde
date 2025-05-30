;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
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
  #:use-module (rde predicates)
  #:use-module (gnu home services)
  #:use-module (rde home services emacs)
  #:use-module (rde home services wm)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages xdisorg)

  #:use-module (guix gexp)
  #:use-module (rde gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)

  #:export (feature-emacs
            feature-emacs-portable

            rde-elisp-configuration-service
            emacs-xdg-service
            expand-extra-elisp))


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
           (string-append "rde-" (symbol->string name))
           elisp-expressions
           #:elisp-packages elisp-packages
           #:autoloads? autoloads?
           #:summary summary
           #:commentary commentary
           #:keywords keywords
           #:url (or url "https://trop.in/rde")
           #:authors (or authors '("Andrew Tropin <andrew@trop.in>")))))
      configure-package))

;; TDOO: Deprecate and get rid of this wrapper in favor of explicit service
;; declaration?  Maybe provide a wrapper for home-elisp-configuration instead.
(define* (rde-elisp-configuration-service
          name config
          #:optional (elisp-expressions '())
          #:key
          summary authors url keywords commentary
          (early-init '())
          (elisp-packages '())
          (elisp-packages-rewrites '()))
  "Adds a configure-NAME package to the profile and emacs load path and if
emacs-portable? rde value is present adds autoloads cookies to each expression
of it, otherwise adds a require to @file{init.el}."
  (service
   (make-home-elisp-service-type (symbol-append 'emacs-rde- name))
   (home-elisp-configuration
    (name (symbol-append 'rde- name))
    ;; TODO: Rename the field to elisp-expressions for cosistency?
    (config elisp-expressions)
    (early-init early-init)
    (elisp-packages elisp-packages)
    (elisp-packages-rewrites elisp-packages-rewrites)
    (authors (or authors '("Andrew Tropin <andrew@trop.in>")))
    (url (or url "https://trop.in/rde"))
    (summary summary)
    (commentary commentary)
    (keywords (or keywords '())))))

;; MAYBE: make handler to be actions instead of desktop entries?
(define* (emacs-xdg-service
          name xdg-name gexp-or-file-like
          #:key
          (default-for '())
          (exec-argument "%u"))
  (define file-name (string-append "emacs-" (symbol->string name)))
  (define file-file (file-append (if (file-like? gexp-or-file-like)
                                     gexp-or-file-like
                                     (program-file file-name gexp-or-file-like))
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

;; (define* (make-emacs-feature
;;           #:key
;;           (name)
;;           (home-service-getter (lambda (_) '())))

;;   (define rde-emacs-feature-name (symbol-append 'rde-emacs- name))
;;   (define rde-emacs-feature-service-type-name
;;     (symbol-append 'rde-emacs- name '-service-type))

;;   (define rde-emacs-feature-service-type
;;     (make-home-elisp-service-type rde-emacs-feature-name))

;;   (feature
;;    (name rde-emacs-feature-name)
;;    (values `((,rde-emacs-feature-name . ,rde-emacs-feature-service-type)))
;;    (home-services-getter home-service-getter)))

;; TODO: Add more generic rde-expand function
(define (expand-extra-elisp elisp config)
  "If ELISP is a list just return it, if it's a function call it with CONFIG
argument, throw an exception otherwise."
  (let ((res (if (procedure? elisp) (elisp config) elisp)))
    (ensure-pred list? res)
    res))

;; This function was used, when shepherd get started before graphical
;; environment.
(define (update-emacs-server-env-variables emacs-client)
  "Returns a PROGRAM-FILE, which get the current environment variables and make
emacs servers' environment variables to same values."
  (program-file
   "update-emacs-server-env-variables"
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


;;;
;;; Emacs features.
;;;

(define %default-emacs-package emacs-pgtk)

(define emacs-configure-rde-keymaps
  (rde-emacs-configuration-package
   'keymaps
   `((defvar rde-app-map nil "Prefix keymap for applications.")
     (define-prefix-command 'rde-app-map nil)
     (defvar rde-toggle-map nil "\
Prefix keymap for binding various minor modes for toggling functionalitty.")
     (define-prefix-command 'rde-toggle-map nil))
   #:summary "Keymaps inteded for reuse among configure-* packages"))

(define emacs-configure-rde-startup
  (rde-emacs-configuration-package
   'startup
   `((when (file-exists-p "~/.emacs.d")
       (display-warning 'rde-startup "\
~/.emacs.d exists, emacs may load the wrong init.el")))
   #:autoloads? #t
   #:summary "Additional autoloaded rde startup configuration."))

(define (rde-emacs-base config)
  (let* ((full-name (get-value 'full-name config))
         (email     (get-value 'email config)))
    (rde-elisp-configuration-service
     'emacs-base
     config
     `((defgroup rde nil
         "Base customization group for rde."
         :group 'external
         :prefix 'rde-)
       (require 'rde-keymaps)

       (setq native-comp-jit-compilation nil)

       (setq user-full-name ,full-name)
       (setq user-mail-address ,email)

       ,#~"\n;; Disable messages, when minibuffer is active"
       (setq minibuffer-message-timeout 0)

       ,#~""
       (setq custom-file
             (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                     "/emacs/custom.el"))
       (load custom-file t)

       (setq
        backup-directory-alist
        `(,(cons "." (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                             "/emacs/backup"))))

       (setq
        recentf-save-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                "/emacs/recentf"))

       ;; (add-hook 'after-init 'recentf-mode)
       (recentf-mode 1)
       (run-with-idle-timer 30 t 'recentf-save-list)

       ;; (customize-set-variable 'history-length 10000)
       (setq
        savehist-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                "/emacs/history"))

       (savehist-mode 1)
       (run-with-idle-timer 30 t 'savehist-save)

       (setq
        bookmark-default-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                "/emacs/bookmarks"))

       (setq
        auto-save-list-file-prefix
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                "/emacs/auto-save-list"))

       ,#~""
       (pixel-scroll-precision-mode 1)
       (column-number-mode 1)
       (save-place-mode 1)
       (setq
        save-place-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                "/emacs/places"))
       ;; MAYBE: Make it buffer local?
       (show-paren-mode 1)
       ,#~";; Treat camelCased parts as separate words."
       (subword-mode 1)
       ;; (add-hook 'prog-mode-hook 'subword-mode)

       (setq-default indent-tabs-mode nil)
       (setq save-interprogram-paste-before-kill t)
       (setq mouse-yank-at-point t)
       (setq require-final-newline t)

       (defun rde-whitespace-mode ()
         "Equivalent of `whitespace-mode', but highlights only tabs."
         (interactive)
         (if (and (featurep 'whitespace) whitespace-mode)
             (whitespace-mode 0)
             (progn
              (defvar whitespace-style) ; dynamically bind
              (let ((whitespace-style '(face tabs)))
                (whitespace-mode 1)))))
       (add-hook 'prog-mode-hook
                 (lambda ()
                   (rde-whitespace-mode)
                   (setq show-trailing-whitespace t)))

       (if after-init-time
           (require 'org-protocol)
           (add-hook 'after-init-hook
                     (lambda () (require 'org-protocol))))

       ;; Highlight zero-width whitespaces and other glypless characters.
       (set-face-background 'glyphless-char "red")
       ,#~""
       (autoload 'er/expand-region "expand-region")
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
       (setq isearch-lazy-count t)
       (setq search-whitespace-regexp ".*?")

       ,#~""

       (dolist (mode-hook '(prog-mode-hook))
               (add-hook mode-hook (lambda () (setq truncate-lines t))))

       ,#~""
       ;; XXX: Keep in sync with nyxt *rde-keymap*.
       (define-key global-map (kbd "s-b") 'switch-to-buffer)
       (define-key global-map (kbd "s-w") 'kill-current-buffer)
       (define-key global-map (kbd "s-W") 'kill-buffer-and-window)
       (define-key global-map (kbd "s-o") 'other-window)
       (define-key global-map (kbd "C-z") nil)

       ,#~""
       ,@(if (get-value 'emacs-advanced-user? config #f)
             '((put 'narrow-to-page   'disabled nil)
               (put 'narrow-to-region 'disabled nil))
             '())

       ,#~""

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
         '("rde toggles" . rde-toggle-map))

       ,#~""
       ,@(if (or (get-value 'emacs-disable-warnings? config #f)
                 (get-value 'emacs-advanced-user? config #f))
             `(;; Don't warn for large files
               (setq large-file-warning-threshold nil)
               ;; Don't warn for followed symlinked files
               (setq vc-follow-symlinks t)
               ;; Don't warn when advice is added for functions
               (setq ad-redefinition-action 'accept))
             '())

       ,#~""
       ,@(if (get-value 'emacs-auto-update-buffers? config #f)
             `(;; Revert Dired and other buffers
               (setq global-auto-revert-non-file-buffers t)
               ;; Revert buffers when the underlying file has changed
               (global-auto-revert-mode 1))
             '())

       ,#~""
       ,@(if (get-value 'emacs-auto-clean-space? config #f)
             `((eval-when-compile (require 'ws-butler))
               (autoload 'ws-butler-mode "ws-butler")
               (add-hook 'text-mode-hook 'ws-butler-mode)
               (add-hook 'prog-mode-hook 'ws-butler-mode))
             '())

       ,#~""
       ,@(if (get-value 'desktop-notifications config #f)
             `((with-eval-after-load 'alert
                 (setq alert-default-style 'notifications)))
             '())
       ,#~""
       ;; Specifying default action for display-buffer.
       ;; (setq display-buffer-base-action
       ;;       '(display-buffer-reuse-mode-window
       ;;         display-buffer-reuse-window
       ;;         display-buffer-same-window))
       ;; If a popup does happen, don't resize windows to be equal-sized
       (setq even-window-sizes nil)
       ;; Configure ediff for window manager.
       (setq ediff-diff-options "-w"
             ediff-split-window-function 'split-window-horizontally
             ediff-window-setup-function 'ediff-setup-windows-plain)
       ;; Configure emacs background server.
       ,@(if (get-value 'emacs-server-mode? config #f)
             `((defun rde-kill-emacs (&optional arg restart)
                 "\
Make GNU Shepherd kill the Emacs server.

If RESTART is non-nil, instead of just exiting at the end, herd will restart
an Emacs server. ARG is ignored.

Note: This is added through RDE. Redefining a primitive is not advised in
Emacs, but this one is high-level (present in few other functions), and
tested."
                 (interactive)
                 (call-process
                  ,(file-append
                    (get-value 'shepherd config shepherd) "/bin/herd")
                  nil 0 nil (if restart "restart" "stop")
                  (concat "emacs-" server-name)))

               (add-hook
                'emacs-startup-hook
                (lambda ()
                  (when server-mode
                    (advice-add 'kill-emacs :override 'rde-kill-emacs)))))
             '()))
     #:summary "General settings, better defaults"
     #:commentary "\
It can contain settings not yet moved to separate features."
     #:keywords '(convenience)
     #:elisp-packages
     (append (list (get-value 'emacs-configure-rde-keymaps config)
                   (get-value 'emacs-configure-rde-startup config)
                   emacs-expand-region)
             (if (get-value 'emacs-auto-clean-space? config #f)
                 (list emacs-ws-butler) '())))))

;; https://idiomdrottning.org/bad-emacs-defaults
(define* (feature-emacs
          #:key
          (emacs %default-emacs-package)
          (disable-warnings? #t)
          (auto-update-buffers? #t)
          (auto-clean-space? #t)
          ;; TODO: [Andrew Tropin, 2024-04-04] Enable back, when substitute
          ;; server for RDE is deployed.
          (native-comp? #f)

          ;; TODO: Deprecate (can be added via home services)
          (additional-elisp-packages '())
          (extra-init-el '())
          (extra-early-init-el '())
          ;;=======================

          (emacs-server-mode? #t)
          (default-terminal? #t)
          (default-application-launcher? #t)
          (standalone-minibuffer-height 10))
  "Setup and configure GNU Emacs."
  (ensure-pred file-like? emacs)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)
  (ensure-pred boolean? disable-warnings?)
  (ensure-pred boolean? auto-update-buffers?)
  (ensure-pred boolean? auto-clean-space?)
  (ensure-pred boolean? native-comp?)
  (ensure-pred boolean? emacs-server-mode?)
  (ensure-pred boolean? default-terminal?)
  (ensure-pred boolean? default-application-launcher?)
  (ensure-pred integer? standalone-minibuffer-height)

  (define emacs-client (file-append emacs "/bin/emacsclient"))
  (define emacs-client-create-frame
    (program-file "emacs-client-create-frame"
                  #~(apply system*
                           #$(file-append emacs "/bin/emacsclient")
                           "--create-frame"
                           "--quiet"
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

  ;; Emacs and emacsclient don't handle stdin properly, in the case they
  ;; should be used as dmenu. This tiny guile+emacs script behaves like dmenu.
  (define emacs-dmenu
    (program-file
     "emacs-dmenu"
     #~(begin
         (use-modules (ice-9 textual-ports) (ice-9 popen))
         (let* ((raw-input (get-string-all (current-input-port)))
                (len (string-length raw-input))
                (clean-input
                 (if (and (> len 0)
                          (char=? (string-ref raw-input (- len 1)) #\newline))
                     (string-drop-right raw-input 1)
                     raw-input))
                (count (string-count clean-input #\newline))
                (port (open-input-pipe
                       (format
                        #f "~a --eval '~s'"
                        #$emacs-client
                        `(let* ((vertico-count ,(+ 1 count))
                                (after-make-frame-functions '())
                                (minibuffer-frame
                                 (make-frame
                                  (list
                                   (cons 'display (or x-display-name
                                                      (getenv "WAYLAND_DISPLAY")))
                                   '(name . "dynamic menu - Emacs Client")
                                   '(minibuffer . only)
                                   '(width . 120)
                                   '(height . ,(+ 2 count))))))
                           (unwind-protect
                            (with-selected-frame
                             minibuffer-frame
                             (completing-read "Select: "
                                              (split-string ,clean-input "\n")))
                            (delete-frame minibuffer-frame))))))
                ;; Drop surrounding quotes and newline.
                (selected (string-drop
                           (string-drop-right (get-string-all port) 2)
                           1)))
           (close-port port)
           (format #t "~a\n" selected)))))

  ;; This doesn't use the emacs-client-create-frame program because
  ;; it executes by default the after-make-frame-functions, which
  ;; can mess up with other buffers or frames.
  (define* (emacs-minibuffer-program
            config #:key
            (alternate
             (program-file
              "emacs-client-alternate-fail"
              #~(system*
                 #$(file-append (get-value 'libnotify config libnotify)
                                "/bin/notify-send")
                 "Emacs error"
                 "Minibuffer programs require a running server."
                 "--icon=emacs"))))
    (lambda* (file-name-suffix title command
                               #:key (client emacs-client)
                               (height 10))
      (program-file
       (string-append "emacs-" file-name-suffix)
       #~(system* #$client
                  #$@(if alternate
                         #~((string-append "--alternate-editor=" #$alternate))
                         #~())
                  "--eval"
                  #$(format
                     #f "~s"
                     `(let* ((vertico-count ,height)
                             (after-make-frame-functions '())
                             (minibuffer-frame
                              (make-frame
                               (list
                                (cons 'display (or x-display-name
                                                   (getenv "WAYLAND_DISPLAY")))
                                '(name . ,(string-append title " - Emacs Client"))
                                '(minibuffer . only)
                                '(width . 120)
                                '(height . ,(1+ height))))))
                        (unwind-protect
                         (with-selected-frame minibuffer-frame
                                              (command-execute ',command))
                         (delete-frame minibuffer-frame))))))))

  (define (emacs-application-launcher config)
    ((emacs-minibuffer-program config)
     "application-launcher" "Application Launcher"
     'app-launcher-run-app #:height standalone-minibuffer-height))

  (define (emacs-home-services config)
    "Returns home services related to GNU Emacs."
    (list
     (emacs-xdg-service 'emacs-q "Emacs (No init: -q)"
                        #~(system* "emacs" "-q"))
     (emacs-xdg-service 'emacs-Q "Emacs (No init, no site-lisp: -Q)"
                        #~(system* "emacs" "-Q"))
     (emacs-xdg-service 'emacs-org-protocol "Emacs (Client) [Org-protocol]"
                        emacs-client
                        #:default-for '(x-scheme-handler/org-protocol))

     (service home-emacs-feature-loader-service-type
              (home-emacs-feature-loader-configuration
               (autoloads? #t)
               (add-to-init-el? #f)))

     (rde-emacs-base config)

     (service
      home-emacs-service-type
      (home-emacs-configuration
       (emacs emacs)
       (elisp-packages
        (append
         additional-elisp-packages
         (if default-application-launcher? (list emacs-app-launcher) '())))
       (emacs-servers (if emacs-server-mode? '(server) '()))
       (xdg-flavor? #t)
       (early-init-el
        `(,(slurp-file-like (local-file "./emacs/early-init.el"))))
       (native-comp? native-comp?)))

     (simple-service
      'emacs-add-to-init-el
      home-emacs-service-type
      (home-emacs-extension
       (init-el extra-init-el)
       (early-init-el extra-early-init-el)))

     (simple-service
      'emacs-set-default-editor
      home-environment-variables-service-type
      `(("ALTERNATE_EDITOR" . ,emacs-editor)
        ("VISUAL" . ,emacs-client-create-frame)
        ("MENU" . ,emacs-dmenu)))
     (when (get-value 'sway config #f)
       (simple-service
        'emacs-update-environment-variables-on-sway-start
        home-sway-service-type
        `((,#~"")
          ;; (exec_always "sleep 2s && " ;; Need to wait until emacs daemon loaded.
          ;;              ,(update-emacs-server-env-variables emacs-client))

          (for_window "[title=\".* - Emacs Client\"]" floating enable))))))

  (feature
   (name 'emacs)
   (values (append
            (make-feature-values
             standalone-minibuffer-height
             emacs
             emacs-editor emacs-client emacs-dmenu
             emacs-client-create-frame
             emacs-client-no-wait
             emacs-minibuffer-program
             emacs-configure-rde-keymaps
             emacs-configure-rde-startup
             emacs-server-mode?)
            `((emacs-disable-warnings? . ,disable-warnings?)
              (emacs-auto-update-buffers? . ,auto-update-buffers?)
              (emacs-auto-clean-space? . ,auto-clean-space?))
            (if default-terminal?
                `((default-terminal . ,emacs-client-create-frame))
                '())
            (if default-application-launcher?
                `((default-application-launcher-fn . ,emacs-application-launcher))
                '())))
   (home-services-getter emacs-home-services)))

;; "#f0d3ff" ;; magenta
;; "#c0efff" ;; cyan
;; "#b5d0ff" ;; blue
;; "#aecf90" ;; green
;; "#f2b0a2" ;; red

(define* (feature-emacs-portable
          #:key
          (emacs %default-emacs-package)
          (status-line-bg-color "#b5d0ff")
          (autoloads? #t)
          (additional-elisp-packages '()))
  (ensure-pred file-like? emacs)
  (ensure-pred maybe-string? status-line-bg-color)
  (ensure-pred boolean? autoloads?)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)

  (define (emacs-home-services config)
    "Returns home services related to GNU Emacs, which usually used in development
environment outside of Guix Home."
    (list
     (service home-emacs-feature-loader-service-type
              (home-emacs-feature-loader-configuration
               (loader-feature-name 'feature-loader-portable)
               (autoloads? autoloads?)
               (add-to-init-el? #f)))

     (rde-emacs-base config)

     (service
      home-emacs-service-type
      (home-emacs-configuration
       (emacs emacs)
       (elisp-packages additional-elisp-packages)
       ;;; TODO: Rebuilding packages with emacs will be useful for
       ;;; native-comp, but some packages fails to build, need to fix them.
       (native-comp? #f)))

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
   (values (append (make-feature-values emacs emacs-configure-rde-keymaps
                                        emacs-configure-rde-startup)
                   `((emacs-disable-warnings? . #t)
                     (emacs-auto-update-buffers? . #t)
                     (emacs-auto-clean-space? . #t)
                     (emacs-portable? . #t))))
   (home-services-getter emacs-home-services)))

;; TODO: https://www.reddit.com/r/emacs/comments/xb6qdm/super_fast_emacs_start_up/

;;; emacs.scm end here
