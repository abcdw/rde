;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
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
  #:use-module (gnu home services)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu services)

  #:use-module (rde packages)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (guix gexp)
  #:use-module (rde gexp)
  #:use-module (guix packages)

  #:export (feature-emacs-tempel

            feature-emacs-org
            feature-emacs-org-roam
            feature-emacs-org-agenda))

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
;;; Notes.
;;;

(define* (feature-emacs-org
          #:key
          (emacs-org-modern emacs-org-modern-latest)
          (emacs-org-appear emacs-org-appear)
          (org-directory "~/org")
          (org-capture-templates #f)
          (org-rename-buffer-to-title? #t)
          (org-indent? #t)
          (org-modern? #t))
  "Configure org-mode for GNU Emacs."
  (ensure-pred path? org-directory)
  (ensure-pred maybe-list? org-capture-templates)
  (ensure-pred boolean? org-rename-buffer-to-title?)
  (ensure-pred boolean? org-indent?)
  (ensure-pred boolean? org-modern?)
  (ensure-pred file-like? emacs-org-modern)
  (ensure-pred file-like? emacs-org-appear)

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

        (with-eval-after-load
         'org
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

         (with-eval-after-load 'notmuch (require 'ol-notmuch))

         (add-hook 'org-mode-hook 'org-appear-mode)
         (add-hook 'org-mode-hook 'olivetti-mode)

         (with-eval-after-load
          'org-modern
          (setq org-modern-todo nil)
          (setq org-modern-timestamp nil)
          (setq org-modern-statistics nil)
          (setq org-modern-tag nil)
          (setq org-modern-priority nil))
         ,@(if org-modern? `((global-org-modern-mode)) '())))
      #:summary "\
Sensible defaults for org mode"
      #:commentary "\
Indentation and refile configurations, visual adjustment."
      #:keywords '(convenience org-mode org-modern)
      #:elisp-packages (list emacs-org emacs-org-contrib
                             (get-value 'emacs-olivetti config emacs-olivetti)
                             emacs-org-appear emacs-org-modern))))

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
          (org-roam-directory #f)
          (org-roam-dailies-directory #f)
          (org-roam-capture-templates #f)
          (use-node-types? #t))
  "Configure org-roam for GNU Emacs."
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
               `((setq org-roam-capture-templates org-roam-capture-templates))
               '())

         ,@(if org-roam-dailies-directory
               `((setq org-roam-dailies-directory ,org-roam-dailies-directory))
               '()))

        (define-key global-map (kbd "C-c n t") 'org-roam-dailies-goto-today)
        (define-key global-map (kbd "C-c n d") 'org-roam-dailies-goto-date)
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

;;; emacs-xyz.scm end here
