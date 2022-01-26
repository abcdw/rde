(define-module (rde examples abcdw emacs)
  #:export (init-el))

(define init-el
  '(
    ;; NOWEB GENERAL START
    (defmacro qz/advice- (target-fn state advice-fn)
      (let* ((s-advice (lambda (e)
                         (intern (format "qz/advice-%s--%s--%s"
                                         e target-fn advice-fn))))
             (enable (funcall s-advice 'enable))
             (disable (funcall s-advice 'disable)))
        `(progn
           (defun ,enable ()
             (interactive)
             (advice-add ',target-fn ,state ',advice-fn))
    
           (defun ,(funcall s-advice 'disable) ()
             (interactive)
             (advice-remove ',target-fn ',advice-fn))
    
           (,enable)
           (list ',enable ',disable))))
    (defun qz/ensure-list (s)
      (if (listp s)
          s
        (list s)))
    (defvar qz/debug 0 "debugging assists")
    
    (defmacro qz/debug- (&rest body)
      (if qz/debug
          `(progn ,@body)))
    
    (qz/debug- (message "yo"))
    (setq qz/newstore-envs '(sandbox staging production)
          qz/newstore-env-current nil
          qz/newstore-envs-abbrev '((sandbox . x) (staging . s) (production . p))
          qz/newstore-tenant-current nil
          qz/newstore-tenants '("dodici" "windsor"
                                "boardriders" "marine-layer"
                                "frankandoak" "vince"))
    
    (defun qz/newstore-choose-env (&optional env)
      (interactive)
      (message "qz/newstore-env-current: %s"
               (setq qz/newstore-env-current
                     (or env (completing-read "env: " qz/newstore-envs))))
      (qz/restclient-choose-env qz/newstore-env-current)
      (qz/es-choose-url nil nil qz/newstore-env-current))
    
    (defun qz/newstore-choose-tenant (&optional tenant)
      (interactive)
      (message "qz/newstore-tenant-current: %s"
               (setq qz/newstore-tenant-current
                     (or tenant (completing-read "tenant: " qz/newstore-tenants))))
      (qz/restclient-choose-tenant qz/newstore-tenant-current))
    
    (defun qz/newstore-auth-current ()
      (message "qz/newstore-auth-cache: <for qz/newstore-env-current: %s>"
               qz/newstore-env-current)
      (setq qz/newstore-auth-cache
            (qz/newstore-auth qz/newstore-env-current)))
    
    (defun qz/newstore-auth (env)
      "get the auth (password) associated with
    a given `env' from `qz/newstore/envs'
    
    to populate, just fill a `pass' entry like so echo mypass | pass
      insert -e newstore/production"
      (s-trim (shell-command-to-string
               (format "pass newstore/%s" env))))
    
    (defun qz/newstore-quick-auth ()
      (interactive)
      (qz/newstore-choose-tenant)
      (qz/newstore-choose-env)
      (org-sbe "newstore-token"))
    (defun qz/shell-command-to-list-of-strings (command)
      (remove "" (s-split "\n" (shell-command-to-string command))))
    (defun qz/revert-buffer-no-confirm ()
      "Revert buffer without confirmation."
      (interactive)
      (revert-buffer :ignore-auto :noconfirm))
    ;; NOWEB GENERAL END

    ;; NOWEB CONF START
    ;; NOWEB KBD START
    ;;(custom-set-variables
    ;; '(org-disputed-keys '([(shift o)] . [(meta shift o)])))
    
    (defun qz/newline-above ()
      (interactive)
      (save-excursion
        (beginning-of-line)
        (newline))
      (indent-according-to-mode))
    
    (define-key global-map (kbd "C-z") 'qz/newline-above)
    ;;(define-key global-map (kbd "C-o") 'open-line)
    ;;
    ;;(org-remap org-mode-map
    ;;           'open-line 'org-open-line)
    
    (define-key global-map (kbd "H-M-s-h") 'windmove-swap-states-left)
    (define-key global-map (kbd "H-M-s-j") 'windmove-swap-states-down)
    (define-key global-map (kbd "H-M-s-k") 'windmove-swap-states-up)
    (define-key global-map (kbd "H-M-s-l") 'windmove-swap-states-right)
    (define-key global-map (kbd "H-s-h") 'windmove-left)
    (define-key global-map (kbd "H-s-j") 'windmove-down)
    (define-key global-map (kbd "H-s-k") 'windmove-up)
    (define-key global-map (kbd "H-s-l") 'windmove-right)
    (define-key global-map (kbd "H-s-\\") 'org-store-link)
    ;; NOWEB KBD END
    ;; NOWEB CONSULT START
    (with-eval-after-load 'consult
      (defun qz/consult-ripgrep-files (files)
        (let* ((consult-ripgrep-args (concat consult-ripgrep-args " -L"))
               (rg-dir "/tmp/null"))
          (f-delete rg-dir t)
          (mkdir rg-dir t)
          (mapcar (lambda (f)
                    (f-symlink (expand-file-name f)
                               (format "%s/%s-%s"
                                       rg-dir (gensym) (s-replace "/" "-" f))))
                  files)
          (consult-ripgrep rg-dir)))
      (defun qz/consult-ripgrep-bookmark ()
        (interactive)
        (let ((files (mapcar (lambda (b) (cdr (assoc 'filename b)))
                             bookmark-alist)))
          (qz/consult-ripgrep-files files)))
      
      (define-key global-map (kbd "C-c b s") 'qz/consult-ripgrep-bookmark)
      )
    ;; NOWEB CONSULT END
    ;; NOWEB CUSTOM START
    (custom-set-variables
     '(org-imenu-depth 99))
    ;; NOWEB CUSTOM END
    ;; NOWEB ES START
    (with-eval-after-load 'restclient
      (defun qz/es-choose-url (&optional url backend env)
        (interactive)
        (and qz/debug (message "DEBUG qz/es-choose-url: %s"
                               (list url backend env)))
        (let* ((backend (qz/es-choose-backend backend))
               (url (or url
                        (and backend env
                             (qz/es-choose-env env)
                             (format qz/newstore-es-string backend env)))))
          (message "es-default-url: %s"
                   (setq es-default-url
                         (or url (completing-read
                                  "es-url: " qz/newstore-es-urls)))))
        es-default-url)
      
      (defun qz/es-choose-backend (&optional backend)
        (interactive)
        (and qz/debug (message "DEBUG qz/es-choose-backend: %s" backend))
        (message "qz/newstore-es-backend-current: %s"
                 (setq qz/newstore-es-backend-current
                       (or backend (completing-read "es-backend: " qz/newstore-es-backends))))
        qz/newstore-es-backend-current)
      
      (defun qz/es-choose-env (&optional env)
        (interactive)
        (and qz/debug (message "DEBUG qz/es-choose-env: %s" env))
        (message "qz/newstore-es-env-current: %s"
                 (setq qz/newstore-es-env-current
                       (or env (completing-read "es-env: " qz/newstore-envs))))
        qz/newstore-es-env-current)
      
      (defun qz/test-es-ui (&optional url backend env)
        (setq qz/newstore-es-env-current nil
              qz/newstore-es-backend-current nil)
        (funcall-interactively 'qz/es-choose-url url backend env)
        (list
         qz/newstore-es-env-current
         qz/newstore-es-backend-current
         es-default-url))
      
      ;;(qz/test-es-ui)              ;; prompt, noset
      ;;(qz/test-es-ui nil)          ;; prompt, noset
      ;;(qz/test-es-ui nil nil)      ;; prompt, noset
      ;;(qz/test-es-ui nil nil nil)  ;; prompt, noset
      ;;(qz/test-es-ui nil 'kibana 'production)    ;; noprompt, set
      
      (defun qz/es-choose-cookie-headers ()
        "TODO"
        (interactive)
        (message
         "es-default-headers: %s"
         (setq es-default-headers `(("Content-Type" . "application/json; charset=UTF-8")
                                    ("Cookie" . ,(format "ACCEZZIOCOOKIE=%s"
                                                         (read-from-minibuffer "es cookie: ")))))))
      (setq es-default-url "https://elasticsearch-production.newstore.luminatesec.com"
            es-default-headers nil
            es-always-pretty-print t
            es-default-headers
            `(("Content-Type" . "application/json; charset=UTF-8")
              ("Cookie" . ,(format "ACCEZZIOCOOKIE=%s"
                                   "11fdbe68-b0f3-4dd0-9894-f97afe3662dc"))))
      
      (setq qz/newstore-es-string "https://%s-%s.newstore.luminatesec.com"
            qz/newstore-es-backends '(kibana elasticsearch)
            qz/newstore-es-backend-current nil
            qz/newstore-es-env-current nil
            qz/newstore-es-urls (cl-loop for env in qz/newstore-envs
                                         append (cl-loop for es-backend in qz/newstore-es-backends
                                                         collect (format qz/newstore-es-string es-backend env))))
      (defvar qz/restclient-tenant nil)
      
      (defun qz/restclient-choose-tenant (&optional tenant)
        (interactive)
        (message "qz/restclient-tenant: %s"
                 (setq qz/restclient-tenant
                       (or tenant (completing-read
                                   "restclient-tenant: " qz/newstore-tenants))))
        qz/restclient-tenant)
      )
    ;; NOWEB ES END
    ;; NOWEB EMBARK START
    (define-key global-map (kbd "C-.") 'embark-act)
    (with-eval-after-load 'embark
      
      )
    ;; NOWEB EMBARK END
    (defun qz/yq-interactively ()
      "haha yaml loophole"
      (interactive)
      (let ((jq-interactive-command "yq"))
        (call-interactively 'jq-interactively)))
    (require 'hyperbole)
    (message "pre org: %s" (shell-command-to-string "date"))
    (with-eval-after-load 'org
      (message "mid org: %s" (
                              shell-command-to-string "date"))
      ;; NOWEB ORG START
      (define-key org-mode-map (kbd "C-c C-j") 'consult-org-heading)
      (defvar qz/org-babel-indent-exclude-lang nil "org-babel languages to exclude from auto indent/format with ")
      (setq qz/org-babel-indent-exclude-lang nil)
      
      (setq qz/debug t)
      
      (defun qz/org-babel-indent-block (beg end &rest args)
        (interactive "r")
        (and qz/debug (message "qz/org-babel-indent-block: BEG %s END %s ARGS %s" beg end args))
        (save-mark-and-excursion
          (when (and (funcall-interactively 'org-babel-mark-block)
                     (not (seq-contains-p
                           qz/org-babel-indent-exclude-lang
                           (car (car (cdr (car (org-babel-tangle-single-block 1 t))))))))
            (call-interactively 'indent-region))))
      
      (define-key org-mode-map (kbd "C-c C-v C-\\") 'qz/org-babel-indent-block)
      
      ;; NOTE: blocks default
      ;;(add-to-list 'org-ctrl-c-ctrl-c-hook 'qz/org-babel-indent-block)
      ;;(setq org-ctrl-c-ctrl-c-hook nil)
      ;;
      ;; NOTE: not the right eval/exec fn for `{C-c C-c}'
      ;;(advice-add 'org-babel-eval :before 'qz/org-babel-indent-block)
      ;;(advice-remove 'org-babel-eval 'qz/org-babel-indent-block)
      ;;
      ;; conclusion: use `advice' so as not to block standard org-mode
      ;; `{C-c C-c}' behaviour like with `org-ctrl-c-ctrl-c-hook'
      
      (qz/advice- org-babel-execute-src-block :before qz/org-babel-indent-block)
      (defun qz/org-refresh-inline-images (&rest args)
        (org-toggle-inline-images t)
        (org-toggle-inline-images t))
      
      (qz/advice- org-babel-execute-src-block :after qz/org-refresh-inline-images)
      
      ;; NOWEB AGENDA START
      (defun qz/agenda-files-update (&rest _)
        "Update the value of `org-agenda-files' with relevant candidates"
        (interactive)
        (setq org-agenda-files (qz/files-agenda)
              qz/agenda-daily-files (qz/agenda-daily-files-f)))
      (defun qz/agenda-files-update-clock (&rest _)
        "An optimisation for org-clock, which is SO SLOW.
       Returns a LIST of files that contain CLOCK, which reduces
      processing a lot"
        (interactive)
        (setq org-agenda-files (qz/files-clock)))
      (list 
       ;; optimisation setup: setup subset of clock files 
       (qz/advice- org-clock-resolve :before qz/agenda-files-update-clock)
       ;; optimisation teardown: restore full set of agenda-files
       (qz/advice- org-clock-resolve :after qz/agenda-files-update))
      (setq qz/daily-title-regexp ".?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.?")
      
      (defun qz/agenda-daily-files-f ()
        (seq-filter (lambda (s) (string-match qz/daily-title-regexp s))
                    org-agenda-files))
      (defun qz/clock-files ()
        (split-string
         (shell-command-to-string "rg CLOCK ~/life/roam/ -c | grep -v 'org#' | awk -F '[,:]' '{print $1}'")))
      (defun qz/files-agenda ()
        (seq-uniq (append qz/org-agenda-files (qz/project-files))))
      (defun qz/project-files ()
        "Return a list of note files containing Project tag."
        (seq-map
         'car
         (org-roam-db-query
          '(:select :distinct file
                    :from tags
                    :inner :join nodes
                    :on (= tags:node_id nodes:id)
                    :where (= tags:tag "project")))))
      (defun qz/org-roam-private-files ()
        "Return a list of note files containing tag =private="
        (seq-map
         #'car
         (org-roam-db-query
          [:select :distinct file
                   :from tags
                   :inner :join nodes
                   :on (= tags:node_id nodes:id)
                   :where (= tags:tag "private")])))
      (setq qz/org-agenda-prefix-length 20
            org-agenda-prefix-format nil)
      ;; '((agenda . " %i Emacs Configuration %?-12t% s")
      ;;   (todo . " %i Emacs Configuration  ")
      ;;   (tags . " %i Emacs Configuration  ")
      ;;   (search . " %i Emacs Configuration  "))
      
      (defun vulpea-agenda-category (&optional len)
        "Get category of item at point for agenda.
      
      Category is defined by one of the following items:
      - CATEGORY property
      - TITLE keyword
      - TITLE property
      - filename without directory and extension
      
      When LEN is a number, resulting string is padded right with
      spaces and then truncated with ... on the right if result is
      longer than LEN.
      
      Usage example:
      
        (setq org-agenda-prefix-format
              '((agenda . \" Emacs Configuration %?-12t %12s\")))
      
      Refer to `org-agenda-prefix-format' for more information."
        (let* ((file-name (when buffer-file-name
                            (file-name-sans-extension
                             (file-name-nondirectory buffer-file-name))))
               (title (qz/node-title))
               (category (org-get-category))
               (result
                (or (if (and
                         title
                         (string-equal category file-name))
                        title
                      category)
                    "")))
          (if (numberp len)
              (s-truncate len (s-pad-right len " " result))
            result)))
      ;; NOWEB AGENDA END
      
      (require 'ob-async)
      (setq org-confirm-babel-evaluate nil)
      (setq org-structure-template-alist
            '(;; yp
              ("d"  . "definition")
              ("ee" . "example")
              ("es" . "src es")
              ("el" . "src emacs-lisp")
              ("q"  . "quote")
              ("sb" . "src shell")
              ("se" . "src emacs-lisp")
              ("sl" . "src scheme")
              ("sp" . "src sql :engine postgres")
              ("sr" . "src R")
              ("ss" . "src")
              ("jp" . "src jupyter-python")
              ("jr" . "src jupyter-R")
              ("r"  . "src restclient")))
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (lisp . t)
         ;;(jupyter . t)
         (python . t)
         ;;(ipython . t)
         (scheme . t)
         (sql . t)
         ;;(plant-uml . t)
         (shell . t)
         (sqlite . t)
         (elasticsearch . t)
         (restclient . t)
         (R . t)))
      (defun qz/org-babel-choose-block (&optional lob)
        "choose block, insert scaffold for args.
      
      might honestly be better to generate `yas' template when we load
      blocks with `qz/org-babel-do-lob-ingest-files', but I've never used
      yas so idk
      
      use a prefix arg to shortcut (org-table-get-constant \"bonk\"
      "
        (interactive)
        (message "prefix: %s" (list current-prefix-arg prefix-arg lob))
        (let ((lob (or lob
                       (intern (completing-read
                                "lob: " (mapcar 'car org-babel-library-of-babel))))))
          (with-current-buffer (current-buffer)
            (end-of-line)
            (newline)
            (insert (format "#+name: call-%s\n#+call: %s(%s)"
                            lob lob (or (and current-prefix-arg
                                             "(org-table-get-constant \"bonk\")")
                                        "")))
      
            (when-let
                ((args (remove
                        nil (cl-loop for a in (assoc lob org-babel-library-of-babel)
                                     append
                                     (when (listp a)
                                       (cl-loop for b in a
                                                collect
                                                (when (eq :var (car b)) (cdr b))))))))
              (message "%s" args)
              (insert (format "(%s)" (s-join ", " args)))))))
      
      ;;(qz/org-babel-choose-block 'newstore-get-order-by-type)
      (define-key org-babel-map (kbd "M-l") 'qz/org-babel-choose-block)
      (defun qz/org-babel-make-table-constants ()
        "exec from the top of a tree"
        (interactive)
        (let* ((hi-lock-auto-select-face t)
               ;; above is 100x better when you patch `hi-lock-face-symbol-at-point'
               ;; with `(or (and hi-lock-auto-select-face (hi-lock-read-face-name)) 'hi-yellow)'
               (col '()))
          (save-excursion
            (org-map-tree
             (lambda ()
               (when-let* ((s (org-get-heading))
                           (s (org-no-properties s))
                           (i (string-match ":" s))
                           (k (substring s 0 i))
                           (v (substring s (+ 2 i))))
                 (message "key: %s" k)
                 (message "value: %s" v)
                 (message "col: %s" col)
                 (setq col (cons (format "%s=%s" k v) col))
                 (funcall-interactively 'highlight-phrase v))))
            (org-back-to-heading)
            (next-line)
            (newline)
            (previous-line)
            (insert (format "#+constants: %s" (s-join " " (reverse col)))))
          col))
      
      (define-key org-babel-map (kbd "M-d") 'qz/org-babel-make-table-constants)
      (defun qz/org-babel--list->rows (name lst)
        (cons (list name)
              (cons 'hline (mapcar 'list lst))))
      (defun qz/org-inbox-capture ()
        (interactive)
        "Capture a task in agenda mode."
        (org-capture nil "i"))
      (with-eval-after-load 'org-roam
        ;; NOWEB ROAM START
        (message "roam start")
        (setq qz/org-roam-dailies-filespec "private-%<%Y-%m-%d>.org")
      
        (defun qz/inspect-agenda-files ()
          `((org-files-list . ,(length (org-files-list)))
            ((org-agenda-files . ,(length (org-agenda-files)))
             ((qz/project-files . ,(length (qz/project-files)))
              (qz/agenda-daily-files-f . ,(length (qz/agenda-daily-files-f)))))))
        (defun qz/inspect-agenda-updates ()
          (mapcar (lambda (s) `(,s . (,(progn (funcall s)
                                              (qz/inspect-agenda-files)))))
                  '(qz/agenda-files-update qz/agenda-files-update-clock)))
        (defun qz/org-agenda-gtd ()
          (interactive)
          (org-agenda nil "g")
          (goto-char (point-min))
          (org-agenda-goto-today))
        
        (setq org-agenda-custom-commands nil)
        
        (add-to-list
         'org-agenda-custom-commands
         `("g" "GTD"
           ((agenda "" ((org-agenda-span 'day) (org-deadline-warning-days 60)))
            (tags-todo "now"
                       ((org-agenda-overriding-header "now")))
            (tags-todo "wip"
                       ((org-agenda-overriding-header "wip")))
            (todo "TODO"
                  ((org-agenda-overriding-header "to process")
                   (org-agenda-files '(,(format "%s/%s" org-roam-directory "inbox.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "daily inbox")
                   (org-agenda-files qz/agenda-daily-files)))
            (todo "TODO"
                  ((org-agenda-overriding-header "emails")
                   (org-agenda-files '(,(format "%s/%s" org-roam-directory "emails.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "one-off Tasks")
                   (org-agenda-files '(,(format "%s/%s" org-roam-directory "next.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "to yak shave")
                   (org-agenda-files '(,(format "%s/%s" org-roam-directory "emacs.org"))))))))
        
        (add-to-list
         'org-agenda-custom-commands
         `("c" "create"
           ((agenda "" ((org-agenda-span 'day) (org-deadline-warning-days 60)))
            (tags-todo "diy+create+do+buy+make+wip"
                       ((org-agenda-overriding-header "wip")))
            (tags-todo "diy+create+do"
                       ((org-agenda-overriding-header "create")))
            (tags-todo "buy"
                       ((org-agenda-overriding-header "buy")))
            (tags-todo "make"
                       ((org-agenda-overriding-header "make"))))))
        
        (add-to-list
         'org-agenda-custom-commands
         `("w" "work"
           ((tags "@work+wip"
                  ((org-agenda-overriding-header "wip")))
            (tags-todo "@work"
                       ((org-agenda-overriding-header "work"))))))
        
        ;;(pp org-agenda-custom-commands)
        (setq qz/org-agenda-files
              (mapcar (lambda (f) (expand-file-name (format "%s/%s" org-roam-directory f)))
                      '("calendar-home.org" "calendar-work.org" "schedule.org")))
        (defvar qz/org-babel-lob-ingest-files
          (append (mapcar (lambda (s)
                            (when-let ((n (org-roam-node-from-title-or-alias s)))
                              (org-roam-node-file n)))
                          '("NewStore" "kubernetes"))
                  (list nil))
          "files from which named `src' blocks should be loaded")
        
        (defun qz/org-babel-do-lob-ingest-files (&optional files)
          (interactive)
          (mapcar (lambda (f) (org-babel-lob-ingest f))
                  (append qz/org-babel-lob-ingest-files files)))
        
        (qz/org-babel-do-lob-ingest-files)
        ;; [[file:~/.doom.d/config.org::*templates][templates]]
        (setq org-capture-templates
              `(("i" "inbox" entry
                 (file ,(concat org-agenda-directory "/inbox.org"))
                 "* TODO %? \nCREATED: %u\nFROM: %a")
                ;; spanish language capturing
                ("v" "vocab; spanish" entry
                 (file+headline ,(concat org-roam-directory "/spanish_language.org") "vocab, phrases")
                 ,(s-join "\n" '("** \"%?\" :es:"
                                 "FROM: %a" ""
                                 "*** :en:" "")))
                ;; capture link to live `org-roam' thing
                ("n" "now, as in NOW" entry (file ,(concat org-agenda-directory "/wip.org"))
                 ,(s-join "\n" '("* TODO [#A1] %? "
                                 "DEADLINE: %T"
                                 "CREATED: %u")))
                ;; fire directly into inbox
                ("c" "org-protocol-capture" entry (file ,(concat org-agenda-directory "/inbox.org"))
                 ,(s-join "\n" '("* TODO [[%:link][%:description]]"
                                 "CREATED: %u" ""
                                 "#+begin_quote" ""
                                 "%i"
                                 "#+end_quote"))
                 :immediate-finish t)
                ;; push last captured item into inbox
                ("l" "last-capture" entry (file ,(concat org-agenda-directory "/inbox.org"))
                 (function qz/inbox-last-captured)
                 :immediate-finish t)
                ("I" "current-roam" entry (file ,(concat org-agenda-directory "inbox.org"))
                 (function qz/current-roam-link)
                 :immediate-finish t)
                ("w" "weekly review" entry
                 (file+datetree ,(concat org-agenda-directory "reviews.org"))
                 (file ,(concat org-agenda-directory "templates/weekly_review.org")))))
        
        
        
        
        ;; [[file:~/.doom.d/config.org::*capture templates][roam capture templates]]
        
        (defun qz/org-roam-capture-current ()
          (interactive)
          "Capture a task in agenda mode."
          (org-capture nil "I"))
        
        (defun qz/roam-capture-todo ()
          (interactive)
          "Capture a task in agenda mode."
          (cl-destructuring-bind (thing region)
              (qz/thing-at-point-or-region-and-region)
            (org-roam-capture- :goto t
                               :keys "n"
                               :node (org-roam-node-create :title thing)
                               :props `(:immediate-finish t :jump-to-captured nil
                                                          :region ,region     :insert-at ,(point-marker)
                                                          :finalize 'insert-link))
            (qz/capture-last-captured)))
        (setq qz/org-roam-capture-head "#+title: ${title}\n")
        (setq qz/capture-title-timestamp-roam "20210813T161035Z-${slug}.org")
        (setq org-roam-capture-templates
              `(("d" "default" plain "%?"
                 :if-new (file+head ,qz/capture-title-timestamp-roam
                                    ,qz/org-roam-capture-head)
                 :unnarrowed t)
                ("n" "empty" plain "%?"
                 :if-new (file+head ,qz/capture-title-timestamp-roam
                                    ,qz/org-roam-capture-head)
                 :immediate-finish t)
                ))
        (setq org-roam-dailies-capture-templates
              `(("d" "default" entry
                 ,(s-join "\n" '("* [%<%H:%M>] %?"
                                 "CREATED: <%<%Y-%m-%d %H:%M>>"
                                 "FROM: %a"))
                 :if-new (file+head+olp
                          ,qz/org-roam-dailies-filespec
                          ,(s-join "\n" '("#+title: <%<%Y-%m-%d>>"
                                          "#+filetags: daily private project" ""
                                          "%(qz/today-dateref)" ""
        
                                          "* today, I will"))
                          ("journal")))))
        
        (setq qz/org-roam-dailies-capture-templates--tangent
              '("d" "default" entry
                ,(s-join "\n" '("* TANGENT [%<%H:%M>] %?"
                                "CREATED: <%<%Y-%m-%d %H:%M>>"
                                "FROM: %a"))
                :if-new (file+head+olp
                         ,qz/org-roam-dailies-filespec
                         ,(s-join "\n" '("#+title: <%<%Y-%m-%d>>"
                                         "#+filetags: daily private project" ""
                                         "%(qz/today-dateref)" ""
                                         "* today, I will"
                                         "* journal"
                                         "* tangent"))
                         ("tangent"))))
        ;;; day lookup
        (defvar qz/day-lookup
          '((Mon . "[[id:d5ad0bac-e82b-43d0-960f-26eeb1daf91b][Monday]]")
            (Tue . "[[id:cb662cc6-bde2-4f9c-b3fa-62346c6df27a][Tuesday]]")
            (Wed . "[[id:411a8e5a-8d89-4886-b2ea-047a3970710a][Wednesday]]")
            (Thu . "[[id:659b9931-ae09-422b-8e91-1bf4cc58e94c][Thursday]]")
            (Fri . "[[id:b3255cd1-db37-4e07-99cf-5e60d52a2579][Friday]]")
            (Sat . "[[id:b63897c3-30cc-42eb-83b5-c8e372e5af9a][Saturday]]")
            (Sun . "[[id:2e28574b-4793-4c05-b83d-e36e9a77515b][Sunday]]"))
          "an index; get days from abbrev (assoc 'Mon qz/day-lookup)")
        (defvar qz/month-lookup
          '("[[id:b92355d7-110e-467c-b7a7-d02b2043af3f][January]]"
            "[[id:7e0af966-8d3e-4e88-b53f-d074902e175a][February]]"
            "[[id:f41751f8-a2a9-4b38-ba03-2ceec2fae4cc][March]]"
            "[[id:ae0ae458-2216-4178-8073-4a26f23747d9][April]]"
            "[[id:6a680100-e842-4257-819f-8cf6cbedddbc][May]]"
            "[[id:f811621c-1b37-43f7-9d01-52bdf9f27637][June]]"
            "[[id:a4d5c8fe-3910-4483-b59e-ce50cd6699a7][July]]"
            "[[id:94e9b0a7-6cd0-4104-821e-613876fe76e3][August]]"
            "[[id:f9ad8160-cae5-4195-a85f-0160710ce8dd][September]]"
            "[[id:da9f0d53-e3f7-4f72-bc1a-d060bc2d1745][October]]"
            "[[id:a4e3a97a-dac9-4bc6-a5e9-5949f707a6de][November]]"
            "[[id:f874ca1a-0d3f-4840-8340-511ed0ac286f][December]]")
          "an index; get days from abbrev (nth 0 qz/month-lookup)")
        (defun qz/today-dateref (&optional time)
          (cl-destructuring-bind (day nday month year)
              (split-string
               (format-time-string "%a:%d:%m:%Y" (or nil (current-time))) ":")
            (format "%s %s %s, %s"
                    (cdr (assoc (intern day) qz/day-lookup))
                    nday
                    (nth (- (string-to-number month) 1) qz/month-lookup)
                    (or (if-let ((node (org-roam-node-from-title-or-alias year)))
                            (org-link-make-string
                             (concat "id:" (org-roam-node-id node))
                             (org-roam-node-title node)))
                        year))))
        (defun qz/org-daily-tangent-capture ()
          (interactive)
          "Capture the inevitable tangent"
          (org-capture nil "t"))
        (defun qz/today-as-daily-file ()
          (format-time-string "private-%Y-%m-%d.org"))
        ;; [[file:~/.doom.d/config.org::*capture convenience functions][capture convenience functions]]
        (defun qz/current-roam-link ()
          "Get link to org-roam file with title"
          (interactive)
          (concat "* TODO "
                  (let ((n (qz/org-roam-node-at-point)))
                    (org-link-make-string
                     (concat "id:" (org-roam-node-id n))
                     (org-roam-node-title n)))))
        (defun qz/node-tags (&optional node)
          (or (and node (org-roam-node-tags node))
              (save-excursion
                (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
                (if (= (org-outline-level) 0)
                    (split-string-and-unquote (or (cadr (car (org-collect-keywords '("filetags")))) ""))
                  (org-get-tags)))))
        
        (defun qz/node-title (&optional node limit)
          (or (and node (org-roam-node-title node))
              (save-excursion
                (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
                (if (= (org-outline-level) 0)
                    (cadr (car (org-collect-keywords '("title"))))
                  (substring-no-properties (org-get-heading t t t))))))
        (defun qz/title->roam-id (title)
          (org-roam-node-id (org-roam-node-from-title-or-alias title)))
        (defun qz/ensure-tag (tagstring tag)
          "Apply `org-roam-tag-add' for `tag' to node with existing tags
        `tagstring'
        
        HACK: using `re-search-backward' to jump back to applicable
        point (implicitly, `point-min' for file-level; :PROPERTIES: drawer for
        entry); covering 'inherited match'.
        
        this could be updated to jump back, but only 'landing' final on
        PROPERTIES with non-nil :ID:"
          (let ((ltag (-flatten (or (and (listp tag) tag) (list tag)))))
            (progn (message "ensuring tag for %s" ltag)
                   (org-roam-tag-add ltag))))
        (defun qz/org-roam--insert-timestamp (&rest args)
          (when (not (org-entry-get nil "CREATED"))
            (org-entry-put nil "CREATED" (format-time-string "<%Y-%m-%d %a %H:%M>")))
          (qz/org-roam--updated-timestamp))
        
        (defun qz/org-roam--updated-timestamp (&rest args)
          (when-let* ((_ (org-roam-file-p))
                      (n (org-roam-node-at-point)))
            (org-entry-put
             (org-roam-node-point n) "UPDATED"
             (format-time-string "<%Y-%m-%d %a %H:%M>"))))
        
        (add-hook 'org-roam-capture-new-node-hook 'qz/org-roam--insert-timestamp)
        (add-hook 'before-save-hook 'qz/org-roam--updated-timestamp)
        (qz/advice- org-id-get-create :after qz/org-roam--insert-timestamp)
        (defun qz/hard-refresh-org-tags-in-buffer ()
          (interactive)
          (setq org-file-tags nil)      ; blast the cache
          (org-set-regexps-and-options) ; regen property detection regexp
          (org-get-tags))               ; write to cache
        (defun qz/title-to-tag (title)
          "Convert TITLE to tag."
          (if (equal "@" (subseq title 0 1))
              title
            (concat "@" (s-replace " " "" title))))
        (defun qz/org-roam-node-from-tag (tag)
          (seq-map
           #'car
           (org-roam-db-query
            [:select :distinct file
                     :from tags
                     :inner :join nodes
                     :on (= tags:node_id nodes:id)
                     :where (= tags:tag tag)])))
        (defun qz/note-buffer-p (&optional node &rest _)
          "Return non-nil if the currently visited buffer is a note."
          (interactive)
          (or (org-roam-node-p node)
              (and buffer-file-name (org-roam-file-p buffer-file-name))))
        (defun qz/is-private-p (&optional node &rest _)
          (interactive)
          (let ((title (qz/node-title node)))
            (if (not title)
                (and (message "unable to evaluate privateness; no title") nil) ; return false (not private)
              (or (string-match-p qz/daily-title-regexp title) ; daily
                  (string-match-p "meeting" title)             ; concerns a meeting
                  (qz/has-link-to-p
                   (list (qz/title->roam-id "thinkproject")
                         (qz/title->roam-id "NewStore")))))))   ; concerns work
        (defun qz/has-links (node)
          "connections exist, for id of `node'"
          (org-roam-db-query
           [:select [source dest]
                    :from links
                    :where (or  (= dest $s1)
                                (= source $s1))]
           node))
        
        (defun qz/node-has-links (node)
          "connections exist, for `node'"
          (qz/has-links (org-roam-node-id node)))
        (defun qz/has-link-to-p (dst &optional src)
          "directed connection exists, from `src' to `dst'"
          (if-let* ((nap (or src (org-roam-node-at-point)))
                    (src (or src (org-roam-node-id nap))))
              (org-roam-db-query
               [:select dest
                        :from links
                        :where (and (= source $s1)
                                    (IN dest $v2))]
               src (apply 'vector (qz/ensure-list dst)))))
        
        (defun qz/node-has-link-to-p (dst &optional src)
          (qz/has-link-to-p (org-roam-node-id dst)
                            (and dst (org-roam-node-id dst))))
        ;;; ref capture
        (setq org-roam-capture-ref-templates
              `(("r" "ref" plain
                 "%?"
                 :if-new (file+head ,qz/capture-title-timestamp-roam
                                    "#+title: ${title}\n")
                 :unnarrowed t)))
        (defun qz/roam-buffer-image-width ()
          (setq-local org-image-actual-width 150)
          (org-redisplay-inline-images))
        
        (add-hook 'org-roam-mode-hook 'qz/roam-buffer-image-width)
        (add-to-list 'magit-section-initial-visibility-alist (cons 'org-roam-node-section 'hide))
        ;; NOWEB ROAM END
        )
      (setq org-confirm-babel-evaluate nil)
      ;; [[file:~/.doom.d/config.org::*refile][refile]]
      (setq org-refile-targets '(("reading.org" :level . 0)
                                 ("emacs.org" :level . 0)
                                 ("watching.org" :level . 0)
                                 ("learning.org" :level . 0)
                                 ("inbox.org" :level . 0)
                                 ("sample.org" :level . 0)
                                 ("wip.org" :level . 0)))
      (setq org-log-done 'time)
      (require 'org-download)
      (defun qz/org-choose-current-attachment ()
        (let ((attach-dir (org-attach-dir)))
          (if attach-dir
              (let* ((file (pcase (org-attach-file-list attach-dir)
                             (`(,file) file)
                             (files (completing-read "Open attachment: "
                                                     (mapcar #'list files) nil t))))
                     (path (expand-file-name file attach-dir)))
                path))))
      
      (defun qz/org-insert-current-attachment ()
        (interactive)
        (insert
         (format "[[file:./%s]]"
                 (dired-make-relative
                  (qz/org-choose-current-attachment)))))
      
      (define-key org-mode-map (kbd "C-c M-a") 'qz/org-insert-current-attachment)
      
      ;; NOWEB ORG END
      (message "post org: %s" (shell-command-to-string "date"))
      )
    (setq org-image-actual-width 640)
    (with-eval-after-load 'pdf-view
      (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))
    (defvar qz/restclient-env nil)
    
    (defun qz/restclient-choose-env (&optional env)
      (interactive)
      (message "qz/restclient-env: %s"
               (setq qz/restclient-env
                     (cdr (assoc (intern (or env
                                             (completing-read "restclient-env: " qz/newstore-envs)))
                                 qz/newstore-envs-abbrev))))
      qz/restclient-env)
    (defvar qz/restclient-token nil)
    (defvar qz/restclient-token-field 'access_token)
    
    (defun qz/restclient-hook ()
      "Update token from a request."
      ;; url is visible while the hook is running.
      (let ((result))
        (save-excursion
          (cond
           ((string-suffix-p "/token" url)
            (condition-case nil
                (progn
                  (setq result (cdr (assoc qz/restclient-token-field (json-read))))
                  (when (stringp result)
                    (progn
                      (setq qz/restclient-token result)
                      (message (concat "stored token: " qz/restclient-token)))))
              (error (message "That wasn't cleanly handled."))))))))
    
    (add-hook 'restclient-response-loaded-hook 'qz/restclient-hook)
    (provide 'restclient-hooks)
    (defvar qz/aws-env nil
      "the aws login configuration, managed through saml2aws
    
    to manipulate, run
    $ saml2aws login -a PROFILE_ALIAS
    
    files of note
    `$HOME/.aws/'
    `$HOME/.saml2aws'")
    (defun qz/choose-aws-env (&optional env)
      (interactive)
      (setq qz/aws-env
            (or env (completing-read
                     "aws-env: "
                     (->> (shell-command-to-string
                           "cat ~/.saml2aws | grep '^name' | cut -d'=' -f2")
                          (s-split "\n")
                          (remove "")))))
      (async-shell-command (format "saml2aws login -a %s"
                                   qz/aws-env)
                           "*aws*"))
    (defvar qz/kubectl-context nil
      "the operating kubernetes context.
    
    to check, at a shell, run: 
    `$ kubectl config get-contexts -o name'
    or
    `$ kubectl config current-context")
    (defun qz/choose-kubectl-context (ctx)
      (interactive)
      (setq qz/kubectl-context
            (or ctx (completing-read "k8s ctx: "
                                     (qz/shell-command-to-list-of-strings
                                      "kubectl config get-contexts -o name"))))
      (async-shell-command (format "kubectl config use-context %s" 
                                   qz/kubectl-context)
                           "*kubectl*"))
    
    ;; optional; quality of life improvement to bury kubectl buffer
    (add-to-list 'display-buffer-alist '("*kubectl*" display-buffer-no-window))
    (defun qz/get-mail ()
      (interactive)
      (async-shell-command "mbsync -Va && notmuch new"))
    (defun qz/rde-sanity ()
      (interactive)
      (async-shell-command
       (concat "cd $HOME/git/sys/rde"
               "&& guix repl -L . sanity.scm")))
    (defun qz/reload-config-home ()
      (interactive)
      (org-babel-tangle-file
       "~/git/sys/rde/rde/examples/abcdw/emacs.org")
      (sleep-for .5)
      (async-shell-command
       (concat "cd $HOME/git/sys/rde/rde/examples/abcdw/ "
               "&& make ixy-home-reconfigure")))
    
    (defun qz/reload-config-system ()
      (interactive)
      (async-shell-command
       (concat "cd $HOME/git/sys/rde/rde/examples/abcdw/ "
               "&& sudo -E make ixy-system-reconfigure")))
    
    (defun qz/reload-config-emacs ()
      (interactive)
      (load-file "~/.config/emacs/init.el"))
    
    (defun qz/reload-guix-pins ()
      (interactive)
      (async-shell-command
       (concat "cd $HOME/git/sys/rde/ "
               "&& make channels-update-lock && make channels-pull")))
    
    (defun qz/guix-upgrade ()
      (interactive)
      (async-shell-command
       (concat "cd $HOME/git/sys/rde"
               "&& make channels-update-lock && make channels-pull && guix upgrade && make")))
    (setq tramp-cache-read-persistent-data t)
    (require 'perfect-margin)
    
    (perfect-margin-mode 1)
    (setq perfect-margin-ignore-regexps nil
          perfect-margin-ignore-filters nil)
    (custom-set-variables
     '(cursor-type 'hbar))
    (defun hi-lock-face-symbol-at-point ()
      "Highlight each instance of the symbol at point.
    Uses the next face from `hi-lock-face-defaults' without prompting,
    unless you use a prefix argument.
    Uses `find-tag-default-as-symbol-regexp' to retrieve the symbol at point.
    
    If REGEXP contains upper case characters (excluding those preceded by `\\')
    and `search-upper-case' is non-nil, the matching is case-sensitive.
    
    This uses Font lock mode if it is enabled; otherwise it uses overlays,
    in which case the highlighting will not update as you type.  The Font
    Lock mode is considered \"enabled\" in a buffer if its `major-mode'
    causes `font-lock-specified-p' to return non-nil, which means
    the major mode specifies support for Font Lock."
      (interactive)
      (let* ((regexp (hi-lock-regexp-okay
                      (find-tag-default-as-symbol-regexp)))
             (hi-lock-auto-select-face t)
             (face (hi-lock-read-face-name)))
        (or (facep face)
            (setq face (or (and hi-lock-auto-select-face (hi-lock-read-face-name))
                           'hi-yellow)))
        (unless hi-lock-mode (hi-lock-mode 1))
        (hi-lock-set-pattern
         regexp face nil nil
         (if (and case-fold-search search-upper-case)
             (isearch-no-upper-case-p regexp t)
           case-fold-search))))
    (defvar qz/font-initial-size (face-attribute 'default :height))
    (defvar qz/resize-mini-windows-initial resize-mini-windows)
    (defvar qz/max-mini-window-height-initial max-mini-window-height)
    
    (defun qz/reset-visual-initial ()
      (interactive)
      (set-face-attribute 'default nil :height qz/font-initial-size)
      (setq resize-mini-windows    qz/resize-mini-windows-initial
            max-mini-window-height qz/max-mini-window-height-initial))
    (defun qz/font-big-80 ()
      (interactive)
      (set-face-attribute 'default nil :height 300)
      (setq resize-mini-windows t
            max-mini-window-height nil))
    (defvar qz/unsplash-tags nil)
    (defun qz/unsplash ()
      "yet another lazy shell-command wrapper; wallpaper edition"
      (interactive)
      (let ((tag (read-from-minibuffer
                  "unsplash tags: " (car qz/unsplash-tags))))
        (async-shell-command
         (format "TAGS='%s'
    mv \"$XDG_CACHE_HOME/wallpaper.png\" \"$XDG_CACHE_HOME/$(date +%%Y-%%m-%%d--%%H-%%M-%%S)-wallpaper.png\"
    curl -L \"https://source.unsplash.com/5120x1440?$TAGS\" -o \"$XDG_CACHE_HOME/wallpaper.png\"
    swaymsg output \"*\" background ~/.cache/wallpaper.png fill" tag))
        (setq qz/unsplash-tags (seq-uniq (cons tag qz/unsplash-tags)))))
    ;; NOWEB CONF END
    ))
