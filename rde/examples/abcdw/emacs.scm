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
    (defun qz/revert-buffer-no-confirm ()
      "Revert buffer without confirmation."
      (interactive)
      (revert-buffer :ignore-auto :noconfirm))
    ;; NOWEB GENERAL END

    ;; NOWEB CONF START
    ;; NOWEB KBD START
    (define-key global-map (kbd "M-s-h") 'windmove-swap-states-left)
    (define-key global-map (kbd "M-s-j") 'windmove-swap-states-down)
    (define-key global-map (kbd "M-s-k") 'windmove-swap-states-up)
    (define-key global-map (kbd "M-s-l") 'windmove-swap-states-right)
    (define-key global-map (kbd "s-h") 'windmove-left)
    (define-key global-map (kbd "s-j") 'windmove-down)
    (define-key global-map (kbd "s-k") 'windmove-up)
    (define-key global-map (kbd "s-l") 'windmove-right)
    (define-key global-map (kbd "s-\\") 'org-store-link)
    ;; NOWEB KBD END
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
    (defun qz/yq-interactively ()
      "haha yaml loophole"
      (interactive)
      (let ((jq-interactive-command "yq"))
        (call-interactively 'jq-interactively)))
    (require 'hyperbole)
    (with-eval-after-load 'org
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
      (defun qz/files-agenda ()
        (seq-uniq (append qz/org-agenda-files (qz/project-files))))
      
      (defun qz/agenda-files-update (&rest _)
        "Update the value of `org-agenda-files' with relevant candidates"
        (interactive)
        (setq org-agenda-files (qz/files-agenda)
              qz/agenda-daily-files (qz/agenda-daily-files-f)))
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
      
                                              ;(qz/pprint org-agenda-custom-commands)
      (defun qz/agenda-daily-files-f ()
        (seq-filter (lambda (s) (string-match qz/daily-title-regexp s))
                    org-agenda-files))
      (defun qz/project-files ()
        "Return a list of note files containing Project tag."
        (seq-map
         'car
         (org-roam-db-query
          [:select :distinct file
                   :from tags
                   :inner :join nodes
                   :on (= tags:node_id nodes:id)
                   :where (= tags:tag "project")])))
      (setq qz/org-agenda-files
            (mapcar (lambda (f) (expand-file-name (format "%s/%s" org-roam-directory f)))
                    '("calendar-home.org" "calendar-work.org" "schedule.org")))
      (setq qz/org-agenda-prefix-length 30
            org-agenda-prefix-format 
            '((agenda . " %i Emacs Configuration %?-12t% s")
              (todo . " %i Emacs Configuration  ")
              (tags . " %i Emacs Configuration  ")
              (search . " %i Emacs Configuration  ")))
      
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
         (elasticsearch . t)
         (restclient . t)
         (R . t)))
      (with-eval-after-load 'org-roam
      
        (defvar qz/org-babel-lob-ingest-files
          (list (org-roam-node-file (org-roam-node-from-title-or-alias "NewStore")))
          "files from which named `src' blocks should be loaded")
      
        (defun qz/org-babel-do-lob-ingest-files (&optional files)
          (interactive)
          (mapcar (lambda (f) (org-babel-lob-ingest f))
                  (append qz/org-babel-lob-ingest-files files)))
      
        (qz/org-babel-do-lob-ingest-files))
      (defun qz/org-babel-choose-block (&optional lob)
        "choose block, insert scaffold for args.
      
      might honestly be better to generate `yas' template when we load
      blocks with `qz/org-babel-do-lob-ingest-files', but I've never used
      yas so idk"
        (interactive)
        (let ((lob (or lob
                       (intern (completing-read
                                "lob: " (mapcar 'car org-babel-library-of-babel))))))
          (with-current-buffer (current-buffer)
            (end-of-line)
            (newline)
            (insert (format "#+name: call-%s\n#+call: %s" lob lob))
      
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
      (defun qz/org-babel--list->rows (name lst)
        (cons (list name)
              (cons 'hline (mapcar 'list lst))))
      (defun qz/org-inbox-capture ()
        (interactive)
        "Capture a task in agenda mode."
        (org-capture nil "i"))
      (with-eval-after-load 'org-roam
        ;; NOWEB ROAM START
        (setq qz/org-roam-dailies-filespec "private-%<%Y-%m-%d>.org")
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
        ;;; ref capture
        (setq org-roam-capture-ref-templates
              `(("r" "ref" plain
                 "%?"
                 :if-new (file+head ,qz/capture-title-timestamp-roam
                                    "#+title: ${title}\n")
                 :unnarrowed t)))
        ;; NOWEB ROAM END
        )
      (setq org-confirm-babel-evaluate nil)
      ;; [[file:~/.doom.d/config.org::*refile][refile]]
      (setq org-refile-targets '(("next.org" :level . 0)
                                 ("reading.org" :level . 0)
                                 ("emacs.org" :level . 0)
                                 ("watching.org" :level . 0)
                                 ("learning.org" :level . 0)
                                 ("inbox.org" :level . 0)
                                 ("sample.org" :level . 0)
                                 ("wip.org" :level . 0)))
      ;; NOWEB ORG END
      )
    (with-eval-after-load 'pdf-view
      (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))
    (defvar qz/restclient-environment nil)
    
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
    (defun qz/get-mail ()
      (interactive)
      (async-shell-command "mbsync -Va && notmuch new"))
    
    (defun qz/reload-config-home ()
      (interactive)
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
    ;; NOWEB CONF END
    ))
