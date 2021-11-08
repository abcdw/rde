;; this will be much tidier from literate config, with each standard elisp block as :noweb
      (define-key key-translation-map [?\C-x] [?\C-u])
      (define-key key-translation-map [?\C-u] [?\C-x])

      (define-key global-map (kbd "s-h") 'windmove-left)
      (define-key global-map (kbd "C-j") 'windmove-down)
      (define-key global-map (kbd "C-k") 'windmove-up)
      (define-key global-map (kbd "C-l") 'windmove-right)

      ;; [[file:~/.doom.d/config.org::*refile][refile]]
      (setq org-refile-targets '(("next.org" :level . 0)
                           ("reading.org" :level . 0)
                           ("emacs.org" :level . 0)
                           ("watching.org" :level . 0)
                           ("learning.org" :level . 0)
                           ("inbox.org" :level . 0)
                           ("sample.org" :level . 0)
                           ("wip.org" :level . 0)))

(setq qz/capture-title-timestamp "20210813T161035Z-${slug}")

;; [[file:~/.doom.d/config.org::*templates][templates]]
(setq org-capture-templates
      `(("i" "inbox" entry
         (file ,(concat qz/org-agenda-directory "inbox.org"))
         "* TODO %? \nCREATED: %u\nFROM: %a")
        ;; spanish language capturing
        ("v" "vocab; spanish" entry
         (file+headline ,(concat qz/notes-directory "spanish_language.org") "vocab, phrases")
         "** \"%?\" :es:\nFROM: %a\n\n*** :en:\n")
        ;; capture link to live `org-roam' thing
        ("n" "now, as in NOW" entry (file ,(concat qz/org-agenda-directory "wip.org"))
         "* TODO [#A1] %? \nDEADLINE: %T\nCREATED: %u")
        ;; fire directly into inbox
        ("c" "org-protocol-capture" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\nCREATED: %u\n\n#+begin_quote\n\n%i\n\n#+end_quote"
         :immediate-finish t)
        ;; push last captured item into inbox
        ("l" "last-capture" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         (function qz/inbox-last-captured)
         :immediate-finish t)
        ("I" "current-roam" entry (file ,(concat qz/org-agenda-directory "inbox.org"))
         (function qz/current-roam-link)
         :immediate-finish t)
        ("t" "tangent" entry (file+headline ,(concat org-roam-dailies-directory
                                            (qz/today-as-daily-file))
                                       "tangent")
         ,(s-join "\n" '("* TANGENT [%<%H:%M>] %?"
                         "CREATED: <%<%Y-%m-%d %H:%M>>"
                         "FROM: %a")))
        ("w" "weekly review" entry
         (file+datetree ,(concat qz/org-agenda-directory "reviews.org"))
         (file ,(concat qz/org-agenda-directory "templates/weekly_review.org")))))

;; [[file:~/.doom.d/config.org::*capture convenience functions][capture convenience functions]]
(defun qz/current-roam-link ()
   "Get link to org-roam file with title"
  (interactive)
  (concat "* TODO "
          (let ((n (qz/org-roam-node-at-point)))
            (org-link-make-string
             (concat "id:" (org-roam-node-id n))
             (org-roam-node-title n)))))

(defun qz/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(defun qz/org-daily-tangent-capture ()
  (interactive)
  "Capture the inevitable tangent"
  (org-capture nil "t"))

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

;; [[file:~/.doom.d/config.org::*capture templates][roam capture templates]]

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


;;; ref capture
(setq org-roam-capture-ref-templates
      `(("r" "ref" plain
         "%?"
         :if-new (file+head ,qz/capture-title-timestamp-roam
                            "#+title: ${title}\n")
         :unnarrowed t)))

;;; dailies
(setq
 qz/org-roam-dailies-filespec "private-%<%Y-%m-%d>.org"
 org-roam-dailies-capture-templates
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
            (nth (1- (string-to-number month)) qz/month-lookup)
            (or (if-let ((node (org-roam-node-from-title-or-alias year)))
                  (org-link-make-string
                   (concat "id:" (org-roam-node-id node))
                   (org-roam-node-title node)))
                year))))


;;;; END OF EXTRA CONFIG
