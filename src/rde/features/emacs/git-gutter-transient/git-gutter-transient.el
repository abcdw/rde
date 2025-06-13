;;; git-gutter-transient.el --- Transient additions for git-gutter

;; Copyright © 2021, 2025 Andrew Tropin <andrew@trop.in>

;; Author: Andrew Tropin <andrew@trop.in>
;;
;; URL: https://trop.in/rde
;; Keywords: convenience

;; This file is part of rde.

;; rde is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; rde is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with rde.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface for fast navigation between hunks and easier access to various
;; git commands.

;;; Code:

(require 'git-gutter)
(require 'transient)
(require 'magit)

(autoload 'transient-define-prefix "transient" "" nil 'macro)

(defun git-gutter-transient:next-hunk (arg)
  "Move to next diff hunk"
  (interactive "p")
  (git-gutter:next-hunk arg)
  (git-gutter:popup-hunk))

(defun git-gutter-transient:previous-hunk (arg)
  "Move to previous diff hunk"
  (interactive "p")
  (git-gutter-transient:next-hunk (- arg)))

(defun git-gutter-transient:first-hunk (arg)
  "Move to first diff hunk"
  (interactive "p")
  (goto-char (point-max))
  (git-gutter-transient:next-hunk 1))

(defun git-gutter-transient:last-hunk (arg)
  "Move to last diff hunk"
  (interactive "p")
  (goto-char (point-min))
  (git-gutter-transient:next-hunk -1))

(defun git-gutter-transient:stage-hunk (arg)
  "Stage hunk without asking and go to next ARG times."
  (interactive "p")
  (git-gutter-transient:quit)
  (dotimes (_ arg)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
      (git-gutter:stage-hunk))
    (git-gutter:next-hunk 1))
  (condition-case nil (git-gutter:popup-hunk) (error nil)))

(defun git-gutter-transient:quit ()
  "Quit git-gutter:popup-buffer."
  (interactive)
  (when-let (w (get-buffer-window git-gutter:popup-buffer))
    (quit-window t w)))

(defun git-gutter-transient:quit-and-disable ()
  "Quit."
  (interactive)
  (git-gutter-transient:quit)
  (git-gutter-mode -1))

(defun git-gutter-transient:magit ()
  "Quit and magit."
  (interactive)
  (git-gutter-transient:quit)
  (magit))

(defun git-gutter-transient:magit-dispatch ()
  "Close hunk buffer and call `magit-file-dispatch'."
  (interactive)
  (git-gutter-transient:quit)
  (magit-file-dispatch))

;; transient tutorial: https://github.com/positron-solutions/transient-showcase

;;;###autoload (autoload 'git-gutter-transient "git-gutter-transient" nil t)
(transient-define-prefix git-gutter-transient ()
  :transient-suffix     'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  ["Hunk actions"
   [("p" "Previous" git-gutter-transient:previous-hunk)]
   [("n" "Next" git-gutter-transient:next-hunk)]
   [("M-<" "First" git-gutter-transient:first-hunk)]
   [("M->" "Last" git-gutter-transient:last-hunk)]
   [("s" "Stage" git-gutter-transient:stage-hunk)]
   [("k" "Discard" git-gutter:revert-hunk)]]
  [[("g" "Magit file dispatch" git-gutter-transient:magit-dispatch
     :transient transient--do-replace)]
   [("q" "Quit" git-gutter-transient:quit :transient transient--do-exit)]
   [("Q" "Quit and disable" git-gutter-transient:quit-and-disable
     :transient transient--do-exit)]]

  (interactive)
  (git-gutter-mode)
  ;; (if (condition-case nil
  ;;         (git-gutter:search-here-diffinfo git-gutter:diffinfos)
  ;;       (error nil))
  ;;     (git-gutter:popup-hunk)
  ;;   (condition-case nil (git-gutter-transient:next-hunk 1) (error nil)))
  (transient-setup 'git-gutter-transient))

(provide 'git-gutter-transient)

;; (defun git-gutter:popup-hunk-new (&optional diffinfo)
;;   "Popup current diff hunk."
;;   (interactive)
;;   (git-gutter:awhen (or diffinfo
;;                         (git-gutter:search-here-diffinfo git-gutter:diffinfos))
;;                     (save-selected-window
;;                       (display-buffer (git-gutter:update-popuped-buffer it)))))

;; (advice-add 'git-gutter:popup-hunk :override 'git-gutter:popup-hunk-new)
