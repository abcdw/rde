;;; gider-tests.el --- Tools for running rde tests -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright © 2023 Andrew Tropin <andrew@trop.in>

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


(require 'geiser-debug)
(require 'geiser-mode)
(require 'geiser-guile)

;; (defgroup gider nil
;;   "Customization for gider flavour."
;;   :group 'gider)

(defvar gider-scheme-dir
  (expand-file-name "src" (file-name-directory load-file-name))
  "Directory where the scheme gider modules are installed.")

(defun gider--summary-to-string (summary)
  (let ((pair-to-string (lambda (x) (format "%s: %s" (car x) (cdr x)))))
    (concat
     (seq-reduce
      (lambda (acc x) (concat acc (funcall pair-to-string x) ", "))
      (-drop-last 1 summary) "")
     (funcall pair-to-string (car (last summary))))))

(defun gider--get-error-face ()
  ;; TODO: Invalid face refernce
  `((t :background ,(face-foreground 'error)
       :foreground ,(face-background 'default))))

(defun gider--get-success-face ()
  `((t :background ,(face-foreground 'success)
       :foreground ,(face-background 'default))))

(defun gider--wrap-code (code)
  `(:eval (:scm
           ,(if (stringp code)
                code
              (format "%s" code)))))

(defun gider-reload-module ()
  "Remove all the stale state and completely reload the current
module."
  (interactive)
  (geiser-eval--send
   (gider--wrap-code
    `(let ((m (current-module)))
       (module-clear! m)
       (reload-module m)))
     (lambda (ret)
       (message "Module reloaded: %s" ret))
     (current-buffer)))

(defsubst gider-eval-print--wrap-region (str)
  (format "((@ (ice-9 pretty-print) pretty-print) %s\n)" str))

;; (defun gider-eval-print--unwrap (str)
;;   (if (string-match "((@ (ice-9 pretty-print) pretty-print) \
;; (begin[ \t\n\v\r]+\\(.+\\)*))" str)
;;       (match-string 1 str)
;;     str))

(defun gider-eval-print--send-region (compile start end and-go wrap &optional nomsg)
  "Evaluate (or COMPILE) the region delimited by START and END.
The result of the evaluation is reported asynchronously, so this
call is not blocking. If AND-GO is t, also jump to the repl
buffer.  If WRAP is t, the region's content is wrapped in a begin
form.  The flag NOMSG can be used to avoid reporting of the
result in the minibuffer."
  (let* ((str (buffer-substring-no-properties start end))
         (wrapped (gider-eval-print--wrap-region str))
         (code `(,(if compile :comp :eval) (:scm ,wrapped)))
         (cont (lambda (ret)
                 (let ((res (geiser-eval--retort-result-str ret nil))
                       (scstr (geiser-syntax--scheme-str str)))
                   (when and-go (funcall and-go))
                   (unless (geiser-eval--retort-error ret)
                     (save-excursion
                       (goto-char (/ (+ end start) 2))
                       (geiser-autodoc--clean-cache))
                     (unless nomsg
                       (save-match-data
                         (when (string-match "\\(?:[ \t\n\r]+\\)\\'" res)
                           (setq res (replace-match "" t t res))))
                       (message "%s" res)))
                   (geiser-debug--display-retort scstr ret res)))))
    (geiser-eval--send code cont (current-buffer))))

(defun gider-eval-print-region (start end &optional and-go raw nomsg)
  "Eval the current region in the Geiser REPL.

With prefix, goes to the REPL buffer afterwards (as
`geiser-eval-region-and-go').  The evaluation is performed
asynchronously: this function's return value can be used to wait
for its completion using `geiser-eval-wait'.  See also
`geiser-eval-region/wait' if you just need to eval a region
programmatically in a synchronous way."
  (interactive "rP")
  (save-restriction
    (narrow-to-region start end)
    (check-parens))
  (gider-eval-print--send-region nil
                                 start
                                 end
                                 (and and-go 'geiser--go-to-repl)
                                 (not raw)
                                 nomsg))

(defun gider-eval-print-last-sexp (print-to-buffer-p)
  "Eval the previous sexp in the Geiser REPL and print the result.

With a prefix, revert the effect of `geiser-mode-eval-last-sexp-to-buffer' "
  (interactive "P")
  (let* (bosexp
         (eosexp (save-excursion (backward-sexp)
                                 (setq bosexp (point))
                                 (forward-sexp)
                                 (point)))
         (ret-transformer (or geiser-mode-eval-to-buffer-transformer
                              (lambda (msg is-error?)
                                (format "%s%s%s"
                                        geiser-mode-eval-to-buffer-prefix
                                        (if is-error? "ERROR" "")
                                        msg))))
         (ret (save-excursion
                (gider-eval-print-region bosexp ;beginning of sexp
                                         eosexp ;end of sexp
                                         nil
                                         t
                                         print-to-buffer-p)))
         (ret (geiser-wait-eval ret 30))
         (err (geiser-eval--retort-error ret))
         (will-eval-to-buffer (if print-to-buffer-p
                                  (not geiser-mode-eval-last-sexp-to-buffer)
                                geiser-mode-eval-last-sexp-to-buffer))
         (str (geiser-eval--retort-result-str ret
                                              (when will-eval-to-buffer ""))))
    (cond  ((not will-eval-to-buffer) str)
           (err (insert (funcall ret-transformer
                                 (geiser-eval--error-str err) t)))
           ((string= "" str))
           (t (push-mark)
              (insert (funcall ret-transformer str nil))))))

(defun gider-test--run-some-tests (guile-code)
  (message "Running tests...")
  (let* ((code-wrapped (gider--wrap-code guile-code))
         (error-face (gider--get-error-face))
         (success-face (gider--get-success-face)))
    (geiser-eval--send
     code-wrapped
     (lambda (ret)
       (let* ((summary (read (car (alist-get 'result ret))))
              (fail-count (alist-get 'fail summary))
              (summary-string (gider--summary-to-string summary)))
         (if (zerop fail-count)
             (message (concat (propertize " ✓ " 'face success-face)
                              " Passed all tests! %s") summary-string)
           (message (concat (propertize " ⚠ " 'face error-face)
                            " Failed some tests! %s")
                    summary-string))
         (geiser-debug--display-retort
          "Test summary"
          ;; TODO: Maybe use something more meaningful instead of ret?
          ret
          (geiser-eval--retort-result-str ret nil))))
     (current-buffer))))

(defun gider-test-run-module-tests ()
  (interactive)
  (gider-test--run-some-tests
   '(begin
     (use-modules (gider test-runners))

     (set! %previous-runner (run-module-tests (get-test-module)))
     (test-runner-summary %previous-runner))))

(defun gider-test-rerun-tests ()
  (interactive)
  (gider-test--run-some-tests
   '(begin
     (use-modules (gider test-runners))

     (set! %previous-runner (rerun-tests %previous-runner))
     (test-runner-summary %previous-runner))))

(defun gider-test-rerun-failed-tests ()
  (interactive)
  (gider-test--run-some-tests
   "(begin
     (use-modules (gider test-runners))

     (set! %previous-runner
           (rerun-tests
            %previous-runner
            #:filter-fn (lambda (x) (equal? 'fail (assoc-ref x 'status)))))
     (test-runner-summary %previous-runner))"))

;; TODO: run-module-tests doesn't work on partially reevaluated module (when
;; test re-evaled).

(defvar gider-test-commands-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t") 'gider-test-rerun-tests)
    (define-key map (kbd "C-m") 'gider-test-run-module-tests)
    (define-key map (kbd "C-r") 'gider-test-rerun-tests)
    (define-key map (kbd "C-f") 'gider-test-rerun-failed-tests)
    map))


;;;###autoload
(define-minor-mode gider-mode
  "Enable gider things."
  :global t
  :group 'geiser
  (if gider-mode
      (progn
        (add-to-list 'geiser-guile-load-path gider-scheme-dir)
        (define-key geiser-mode-map (kbd "C-c C-t") gider-test-commands-map)
        ;; TODO: Reimplement it by changing result print function.
        (define-key geiser-mode-map (kbd "C-c C-p") 'gider-eval-print-last-sexp))
    (progn
      (delete gider-scheme-dir geiser-guile-load-path)
      ;; (keymap-unset (kbd "C-c C-t") geiser-mode-map 'remove)
      ;; (keymap-unset (kbd "C-c C-p") geiser-mode-map 'remove)
      (define-key geiser-mode-map (kbd "C-c C-t") nil)
      (define-key geiser-mode-map (kbd "C-c C-p") nil))))

(provide 'gider-tests)
