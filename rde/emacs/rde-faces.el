;; C-u C-x = to get face under cursor
(eval-when-compile
  (require 'rde-core))

(use-package faces
  :demand
  :config
  (progn
    ;; string-join defined in subr-x
    (require 'subr-x)
    (let* ((font-size (number-to-string rde/font-size))
	   (default-font (string-join (list rde/font-family font-size) " "))
	   (sans-font (string-join (list "DejaVu Sans" font-size) " ")))

      (set-face-attribute 'default nil :font default-font)
      (add-to-list 'default-frame-alist `(font . ,default-font))
      ;; (set-face-attribute 'variable-pitch nil :font "Source Sans Pro-10")
      (set-face-attribute 'variable-pitch nil :font sans-font))))

(provide 'rde-faces)
