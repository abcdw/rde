;; C-u C-x = to get face under cursor
(use-package faces
  ;; string-join defined in subr-x
  :after subr-x
  :demand
  :config
  (let* ((font-size (number-to-string rde/font-size))
	 (default-font (string-join (list rde/font-family font-size) " ")))

  (set-face-attribute 'default nil :font default-font)
  (add-to-list 'default-frame-alist `(font . ,default-font))
  ;; (set-face-attribute 'variable-pitch nil :font "Source Sans Pro-10")
  (set-face-attribute 'variable-pitch nil :font "Open Sans-10")
  ;; Without it tables becomes missaligned
  (set-face-attribute 'link nil :inherit '(fixed-pitch button))
  ))
