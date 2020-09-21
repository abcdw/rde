(use-package faces
  :config
  (let* ((font-size (number-to-string rde/font-size))
	 (default-font (string-join (list rde/font-family font-size) " ")))

  (set-face-attribute 'default nil :font default-font)
  (add-to-list 'default-frame-alist `(font . ,default-font))))
