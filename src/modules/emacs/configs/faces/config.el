(use-package faces
  :config
  ;; (defvar default-font-spec
  ;;   (font-spec :family rde/font-family
  ;; 	       :weight 'semi-light
  ;; 	       :size rde/font-size))

  (defvar default-font nil)
  (setq default-font
    (string-join (list rde/font-family (number-to-string rde/font-size)) " "))

  (set-face-attribute 'default nil :font default-font)
  (add-to-list 'default-frame-alist `(font . ,default-font)))
