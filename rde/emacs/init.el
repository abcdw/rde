;; (require 'rde-core)
;; (require 'rde-variables)
;; (require 'rde-faces)
;; (require 'rde-org-roam)
;; (require 'rde-modus-themes)

;; (require 'orderless)
;; (setq completion-styles '(orderless))

(setq org-adapt-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq user-full-name "Andrew Tropin")
(setq user-mail-address "andrew@trop.in")
(setq message-auto-save-directory "~/.cache/mail-drafts")

(load-file "~/work/gnu/guix/etc/copyright.el")
(setq copyright-names-regexp
      (format "%s <%s>" user-full-name user-mail-address))

(require 'yasnippet)
(yas-global-mode 1)
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/work/gnu/guix/etc/snippets"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d2db4af7153c5d44cb7a67318891e2692b8bf5ddd70f47ee7a1b2d03ad25fcd9" "a10ca93d065921865932b9d7afae98362ce3c347f43cb0266d025d70bec57af1" default))
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval let
	   ((root-dir-unexpanded
	     (locate-dominating-file default-directory ".dir-locals.el")))
	   (when root-dir-unexpanded
	     (let*
		 ((root-dir
		   (expand-file-name root-dir-unexpanded))
		  (root-dir*
		   (directory-file-name root-dir)))
	       (unless
		   (boundp 'geiser-guile-load-path)
		 (defvar geiser-guile-load-path 'nil))
	       (make-local-variable 'geiser-guile-load-path)
	       (require 'cl-lib)
	       (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
	   (locate-dominating-file default-directory ".dir-locals.el"))))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
