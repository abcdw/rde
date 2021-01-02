;; This file used compile time for expanding use-package declarations

(progn
  ;; (setq use-package-enable-imenu-support t)
  ;; (setq use-package-compute-statistics t)
  (setq use-package-verbose nil)
  (setq use-package-expand-minimally t)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  ;; without ivy `describe-variable' won't suggest after-init-hook for
  ;; after-init
  (setq use-package-hook-name-suffix nil))

(require 'use-package)

;;; May improve startup time, but doesn't work with compute-statistics
;; (eval-when-compile
;;   (require 'use-package))
;; (require 'bind-key)

;; TODO: add use-package info to info dirs
;; https://jwiegley.github.io/use-package/installation/

(provide 'rde-use-package)
