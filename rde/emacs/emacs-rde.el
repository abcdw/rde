
(eval-and-compile
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  ;; without ivy `describe-variable' won't suggest after-init-hook for
  ;; after-init
  (setq use-package-hook-name-suffix nil))

(require 'use-package)


(use-package which-key
  :config
  (which-key-mode))

;; (provide 'rde-configs)
