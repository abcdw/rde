;; Packages will be initialized by guix later.
(setq package-enable-at-startup nil)
(setq package-archives nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)
;; TODO: Probably the better approach is:
;; (setq inhibit-x-resources t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
;; (setq comp-deferred-compilation nil)
(setq native-comp-deferred-compilation nil)

