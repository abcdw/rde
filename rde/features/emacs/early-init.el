;; Packages will be initialized by use-package later.
(setq package-enable-at-startup nil)
(setq package-archives nil)

;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-threshold 134217728)   ; 128mb

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)
;; TODO: Probably the better approach is:
;; (setq inhibit-x-resources t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
(setq comp-deferred-compilation nil)
