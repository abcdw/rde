(define-module (rde features golang)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (feature-go))

(define* (feature-go
          #:key
          (go go)
          (emacs-go-mode emacs-go-mode)
          (emacs-ob-go emacs-ob-go))
  "Configure and set up tooling for Golang."
  (ensure-pred file-like? go)
  (ensure-pred file-like? emacs-go-mode)
  (ensure-pred file-like? emacs-ob-go)

  (define f-name 'go)

  (define (get-home-services config)
    "Return home services related to Golang."
    (append
     (list
      (simple-service
       'add-go-home-package
       home-profile-service-type
       (list go))
      (simple-service
       'add-go-envs
       home-environment-variables-service-type
       '(("GOPATH" . "$XDG_DATA_HOME/go"))))
     (if (get-value 'emacs config #f)
         (list
          (rde-elisp-configuration-service
           f-name
           config
           `((add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
             ,@(if (get-value 'emacs-org config #f)
                   '((with-eval-after-load 'org
                       (add-to-list 'org-structure-template-alist
                                    '("go" . "src go")))
                     (with-eval-after-load 'ob-core
                       (require 'ob-go))
                     (with-eval-after-load 'ob-go
                       (setq org-babel-default-header-args:go
                             '((:imports . "fmt")))))
                   '()))
           #:elisp-packages (list emacs-go-mode emacs-ob-go)))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . ,go)))
   (home-services-getter get-home-services)))
