(define-module (rde features docker)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu services docker)
  #:use-module (rde system services accounts)

  #:export (feature-docker))

(define* (feature-docker
          #:key
          (docker docker)
          (docker-cli docker-cli)
          (containerd containerd))
  "Configure docker and related packages."

  (define f-name 'docker)
  (define (get-home-services config)
    (list
     (elisp-configuration-service
      'docker
      `((eval-when-compile (require 'configure-rde-keymaps))
        (with-eval-after-load
         'configure-rde-keymaps
         (define-key rde-app-map (kbd "d") 'docker))
        (add-to-list 'auto-mode-alist '(".*Dockerfile\\'" . dockerfile-mode)))
      ;; MAYBE: Add emacs-docker-tramp?
      #:elisp-packages (list emacs-docker emacs-dockerfile-mode
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (define (get-system-services config)
    (list
     (simple-service
      'docker-add-docker-group-to-user
      rde-account-service-type
      (list "docker"))
     (service
      docker-service-type
      (docker-configuration
       (docker docker)
       (docker-cli docker-cli)
       (containerd containerd)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
