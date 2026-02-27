(define-module (rde features difftastic)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix gexp)
  #:export (feature-difftastic))

(define* (feature-difftastic
          #:key
          (difftastic difftastic)
          (emacs-difftastic emacs-difftastic)
          (display "inline")
          (parse-error-limit #f))
  "Setup and configure Difftastic, a structural diff tool."
  (ensure-pred file-like? difftastic)
  (ensure-pred file-like? emacs-difftastic)
  (ensure-pred string? display)
  (ensure-pred maybe-integer? parse-error-limit)

  (define f-name 'difftastic)

  (define difft-cmd
    (let ((difft (file-append difftastic "/bin/difft")))
      (program-file
       "difft-wrapper"
       #~(apply execl #$difft "difft"
                "--display" #$display
                #$@(if parse-error-limit
                       (list "--parse-error-limit"
                             (number->string parse-error-limit))
                       '())
                (cdr (command-line))))))

  (define (get-home-services config)
    (list
     (simple-service
      'add-difftastic-home-package
      home-profile-service-type
      (list difftastic))
     (simple-service
      'difftastic-git-config
      home-git-service-type
      (home-git-extension
       (config
        `((diff
           ((external . ,difft-cmd)))))))

     (rde-elisp-configuration-service
      'difftastic
      config
      `((setopt difftastic-executable ,difft-cmd)
        (with-eval-after-load 'magit
          (difftastic-bindings-mode)))
      #:summary "Difftastic structural diff integration"
      #:commentary "Set executable path and enable magit keybindings."
      #:keywords '(convenience)
      #:elisp-packages (list emacs-difftastic))))

  (feature
   (name f-name)
   (values `((,f-name . ,difftastic)
             (emacs-difftastic . ,emacs-difftastic)))
   (home-services-getter get-home-services)))
