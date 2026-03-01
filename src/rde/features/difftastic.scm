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


;;; Notes
;; [Andrew Tropin, 2026-03-01]

;; - difftastic can't be extended with new tree-sitter grammars without
;; rebuilding the binary.

;; If grammar fails to parse (gexp in scheme code for example), you can still
;; get the diff by ignoring errors with parse-error-limit, however the diff
;; will likely be wrong.

;; The display is not very flexible, you can't disable line numbers or adjust
;; other things.

;; The highlighting of elements of the line, feels unusual and a bit
;; stylistically questionable.  Not sure, what are better diff representation
;; options here.

;; The integration with magit is bare bone, you can get structural diff for
;; staged changes or a particular commit, but not in magit-status.

;; Summary and overall experience: parsing of builting scheme grammar is not
;; very reliable, customization of grammars requires rebuilt of the package.
;; Can be used for reviewing large edits, removes noise well (if parsed
;; correctly).  Requeires more work and improvements (scheme grammar, more
;; flexible display format, better emacs integration) to become a goto daily
;; driver.

(define* (feature-difftastic
          #:key
          (difftastic difftastic)
          (emacs-difftastic emacs-difftastic)
          (display "inline")
          (parse-error-limit #f)
          (git-integration? #f))
  "Setup and configure Difftastic, a structural diff tool."
  (ensure-pred file-like? difftastic)
  (ensure-pred file-like? emacs-difftastic)
  (ensure-pred string? display)
  (ensure-pred maybe-integer? parse-error-limit)
  (ensure-pred boolean? git-integration?)

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
    (append
     (if git-integration?
         (list
          (simple-service
           'difftastic-git-config
           home-git-service-type
           (home-git-extension
            (config
             `((diff
                ((external . ,difft-cmd))))))))
         '())
     (list
      (simple-service
       'add-difftastic-home-package
       home-profile-service-type
       (list difftastic))
      (rde-elisp-configuration-service
       'difftastic
       config
       `((setopt difftastic-executable ,difft-cmd)
         (with-eval-after-load 'magit
           (difftastic-bindings-mode)))
       #:summary "Difftastic structural diff integration"
       #:commentary "Set executable path and enable magit keybindings."
       #:keywords '(convenience)
       #:elisp-packages (list emacs-difftastic)))))

  (feature
   (name f-name)
   (values `((,f-name . ,difftastic)
             (emacs-difftastic . ,emacs-difftastic)))
   (home-services-getter get-home-services)))
