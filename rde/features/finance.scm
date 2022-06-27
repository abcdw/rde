(define-module (rde features finance)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-ledger))


;;;
;;; ledger.
;;;

(define* (feature-ledger
          #:key
          (ledger ledger)
          (emacs-ledger-mode emacs-ledger-mode))
  "Setup and configure ledger related things."
  (ensure-pred file-like? ledger)
  (ensure-pred file-like? emacs-ledger-mode)

  (define (get-home-services config)
    (define emacs-f-name 'ledger)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))

    (list
     (simple-service
      'ledger-add-ledger-package
      home-profile-service-type
      (list ledger)) ;; Needed for info pages

     ;; TODO: Build emacs-ledger with ledger specified in feature argument.
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((with-eval-after-load
           'ledger-mode
           (setq ledger-default-date-format ledger-iso-date-format)))
        #:summary "\
Tweaks for ledger-mode"
        #:commentary "\
Use ISO date."
        #:keywords '(convenience)
        #:elisp-packages (list (get-value 'emacs-configure-rde-keymaps config)
                               emacs-ledger-mode)))))

  (feature
   (name 'ledger)
   (values `((ledger . #t)
             (emacs-ledger-mode . emacs-ledger-mode)))
   (home-services-getter get-home-services)))
