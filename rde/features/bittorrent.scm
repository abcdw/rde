(define-module (rde features bittorrent)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-transmission))


(define* (feature-transmission
          #:key
          (package transmission)
          (auto-start? #t))
  "Setup and configure Transmission and transmission.el"

  (define (transmission-home-services config)
    (define emacs-f-name 'transmission)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))

    (list
     (elisp-configuration-service
      emacs-f-name
      `((require 'configure-rde-keymaps)
        (define-key rde-apps (kbd "T") 'transmission))
      #:elisp-packages (list emacs-transmission
                             (get-value 'emacs-configure-rde-keymaps config)))

     (emacs-xdg-service
      emacs-f-name "Emacs (Client) [magnet:]"
      #~(system*
         #$emacs-cmd "--eval"
	 (string-append "\
(progn
 (set-frame-name \"Transmission - Emacs Client\")
 (transmission)
 (delete-other-windows)
 (transmission-add \"" (cadr (command-line)) "\")
 (revert-buffer))"))
      #:default-for '(x-scheme-handler/magnet))

     (simple-service
      'transmission-add-shepherd-daemon
      home-shepherd-service-type
      (list
       ;; TODO: Make home-transmission service for Guix Home
       (shepherd-service
        (provision '(transmission))
        (auto-start? auto-start?)
        (start #~(make-forkexec-constructor
                  (list #$(file-append package "/bin/transmission-daemon")
                        "--foreground")))
        (stop  #~(make-kill-destructor)))))))

  (feature
   (name 'transmission)
   (values `((transmission . #t)
             (emacs-transmission . #t)))
   (home-services-getter transmission-home-services)))
