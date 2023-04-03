(define-module (rde features bittorrent)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services bittorrent)
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
          (download-dir #f)
          (extra-transmission-settings '()))
  "Setup and configure Transmission and transmission.el"

  (define (transmission-home-services config)
    (define emacs-f-name 'transmission)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))

    (list
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((with-eval-after-load 'rde-keymaps
            (define-key rde-app-map (kbd "T") 'transmission))
          (with-eval-after-load
           'transmission
           (let ((map transmission-mode-map))
             (define-key map "R" 'transmission-move))))
        #:summary "\
Transmission Emacs interface"
        #:commentary "\
Keybinding in `rde-app-map', R for `transmission-move', xdg entry for magnet
links and torrent files."
        #:keywords '(convenience)
        #:elisp-packages (list emacs-transmission)))

     (when emacs-cmd
       (emacs-xdg-service
        emacs-f-name "Emacs (Client) [BitTorrent]"
        #~(system*
           #$emacs-cmd "--eval"
	   (string-append "\
(progn
 (set-frame-name \"Transmission - Emacs Client\")
 (transmission)
 (delete-other-windows)
 (transmission-add \"" (cadr (command-line)) "\")
 (revert-buffer))"))
        #:default-for '(x-scheme-handler/magnet application/x-bittorrent)))

     (service home-transmission-service-type
              (home-transmission-configuration
               (transmission package)
               (download-dir download-dir)
               (settings extra-transmission-settings)))))

  (feature
   (name 'transmission)
   (values `((transmission . #t)
             (emacs-transmission . #t)))
   (home-services-getter transmission-home-services)))
