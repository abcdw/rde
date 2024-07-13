;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde features bittorrent)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services bittorrent)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-transmission))


(define* (feature-transmission
          #:key
          (transmission transmission)
          (emacs-transmission emacs-transmission)
          (auto-start? #t)
          (download-dir %unset-value)
          (extra-transmission-settings '()))
  "Setup and configure Transmission and transmission.el"
  (ensure-pred file-like? transmission)
  (ensure-pred file-like? emacs-transmission)
  (ensure-pred boolean? auto-start?)
  (ensure-pred list? extra-transmission-settings)

  (define (transmission-home-services config)
    (define emacs-f-name 'transmission)
    (define emacs-cmd (get-value 'emacs-client config #f))

    (list
     (when (get-value 'emacs config #f)
       ;; Without desktop notifications it will be unclear what is happening.
       ;; It could be a fallback to old approach (where frame is created), but
       ;; it would require future maintanance.
       (require-value 'desktop-notifications config)
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
(let* ((torrent \"" (cadr (command-line)) "\"))
 (require 'notifications)
 (condition-case nil
     (progn
       (transmission-add torrent)
       (notifications-notify
        :title \"Transmission\"
        :body (format \"Torrent probably added: %s\" torrent)
        :timeout 2000))
   (error
    (notifications-notify
     :title \"Transmission\"
     :body \"Something went wrong, check if transmission daemon is running.\")
    nil)))"))
        #:default-for '(x-scheme-handler/magnet application/x-bittorrent)))

     (service home-transmission-service-type
              (home-transmission-configuration
               (transmission transmission)
               (auto-start? auto-start?)
               (download-dir download-dir)
               (settings extra-transmission-settings)))))

  (feature
   (name 'transmission)
   (values `((transmission . #t)
             (emacs-transmission . #t)))
   (home-services-getter transmission-home-services)))
