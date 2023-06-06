;;; rde --- Reproducible development environment.
;;;
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

(define-module (rde features matrix)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages matrix)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde home services matrix)
  #:use-module (srfi srfi-1)
  #:export (matrix-account
            matrix-account?
            matrix-account-id
            matrix-account-server
            feature-matrix-settings
            feature-pantalaimon
            feature-emacs-ement))


;;;
;;; Records.
;;;

(define-configuration/no-serialization matrix-account
  (id
   (string #f)
   "The Matrix ID to use. It should take the form
of @code{\"@username:example.com\"}.")
  (server
   (string #f)
   "The Matrix server the account is registered under."))

(define (list-of-matrix-accounts? lst)
  (and (list? lst) (not (null? lst)) (every matrix-account? lst)))

(define (maybe-list-of-matrix-accounts? x)
  (or (list-of-matrix-accounts? x) (not x)))


;;;
;;; feature-matrix-settings.
;;;

(define* (feature-matrix-settings
          #:key
          (homeserver "https://matrix.org")
          (matrix-accounts #f))
  "Provide account settings for Matrix.  HOMESERVER refers to the address of
your own Matrix server."
  (ensure-pred string? homeserver)
  (ensure-pred maybe-list-of-matrix-accounts? matrix-accounts)

  (feature
   (name 'matrix-settings)
   (values (append
            `((matrix-settings . #t)
              (matrix-homeserver . ,homeserver))
            (make-feature-values matrix-accounts)))))


;;;
;;; feature-pantalaimon.
;;;

(define (get-pantalaimon-uri config)
  (string-append "http://localhost:"
                 (number->string (get-value 'pantalaimon-port config))))

(define* (feature-pantalaimon
          #:key
          (pantalaimon pantalaimon)
          (port 8009)
          (ssl? #t)
          (ignore-device-verification? #f)
          (extra-config '()))
  "Configure Pantalaimon, an E2EE-aware proxy daemon for Matrix clients.
See @url{/man/pantalaimon.5,,Pantalaimon} for the list of available options."
  (ensure-pred file-like? pantalaimon)
  (ensure-pred integer? port)
  (ensure-pred boolean? ssl?)
  (ensure-pred boolean? ignore-device-verification?)
  (ensure-pred list? extra-config)

  (define (get-home-services config)
    "Return home services related to Pantalaimon"
    (require-value 'matrix-settings config)

    (list
     (service
      home-pantalaimon-service-type
      (home-pantalaimon-configuration
       (pantalaimon pantalaimon)
       (config
        `((Default
            ((LogLevel . debug)))
          (local-matrix
           ((Homeserver . ,#~(format #f "~a"
                                     #$(get-value 'matrix-homeserver config)))
            (ListenAddress . localhost)
            (ListenPort . ,port)
            (Ssl . ,ssl?)
            (IgnoreVerification . ,ignore-device-verification?)
            (UseKeyring . #f)))
          ,@extra-config))))))

  (feature
   (name 'pantalaimon)
   (values `((pantalaimon . ,pantalaimon)
             (pantalaimon-port . ,port)
             (get-pantalaimon-uri . ,get-pantalaimon-uri)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-ement.
;;;

(define* (feature-emacs-ement
          #:key
          (emacs-ement emacs-ement)
          (ement-key "e"))
  "Configure Ement, the Matrix client for Emacs."
  (ensure-pred file-like? emacs-ement)
  (ensure-pred string? ement-key)

  (define emacs-f-name 'ement)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Ement."
    (require-value 'matrix-settings config)
    (define homeserver (get-value 'matrix-homeserver config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
          (require 'ement))
        (defgroup rde-ement nil
          "Utilities for Ement, the Emacs Matrix client."
          :group 'rde)
        (cl-defstruct rde-ement-user id homeserver)
        (defcustom rde-ement-users '()
          "List of `rde-ement-user' structs that hold Matrix accounts."
          :type '(repeat rde-ement-user)
          :group 'rde-ement)
        (defvar rde-ement-map nil
          "Map to bind `ement' commands under.")
        (define-prefix-command 'rde-ement-map)

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar rde-ement-buffer-source
                  `(:name "Ement"
                          :narrow ?e
                          :category buffer
                          :state ,'consult--buffer-state
                          :items ,(lambda ()
                                    (mapcar 'buffer-name
                                            (rde-completion--mode-buffers
                                             'ement-room-mode
                                             'ement-room-list-mode))))
                  "Source for Ement buffers to be set in
`consult-buffer-sources'.")
                (with-eval-after-load 'consult
                  (add-to-list 'consult-buffer-sources
                               rde-ement-buffer-source 'append))
                (with-eval-after-load 'rde-completion
                  (add-to-list 'rde-completion-initial-narrow-alist
                               '(ement-room-mode . ?e))
                  (add-to-list 'rde-completion-initial-narrow-alist
                               '(ement-room-list-mode . ?e))))
              '())

        (defun rde-ement-connect (user)
          "Connect to Matrix homeserver with USER."
          (interactive
           (list (cl-find (completing-read
                           "User: "
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 `(metadata
                                   ,(cons 'display-sort-function 'identity))
                               (complete-with-action
                                action
                                (mapcar 'rde-ement-user-id
                                        rde-ement-users)
                                string pred))))
                          rde-ement-users
                          :key 'rde-ement-user-id :test 'string=)))
          (let ((homeserver (rde-ement-user-homeserver user)))
            (ement-connect
             :user-id (rde-ement-user-id user)
             :password (auth-source-pick-first-password :host homeserver)
             :uri-prefix ,(if (get-value 'pantalaimon config)
                              ((get-value 'get-pantalaimon-uri config) config)
                              homeserver))))

        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,ement-key) 'rde-ement-map))
        (let ((map rde-ement-map))
          (define-key map (kbd "c") 'rde-ement-connect)
          (define-key map (kbd "l") 'ement-room-list))
        (setq rde-ement-users
              (list
               ,@(map
                  (lambda (matrix-acc)
                    `(make-rde-ement-user
                      :id ,(matrix-account-id matrix-acc)
                      :homeserver ,(matrix-account-server matrix-acc)))
                  (get-value 'matrix-accounts config))))
        (with-eval-after-load 'ement
          (add-hook 'ement-room-compose-hook 'ement-room-compose-org)
          (let ((map rde-ement-map))
            (define-key map (kbd "d") 'ement-disconnect)
            (define-key map (kbd "v") 'ement-room-view))
          (let ((map ement-room-mode-map))
            (define-key map "c" 'ement-room-compose-message)
            (define-key map "E" 'ement-room-edit-message))
          (setq ement-room-send-message-filter 'ement-room-send-org-filter)
          (setq ement-notify-notification-predicates
                '(ement-notify--event-mentions-session-user-p
                  ement-notify--event-mentions-room-p))
          (setq ement-save-sessions t)
          (setq ement-room-send-read-receipts nil)))
      #:elisp-packages (list emacs-ement))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-ement)))
   (home-services-getter get-home-services)))
