;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 conses <contact@conses.eu>
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
;;;
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

(define-module (rde features irc)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (feature-irc-settings
            feature-emacs-erc
            irc-account
            irc-account?
            irc-account-id
            irc-account-nick
            irc-account-network
            irc-account-bouncer?))

(define-configuration/no-serialization irc-account
  (id
   (symbol #f)
   "A simple identifier for IRC accounts.")
  (network
   (string "irc.libera.chat")
   "The IRC network to connect to.")
  (bouncer?
   (boolean #f)
   "Whether the IRC account is connecting to a bouncer server.")
  (nick
   (string #f)
   "The nick the account is registered under in the IRC network."))

(define (list-of-irc-accounts? lst)
  (and (list? lst) (not (null? lst)) (every irc-account? lst)))

(define* (feature-irc-settings
          #:key
          (irc-accounts #f))
  "Configure IRC accounts."
  (ensure-pred list-of-irc-accounts? irc-accounts)

  (feature
   (name 'irc-settings)
   (values (append
            `((irc-settings . #t)
              (irc-accounts . ,irc-accounts))))))

(define* (feature-emacs-erc
          #:key
          (erc-server "irc.libera.chat")
          (erc-port 6697)
          (erc-nick #f)
          (erc-full-name #f)
          (erc-autojoin-channels-alist '())
          (erc-auto-query 'window-no-select)
          (erc-query-display 'window)
          (erc-join-buffer 'buffer)
          (erc-kill-buffers-on-quit? #t)
          (erc-align-nicknames? #t)
          (erc-log? #f)
          (erc-images? #f)
          (erc-header-line-format " %n on %t (%m,%l)")
          (erc-hide-list '("NICK" "JOIN" "PART" "QUIT" "MODE" "AWAY"))
          (erc-track-exclude-types '("324" "329" "JOIN" "MODE" "NICK" "PART" "QUIT"))
          (erc-key "i"))
  "Configure ERC, the extensible IRC client for Emacs.
ERC-AUTO-QUERY, ERC-QUERY-DISPLAY, and ERC-JOIN-BUFFER determine the
window behavior upon receiving a message, talking to someone, and joining
a buffer, respectively.  See the documentation of @command{erc-join-buffer} for
the possible configuration values.
ERC-HIDE-LIST is a list of message types to hide, and ERC-TRACK-EXCLUDE-TYPES
is a list of message types to ignore."
  (ensure-pred string? erc-server)
  (ensure-pred integer? erc-port)
  (ensure-pred maybe-string? erc-nick)
  (ensure-pred maybe-string? erc-full-name)
  (ensure-pred list? erc-autojoin-channels-alist)
  (ensure-pred symbol? erc-auto-query)
  (ensure-pred symbol? erc-query-display)
  (ensure-pred symbol? erc-join-buffer)
  (ensure-pred boolean? erc-kill-buffers-on-quit?)
  (ensure-pred boolean? erc-align-nicknames?)
  (ensure-pred boolean? erc-log?)
  (ensure-pred boolean? erc-images?)
  (ensure-pred maybe-string? erc-header-line-format)
  (ensure-pred list? erc-hide-list)
  (ensure-pred list? erc-track-exclude-types)
  (ensure-pred string? erc-key)

  (define emacs-f-name 'erc)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to ERC."
    (require-value 'irc-accounts config)

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'erc))
        (defgroup rde-erc nil
          "Extra customizations for ERC."
          :group 'rde)
        (cl-defstruct rde-erc-user id network nick bouncer-p)
        (defcustom rde-erc-users '()
          "A list of `rde-erc-user' structs that hold IRC accounts."
          :type '(repeat rde-erc-user)
          :group 'rde-erc)

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((autoload 'erc-buffer-list "erc")
                (defvar rde-erc-buffer-source
                  `(:name "ERC"
                    :narrow ?i
                    :category buffer
                    :preview-key ,(kbd "M-.")
                    :state ,'consult--buffer-state
                    :items ,(lambda () (mapcar 'buffer-name (erc-buffer-list))))
                  "Source for ERC buffers to be set in
 `consult-buffer-sources'.")
                (with-eval-after-load 'consult
                  (add-to-list 'consult-buffer-sources rde-erc-buffer-source))
                (with-eval-after-load 'rde-completion
                    (add-to-list 'rde-completion-initial-narrow-alist
                                 '(erc-mode . ?i))))
              '())

        (defun rde-erc-connect (user)
          "Connect USER to irc network via tls."
          (interactive
           (list (cl-find (intern (completing-read
                                   "User: "
                                   (lambda (string pred action)
                                     (if (eq action 'metadata)
                                         `(metadata
                                           ,(cons 'display-sort-function 'identity))
                                         (complete-with-action
                                          action
                                          (mapcar 'rde-erc-user-id rde-erc-users)
                                          string pred)))))
                          rde-erc-users :key 'rde-erc-user-id)))
          (require 'erc)
          (let ((network (rde-erc-user-network user))
                (nick (rde-erc-user-nick user))
                (original-erc-email-userid erc-email-userid))
            (when (rde-erc-user-bouncer-p user)
              (let* ((irc-network (completing-read
                                   "Network: "
                                   (lambda (string pred action)
                                     (if (eq action 'metadata)
                                         `(metadata
                                           ,(cons 'display-sort-function 'identity))
                                       (complete-with-action
                                        action
                                        (mapcar 'rde-erc-user-network
                                                (cl-remove network rde-erc-users
                                                           :key 'rde-erc-user-network
                                                           :test 'string=))
                                        string pred)))))
                     (irc-network-nick (rde-erc-user-nick
                                        (cl-find irc-network rde-erc-users
                                                 :key 'rde-erc-user-network
                                                 :test 'string=))))
                (setq erc-email-userid (format "%s/%s" irc-network-nick irc-network))))
            (erc-tls
             :server network
             :port 6697
             :nick nick
             :password (auth-source-pick-first-password
                        :host network
                        :user nick))
            ;; Restore original value of erc-email-userid
            (setq erc-email-userid original-erc-email-userid)))

        (defun rde-erc-close-buffers ()
          "Close all erc buffers upon closing the erc server process."
          (interactive)
          (mapc 'kill-buffer (erc-buffer-list nil erc-server-process)))

        (defun rde-erc-toggle-timestamps ()
          "Refresh and toggle the timestamps in the current erc buffer."
          (interactive)
          (erc-toggle-timestamps)
          (force-window-update (selected-window)))

        (defun rde-erc-window-reuse-condition (buf-name action)
          "Set up a condition for erc buffers to be reused."
          (with-current-buffer buf-name
            (when (eq major-mode 'erc-mode)
              (not action))))

        (defun rde-erc-status-sidebar-toggle ()
          "Toggle the status sidebar by killing its buffer when closed."
          (interactive)
          (if (get-buffer-window erc-status-sidebar-buffer-name nil)
              (progn
                (erc-status-sidebar-close)
                (kill-buffer erc-status-sidebar-buffer-name))
            (erc-status-sidebar-open)))

        (defun rde-erc-status-add-padding (fun channame &optional num-messages erc-face)
          "Add left padding on the sidebar formatted channels list."
          (concat " " (funcall fun channame num-messages erc-face)))

        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,erc-key) 'rde-erc-connect))
        (setq rde-erc-users
              (list
               ,@(map
                  (lambda (irc-acc)
                    `(make-rde-erc-user
                      :id ',(irc-account-id irc-acc)
                      :network ,(irc-account-network irc-acc)
                      :nick ,(irc-account-nick irc-acc)
                      :bouncer-p ,(if (irc-account-bouncer? irc-acc) 't 'nil)))
                  (get-value 'irc-accounts config))))

        (add-to-list 'display-buffer-alist
                     (cons 'rde-erc-window-reuse-condition
                           '(display-buffer-reuse-mode-window
                             (inhibit-same-window . t)
                             (inhibit-switch-frame . t)
                             (mode . erc-mode))))
        (with-eval-after-load 'erc-status-sidebar
          (advice-add 'erc-status-sidebar-default-chan-format
                      :around 'rde-erc-status-add-padding)
          (when erc-status-sidebar-mode-line-format
            (setq erc-status-sidebar-header-line-format
                  (concat " " erc-status-sidebar-mode-line-format)))
          (setq erc-status-sidebar-width 22)
          (setq erc-status-sidebar-mode-line-format nil))

        (with-eval-after-load 'erc
          (require 'erc-status-sidebar)
          (require 'xdg)

          (let ((map erc-mode-map))
            (define-key map (kbd "C-c C-q") 'rde-erc-close-buffers)
            (define-key map (kbd "C-c C-t") 'rde-erc-toggle-timestamps)
            (define-key map (kbd "C-c C-s") 'rde-erc-status-sidebar-toggle))
          (setq erc-default-server ,erc-server)
          (setq erc-default-port ,erc-port)
          ,@(if erc-nick
                `((setq erc-nick ,erc-nick))
                '())
          ,@(if erc-full-name
                `((setq erc-user-full-name ,erc-full-name))
                '())
          (setq erc-hide-list ',erc-hide-list)
          (setq erc-hide-prompt t)
          (setq erc-hide-timestamps t)
          (setq erc-echo-timestamps nil)
          ,@(if erc-kill-buffers-on-quit?
                '((setq erc-kill-buffer-on-part t)
                  (setq erc-kill-server-buffer-on-quit t)
                  (setq erc-kill-queries-on-quit t))
                '())
          (setq erc-rename-buffers t)
          ,@(if erc-header-line-format
                `((setq erc-header-line-format ,erc-header-line-format))
                '((setq erc-header-line-format nil)))
          (setq erc-auto-query ',erc-auto-query)
          (setq erc-query-display ',erc-query-display)
          (setq erc-join-buffer ',erc-join-buffer)
          (setq erc-timestamp-format "%H:%M")
          (setq erc-prompt-for-password nil)
          (add-to-list 'erc-modules 'keep-place)
          (add-to-list 'erc-modules 'notifications)
          (with-eval-after-load 'erc-track
            (setq erc-track-exclude-server-buffer t)
            (setq erc-track-enable-keybindings t)
            (setq erc-track-shorten-start 8)
            (setq erc-track-exclude-types ',erc-track-exclude-types))
          (with-eval-after-load 'erc-join
            (setq erc-autojoin-timing 'connect)
            (setq erc-autojoin-delay 5)
            (setq erc-autojoin-channels-alist ',erc-autojoin-channels-alist)
            (setq erc-autojoin-domain-only t))
          (with-eval-after-load 'erc-backends
            (setq erc-server-reconnect-timeout 3)
            (setq erc-server-reconnect-attempts t))
          (add-to-list 'erc-modules 'services)
          (with-eval-after-load 'erc-services
            (setq erc-prompt-for-nickserv-password nil))
          ,@(if (get-value 'emacs-spelling config)
                '((add-to-list 'erc-modules 'spelling))
                '())
          ,@(if erc-log?
              '((add-to-list 'erc-modules 'log)
                (with-eval-after-load 'erc-log
                  (setq erc-log-insert-log-on-open t)
                  (setq erc-log-channels-directory
                        (expand-file-name "emacs/erc-logs"
                                          (or (xdg-cache-home) "~/.cache")))))
              '())
        ,@(if erc-align-nicknames?
              '((with-eval-after-load 'erc-fill
                  (setq erc-fill-function 'erc-fill-static)
                  (setq erc-fill-static-center 14)
                  (setq erc-fill-column 82)))
              '())
        ,@(if erc-images?
              '((add-to-list 'erc-modules 'image)
                (with-eval-after-load 'erc-image
                  (setq erc-image-inline-rescale 100)))
              '())))
      #:elisp-packages (append
                        (if erc-images?
                            (list emacs-erc-image)
                            '())
                        (list emacs-erc-hl-nicks))
      #:summary "Reasonable defaults and extensions for ERC"
      #:commentary "Provide a distraction-free IRC experience with highlighting of nicks,
image support, content alignment, and more."
      #:authors '("conses <contact@conses.eu>"))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
