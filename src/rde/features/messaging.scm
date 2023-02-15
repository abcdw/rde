;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 conses <contact@conses.eu>
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

(define-module (rde features messaging)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (srfi srfi-1)
  #:export (slack-account
            slack-account?
            slack-account-workspace
            slack-account-nick
            slack-account-cookie?
            feature-slack-settings
            feature-emacs-slack))


;;;
;;; Records.
;;;

(define-configuration/no-serialization slack-account
  (workspace
   (string #f)
   "The Slack workspace to authenticate with.  It should take the subdomain
of the complete host, as in @code{clojurians} for \"clojurians.slack.com\".")
  (nick
   (string #f)
   "The Slack nick registered under the associated workspace.")
  (cookie?
   (boolean #f)
   "Whether to use a browser cookie for authentication.  As per
@uref{https://github.com/yuya373/emacs-slack#how-to-get-token-and-cookie},
you only need to set this to #t if your Slack token begins with \"xoxc-\"."))

(define (list-of-slack-accounts? lst)
  (and (list? lst) (not (null? lst)) (every slack-account? lst)))


;;;
;;; feature-slack-settings.
;;;

(define* (feature-slack-settings
          #:key
          (slack-accounts #f))
  (ensure-pred list-of-slack-accounts? slack-accounts)

  (feature
   (name 'slack-settings)
   (values (append
            `((slack-settings . #t)
              (slack-accounts . ,slack-accounts))))))


;;;
;;; feature-emacs-slack.
;;;

(define* (feature-emacs-slack
          #:key
          (emacs-slack emacs-slack)
          (slack-key "s"))
  "Configure the Slack.el Emacs client.
For authentication to work, ensure to set your credentials (@pxref{Slack} in
the RDE manual) correctly."
  (ensure-pred file-like? emacs-slack)
  (ensure-pred string? slack-key)

  (define emacs-f-name 'slack)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the Slack.el client."
    (require-value 'slack-accounts config)

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'cl-macs))
        (defgroup rde-slack nil
          "Utilities for slack.el, the Emacs Slack client."
          :group 'rde)
        (cl-defstruct rde-slack-team workspace nick cookie-p)
        (defcustom rde-slack-teams '()
          "List of `rde-slack-team' structs that hold Slack accounts."
          :type '(repeat rde-slack-team)
          :group 'rde-slack)
        (defvar rde-slack-map nil
          "Map to bind `slack' commands under.")
        (define-prefix-command 'rde-slack-map)

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar rde-slack-buffer-source
                  `(:name "Slack"
                    :narrow ?s
                    :category buffer
                    :state ,'consult--buffer-state
                    :items ,(lambda ()
                              (mapcar 'buffer-name
                                      (rde-completion--mode-buffers
                                       'slack-message-buffer-mode
                                       'slack-thread-message-buffer-mode))))
                  "Source for Slack buffers to be set in
`consult-buffer-sources'.")
                (with-eval-after-load 'consult
                  (add-to-list 'consult-buffer-sources
                               rde-slack-buffer-source))
                (with-eval-after-load 'rde-completion
                  (add-to-list 'rde-completion-initial-narrow-alist
                               '(slack-message-buffer-mode . ?s))
                  (add-to-list 'rde-completion-initial-narrow-alist
                               '(slack-thread-message-buffer-mode . ?s))))
              '())

        (defun rde-slack-connect (team)
          "Connect to Slack TEAM with personal credentials."
          (interactive
           (list (cl-find (completing-read
                           "Team: "
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 `(metadata
                                   ,(cons 'display-sort-function 'identity))
                                 (complete-with-action
                                  action
                                  (mapcar
                                   'rde-slack-team-workspace rde-slack-teams)
                                  string pred))))
                          rde-slack-teams :key 'rde-slack-team-workspace
                          :test 'string=)))
          (let ((workspace (rde-slack-team-workspace team)))
            (slack-register-team
             :name workspace
             :token (auth-source-pick-first-password
                     :host workspace
                     :user (rde-slack-team-nick team))
             :cookie (when (rde-slack-team-cookie-p team)
                       (auth-source-pick-first-password
                        :host workspace
                        :user (concat (rde-slack-team-nick team) "^cookie"))))
            (slack-change-current-team)))

        (setq rde-slack-teams
              (list
               ,@(map
                  (lambda (slack-acc)
                    `(make-rde-slack-team
                      :workspace ,(slack-account-workspace slack-acc)
                      :nick ,(slack-account-nick slack-acc)
                      :cookie-p ,(if (slack-account-cookie? slack-acc) 't 'nil)))
                  (get-value 'slack-accounts config))))
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,slack-key) 'rde-slack-map)
          (define-key rde-slack-map "c" 'rde-slack-connect))
        (with-eval-after-load 'slack
          (setq slack-buffer-emojify t)
          (setq slack-prefer-current-team t)
          (setq slack-buffer-function 'switch-to-buffer)
          (let ((map rde-slack-map))
            (define-key map "s" 'slack-channel-select)
            (define-key map "t" 'slack-change-current-team))
          (set-face-attribute 'slack-preview-face nil
                              :background 'unspecified)))
      #:elisp-packages (list emacs-slack))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-slack)))
   (home-services-getter get-home-services)))
