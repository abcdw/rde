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

(define-module (rde features web-browsers)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services web-browsers)
  #:use-module (rde packages web-browsers)
  #:use-module (rde serializers lisp)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages browser-extensions)
  #:use-module (gnu packages chromium)
  #:use-module (gnu services)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (feature-ungoogled-chromium
            feature-nyxt))

(define* (feature-ungoogled-chromium
          #:key
          (ungoogled-chromium ungoogled-chromium/wayland)
          (default-browser? #f)
          (startup-flags '()))
  "Configure the Chromium browser."
  (ensure-pred any-package? ungoogled-chromium)
  (ensure-pred boolean? default-browser?)
  (ensure-pred list? startup-flags)

  (define f-name 'ungoogled-chromium)

  (define (get-home-services config)
    "Return home services related to Ungoogled Chromium."
    (append
     (if default-browser?
         (list
          (simple-service
           'set-chromium-as-default-browser
           home-environment-variables-service-type
           `(("BROWSER" . ,(file-append ungoogled-chromium "/bin/chromium"))))
          (simple-service
           'chromium-xdg-defaults
           home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (default
             '((x-scheme-handler/http . chromium.desktop)
               (x-scheme-handler/https . chromium.desktop)
               (x-scheme-handler/about . chromium.desktop)
               (text/html . chromium.desktop))))))
         '())
     (if (get-value 'emacs config)
         (list
          (rde-elisp-configuration-service
           f-name
           config
           `((with-eval-after-load 'browse-url
               (setq browse-url-chromium-arguments ',startup-flags))
             ,@(if (get-value 'emacs-embark config)
                   `((with-eval-after-load 'embark
                       (define-key embark-url-map "c" 'browse-url-chromium)))
                   '()))))
         '())
     (list
      (simple-service
       'add-chromium-packages
       home-profile-service-type
       (list
        ungoogled-chromium
        ublock-origin/chromium))
      (simple-service
       'add-chromium-xdg-desktop-entry
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration
        (desktop-entries
         (list
          (xdg-desktop-entry
           (file "chromium")
           (name "Chromium")
           (type 'application)
           (config
            `((exec . ,#~(string-join
                          (list
                           #$(file-append ungoogled-chromium "/bin/chromium")
                           #$@startup-flags "%U")))
              (terminal . #f)
              (comment . "Access the Internet")))))))))))

  (feature
   (name f-name)
   (values `((,f-name . ,ungoogled-chromium)))
   (home-services-getter get-home-services)))


;;;
;;; nyxt.
;;;

(define* (feature-nyxt
          #:key
          (nyxt nyxt-next)
          (default-browser? #f)
          (startup-flags '())
          (default-cookie-policy ':no-third-party)
          (extra-config-lisp '())
          (auto-rules '())
          (extra-bindings '())
          (default-new-buffer-url #f)
          (autostart-slynk? #f)
          (scroll-distance 50)
          (download-engine ':renderer)
          (temporary-history? #f)
          (restore-session? #t))
  "Set up Nyxt, the hacker's power browser.
DEFAULT-COOKIE-POLICY is either `:always' (accept all cookies),
`:never' (reject all cookies), or `:no-third-party (only accept
the current site's cookies)'.
DEFAULT-NEW-BUFFER-URL is the default new page URL you'll be prompted with
at browser startup if RESTORE-SESSION? is #f, otherwise you'll be shown the
last-accessed page.
You can control Nyxt remotely via a Lisp REPL if you set AUTOSTART-SLYNK? to #t
and you connect to the underlying Lisp image at port `*slynk-port*'
(by default 4006).
If you set TEMPORARY-HISTORY? to #t, your history will be recorded in
`nyxt-temporary-directory' (by default /tmp).
Use EXTRA-CONFIG-LISP for additional general settings, and consult Nyxt's
manual page, accessible via the command `manual' (C-h r), to discover more
functionalities."
  (ensure-pred file-like? nyxt)
  (ensure-pred boolean? default-browser?)
  (ensure-pred list? startup-flags)
  (ensure-pred symbol? default-cookie-policy)
  (ensure-pred lisp-config? extra-config-lisp)
  (ensure-pred lisp-config? auto-rules)
  (ensure-pred list? extra-bindings)
  (ensure-pred maybe-string? default-new-buffer-url)
  (ensure-pred boolean? autostart-slynk?)
  (ensure-pred integer? scroll-distance)
  (ensure-pred boolean? temporary-history?)
  (ensure-pred boolean? restore-session?)

  (define f-name 'nyxt)
  (define st-name 'nyxt-rde-base)

  (define nyxt-rde-base-service-type
    (make-nyxt-service-type st-name))

  (define (get-home-services config)
    "Return home services related to Nyxt."
    (require-value 'keyboard-layout config)
    (define keyboard-variant
      (keyboard-layout-variant (get-value 'keyboard-layout config)))

    (append
     (if default-browser?
         (list
          (simple-service
           'set-nyxt-as-default-browser
           home-environment-variables-service-type
           `(("BROWSER" . ,(file-append nyxt "/bin/nyxt"))))
          (simple-service
           'add-nyxt-xdg-mime-types
           home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (default
             '((x-scheme-handler/http . nyxt.desktop)
               (x-scheme-handler/https . nyxt.desktop)
               (x-scheme-handler/about . nyxt.desktop))))))
         '())
     (list
      (service
       home-nyxt-service-type
       (home-nyxt-configuration
        (nyxt nyxt)
        (auto-rules-lisp auto-rules)))
      (simple-service
       'add-nyxt-extra-config
       home-nyxt-service-type
       (home-nyxt-extension
        (config-lisp extra-config-lisp)))
      (service
       nyxt-rde-base-service-type
       (home-nyxt-lisp-configuration
        (name st-name)
        (config
         `((let ((sbcl-init (merge-pathnames ".sbclrc"
                                             (user-homedir-pathname))))
             (when (probe-file sbcl-init)
               (load sbcl-init)))
           (asdf:ensure-source-registry)
           (use-nyxt-package-nicknames)
           (defvar *rde-keymap* (make-keymap "rde-map"))
           ,@(if (nil? extra-bindings)
                 '()
                 `((define-key *rde-keymap* ,@extra-bindings)))
           ,@(if temporary-history?
                 '((defmethod files:resolve ((profile nyxt-profile)
                                             (file nyxt:history-file))
                     "Store history in a temporary directory."
                     (sera:path-join
                      (nfiles:expand
                       (make-instance 'nyxt-temporary-directory))
                      (uiop:relativize-pathname-directory
                       (call-next-method)))))
                 '())
           (define-mode rde-keymap-mode ()
             "Dummy mode to apply key bindings in `*rde-keymap*.'"
             ((keyscheme-map (keymaps:make-keyscheme-map
                              keyscheme:emacs *rde-keymap*))
              (visible-in-status-p nil)))
           (define-configuration document-buffer
             ((smooth-scrolling t)
              (scroll-distance ,scroll-distance)))
           (define-configuration web-buffer
             ((default-modes (append '(rde-keymap-mode) %slot-value%))
              (download-engine ',download-engine)))
           (define-configuration browser
             ((default-cookie-policy ,default-cookie-policy)
              (restore-session-on-startup-p ,(if restore-session? 't 'nil))
              ,@(if default-new-buffer-url
                    `((default-new-buffer-url
                       (quri:uri ,default-new-buffer-url)))
                    '())))
           (define-configuration nyxt/hint-mode:hint-mode
             ((nyxt/hint-mode:hints-alphabet ,(match keyboard-variant
                                                ("dvorak" "aoeuidhtns")
                                                (_ "asdfghjklqwertyuiop")))))
           ,@(if autostart-slynk?
                 '((unless nyxt::*run-from-repl-p*
                     (start-slynk)))
                 '()))))))))

  (feature
   (name f-name)
   (values `((,f-name . ,nyxt)
             (nyxt-rde-base-service-type . ,nyxt-rde-base-service-type)
             (nyxt-startup-flags . ,startup-flags)))
   (home-services-getter get-home-services)))
