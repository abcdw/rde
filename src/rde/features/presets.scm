;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features presets)
  #:use-module (rde features base)
  #:use-module (rde features bittorrent)
  #:use-module (rde features documentation)
  #:use-module (rde features emacs)
  #:use-module (rde features guile)
  #:use-module (rde features gtk)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features finance)
  #:use-module (rde features fontutils)
  #:use-module (rde features image-viewers)
  #:use-module (rde features irc)
  #:use-module (rde features linux)
  #:use-module (rde features mail)
  #:use-module (rde features markup)
  #:use-module (rde features networking)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features version-control)
  #:use-module (rde features video)
  #:use-module (rde features virtualization)
  #:use-module (rde features web-browsers)
  #:use-module (rde features wm))

;;;
;;; Various lists of features with predefined values.
;;;

(define-public rde-base
  (list
   ;; TODO: merge them into feature-base (maybe using super-feature)
   (feature-base-services)
   (feature-base-packages)
   (feature-desktop-services)

   (feature-pipewire)
   (feature-backlight #:step 10)
   (feature-networking)))

(define-public rde-desktop
  (list
   (feature-fonts)
   (feature-gtk3)

   ;; https://sr.ht/~tsdh/swayr/
   ;; https://github.com/ErikReider/SwayNotificationCenter
   ;; https://github.com/swaywm/sway/wiki/i3-Migration-Guide

   ;; https://github.com/natpen/awesome-wayland
   (feature-sway)
   (feature-emacs-power-menu)
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   ;; (feature-sway-statusbar
   ;;  #:use-global-fonts? #f)
   (feature-swaynotificationcenter)
   (feature-waybar)
   (feature-swayidle)
   (feature-swaylock)
   (feature-batsignal)
   (feature-imv)
   (feature-mpv)
   (feature-librewolf)
   (feature-ungoogled-chromium)
  (feature-transmission #:auto-start? #f)
  (feature-ledger)))

(define-public rde-mail
  (list
   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)))

(define-public rde-cli
  (list
   (feature-manpages)
   (feature-vterm)
   (feature-tmux)
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-bash)
   (feature-direnv)
   (feature-git)
   (feature-guile)
   (feature-ssh)))

(define-public rde-emacs
  (list
   (feature-emacs
    #:default-application-launcher? #t)

   (feature-emacs-appearance)
   (feature-emacs-modus-themes)
   ;; Adds 0.15s load time and blinking
   ;; (feature-emacs-dashboard)

   (feature-emacs-completion
    #:mini-frame? #f
    #:marginalia-align 'right)
   (feature-emacs-corfu
    #:corfu-doc-auto #f)
   (feature-emacs-vertico)

   (feature-emacs-tramp)
   (feature-emacs-project)
   (feature-compile)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)

   (feature-emacs-message)
   (feature-emacs-erc
    #:erc-log? #t
    #:erc-autojoin-channels-alist '((Libera.Chat "#rde")))
   (feature-emacs-telega)
   (feature-emacs-elpher)
   (feature-emacs-webpaste)

   (feature-emacs-pdf-tools)
   (feature-emacs-nov-el)
   (feature-emacs-org-protocol)

   (feature-emacs-smartparens
    #:show-smartparens? #t)
   (feature-emacs-eglot)))
