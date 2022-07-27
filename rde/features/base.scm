;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features base)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sound)
  #:use-module (gnu services xorg)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages glib)
  #:use-module (rde packages)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)

  #:export (feature-user-info
            feature-base-packages
            feature-custom-services
            feature-base-services
            feature-desktop-services
            feature-hidpi
            feature-generic

            %rde-default-substitute-urls
            %rde-default-authorized-guix-keys))

(define* (feature-user-info
          #:key user-name full-name email
          (home-directory (format #f "/home/~a" user-name))
          (user-initial-password-hash #f)
          (user-groups '())
          (rde-advanced-user? #f)
          (emacs-advanced-user? #f))
  "Provides basic information about user for all features."
  (ensure-pred string? user-name)
  (ensure-pred string? full-name)
  (ensure-pred string? email)
  (ensure-pred string? home-directory)
  (ensure-pred boolean? rde-advanced-user?)
  (ensure-pred boolean? emacs-advanced-user?)
  (ensure-pred maybe-string? user-initial-password-hash)

  (feature
   (name 'user-info)
   (values (make-feature-values
            rde-advanced-user? emacs-advanced-user?
            user-name full-name email home-directory
            user-groups
            user-initial-password-hash))))


;; TODO: Cleanup the list of base system packages, it contains some
;; unecessary for rde packages (some network, fs utils)
;; (display
;;  (map package-name %base-system-packages))
(define %rde-base-system-packages
  (append
   (list nss-certs)
   %base-packages-disk-utilities
   %base-packages))

(define* (feature-base-packages
          #:key
          (home-packages '())
          (system-packages '())
          (base-system-packages %rde-base-system-packages)
          (base-home-packages (list rde)))
  "Provides base packages and allows to specify additional standalone
packages for home-environment, or operating-system, or both.
Standalone means that packages do not require configuration and not
installed by system or home services."
  (ensure-pred list-of-packages? home-packages)
  (ensure-pred list-of-packages? system-packages)
  (ensure-pred list-of-packages? base-system-packages)

  (define (get-home-packages values)
    (list
     (simple-service
      'add-base-package-to-home-profile
      home-profile-service-type
      (append home-packages
              base-home-packages))))

  (define (get-system-packages values)
    (list
     (simple-service
      'add-base-packages-to-system-profile
      profile-service-type
      (append system-packages
              base-system-packages))))

  (feature
   (name 'base-packages)
   (home-services-getter get-home-packages)
   (system-services-getter get-system-packages)))


(define %rde-base-services
  %base-services)

(define %rde-desktop-services
  (remove (lambda (service)
            (member (service-kind service)
                    (append
                     (map service-kind %rde-base-services)
                     (list gdm-service-type screen-locker-service-type
                           pulseaudio-service-type alsa-service-type))))
          %desktop-services))

;; ((@ (ice-9 pretty-print) pretty-print)
;;  (map service-kind  %base-services))

(define* (feature-custom-services
          #:key
          (feature-name-prefix 'generic)
          (system-services '())
          (home-services '()))
  "Allows to specify additional System and Home Services.  PREFIX should
be a symbol, which will be used to construct feature name."
  (ensure-pred symbol? feature-name-prefix)
  (ensure-pred list-of-services? home-services)
  (ensure-pred list-of-services? system-services)

  (define feature-name (symbol-append feature-name-prefix '-custom-services))
  (define (get-custom-home-services cfg)
    home-services)

  (define (get-custom-system-services cfg)
    system-services)

  (feature
   (name feature-name)
   (values `((,feature-name . #t)))
   (home-services-getter get-custom-home-services)
   (system-services-getter get-custom-system-services)))

(define %rde-default-substitute-urls %default-substitute-urls)
(define %rde-default-authorized-guix-keys %default-authorized-guix-keys)

(define* (feature-base-services
          #:key
          (default-substitute-urls %rde-default-substitute-urls)
          (default-authorized-guix-keys %rde-default-authorized-guix-keys)
          (guix-substitute-urls '())
          (guix-authorized-keys '())
          (udev-rules '())
          (base-services %rde-base-services))
  "Provides base system services."
  (ensure-pred list-of-services? base-services)
  (ensure-pred list-of-strings? guix-substitute-urls)
  (ensure-pred list-of-file-likes? guix-authorized-keys)
  (ensure-pred list-of-file-likes? udev-rules)

  (define (get-base-system-services cfg)
    (modify-services base-services
      (console-font-service-type
       config =>
       (map (lambda (x)
              (cons
               (format #f "tty~a" x)
               (get-value 'console-font cfg "LatGrkCyr-8x16")))
            (iota (get-value 'number-of-ttys cfg 6) 1)))
      (guix-service-type
       config =>
       (guix-configuration
        (inherit config)
        (substitute-urls (append
                          guix-substitute-urls
                          default-substitute-urls))
        (authorized-keys (append
                          guix-authorized-keys
                          default-authorized-guix-keys))))
      (udev-service-type
       config =>
       (udev-configuration
        (inherit config)
        (rules (append
                udev-rules
                (udev-configuration-rules config)))))))

  (feature
   (name 'base-services)
   (values `((base-services . #t)
             (number-of-ttys . ,%number-of-ttys)))
   (system-services-getter get-base-system-services)))

(define* (feature-desktop-services
          #:key
          (dbus dbus))
  "Provides desktop system services."
  (define (get-home-services _)
    (list
     ;; TODO: Make home-dbus-service-type
     (simple-service 'dbus-set-some-env-vars
                     home-environment-variables-service-type
                     '(("DBUS_SESSION_BUS_ADDRESS"
                        . "unix:path=$XDG_RUNTIME_DIR/bus")))
     (simple-service
      'dbus-add-shepherd-daemon
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision '(dbus))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append dbus "/bin/dbus-daemon")
                        "--nofork"
                        "--session"
                        (string-append
                         "--address=" "unix:path="
                         (getenv "XDG_RUNTIME_DIR") "/bus"))
                  #:log-file (string-append
                                    (or (getenv "XDG_LOG_HOME")
                                        (format #f "~a/.local/var/log"
                                                (getenv "HOME")))
                                    "/dbus.log"))))))))
  (define (get-system-services _)
    %rde-desktop-services)

  (feature
   (name 'desktop-services)
   (values `((desktop-services . #t)
             (elogind . #t)
             (dbus . ,dbus)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))


(define* (feature-hidpi
          #:key
          (scaling-factor 2)
          (console-font (file-append
                         font-terminus
                         "/share/consolefonts/ter-132n")))
  "Provides values, which will affect other features, making them more
HiDPI friendly."
  (ensure-pred file-like-or-path? console-font)
  (ensure-pred integer? scaling-factor)

  (feature
   (name 'hidpi)
   (values (make-feature-values scaling-factor console-font))))
