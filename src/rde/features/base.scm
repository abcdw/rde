;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
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
  #:use-module (gnu system setuid)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sound)
  #:use-module (gnu services xorg)
  #:use-module (gnu services admin)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services networking)
  #:use-module (gnu services avahi)
  #:use-module (gnu services dbus)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)

  #:use-module (gnu packages certs)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages freedesktop)
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
            feature-foreign-distro

            %rde-default-substitute-urls
            %rde-default-authorized-guix-keys))

(define* (feature-user-info
          #:key user-name full-name email
          (home-directory (format #f "/home/~a" user-name))
          (user-initial-password-hash #f)
          (user-groups '("wheel" "netdev" "audio" "video" "dialout"))
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
      'base-packages-for-home-profile
      home-profile-service-type
      (append home-packages
              base-home-packages))))

  (define (get-system-packages values)
    (list
     (simple-service
      'base-packages-for-system-profile
      profile-service-type
      (append system-packages
              base-system-packages))))

  (feature
   (name 'base-packages)
   (home-services-getter get-home-packages)
   (system-services-getter get-system-packages)))

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

(define %rde-base-system-services
  (list
   (service greetd-service-type)
   (service virtual-terminal-service-type)
   (service console-font-service-type '())
   (service syslog-service-type (syslog-configuration))
   (service static-networking-service-type
            (list %loopback-static-networking))
   (service urandom-seed-service-type)
   (service guix-service-type)
   (service nscd-service-type)

   (service rottlog-service-type)
   (service log-cleanup-service-type
            (log-cleanup-configuration
             (directory "/var/log/guix/drvs")))
   (service udev-service-type
            (udev-configuration
             (rules (list lvm2 fuse alsa-utils crda))))

   (service sysctl-service-type)

   (service special-files-service-type
            `(("/bin/sh" ,(file-append bash "/bin/sh"))
              ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))))

(define %rde-default-substitute-urls %default-substitute-urls)
(define %rde-default-authorized-guix-keys %default-authorized-guix-keys)

(define* (feature-base-services
          #:key
          (default-substitute-urls %rde-default-substitute-urls)
          (default-authorized-guix-keys %rde-default-authorized-guix-keys)
          (guix-substitute-urls '())
          (guix-authorized-keys '())
          (udev-rules '())
          (base-system-services %rde-base-system-services))
  "Provides base system services."
  (ensure-pred list-of-services? base-system-services)
  (ensure-pred list-of-strings? guix-substitute-urls)
  (ensure-pred list-of-file-likes? guix-authorized-keys)
  (ensure-pred list-of-file-likes? udev-rules)

  (define (get-base-system-services cfg)
    (modify-services base-system-services
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
      (greetd-service-type
       config =>
       (greetd-configuration
        (terminals
         (map (lambda (x)
                (greetd-terminal-configuration
                 (terminal-vt (number->string x))))
              (iota 6 1)))))
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

(define %rde-desktop-system-services
  (list
   ;; Add udev rules for MTP devices so that non-root users can access
   ;; them.
   (simple-service 'mtp udev-service-type (list libmtp))
   ;; Add udev rules for scanners.
   (service sane-service-type)
   ;; Add polkti rules, so that non-root users in the wheel group can
   ;; perform administrative tasks (similar to "sudo").
   polkit-wheel-service

   ;; Allow desktop users to also mount NTFS and NFS file systems
   ;; without root.
   (simple-service 'mount-setuid-helpers setuid-program-service-type
                   (map (lambda (program)
                          (setuid-program
                           (program program)))
                        (list (file-append nfs-utils "/sbin/mount.nfs")
                              (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))

   ;; The global fontconfig cache directory can sometimes contain
   ;; stale entries, possibly referencing fonts that have been GC'd,
   ;; so mount it read-only.
   (simple-service 'fontconfig-file-system
                   file-system-service-type
                   (list %fontconfig-file-system))


   ;; The D-Bus clique.
   (service avahi-service-type)
   (service udisks-service-type
           (udisks-configuration (udisks udisks)))
   (service upower-service-type)
   (service accountsservice-service-type)
   (service cups-pk-helper-service-type)
   (service colord-service-type)
   (geoclue-service)
   ;; (service polkit-service-type)
   (service elogind-service-type)
   (service dbus-root-service-type)

   (service ntp-service-type)

   x11-socket-directory-service))

(define %rde-desktop-home-services
  (list (service home-dbus-service-type)))

(define* (feature-desktop-services
          #:key
          (default-desktop-system-services %rde-desktop-system-services)
          (default-desktop-home-services %rde-desktop-home-services)
          (dbus dbus))
  "Provides desktop system services."
  (define (get-home-services _)
    default-desktop-home-services)

  (define (get-system-services _)
    default-desktop-system-services)

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

(define* (feature-foreign-distro
          #:key
          (glibc-locales glibc-locales))
  "Provides missing packages and other fixes for rde usage on foreign distro."
  (ensure-pred file-like? glibc-locales)

  (define (get-home-services _)
    (list
     ;; On ubuntu 20.04 default Guix Home environment fails with
     ;; guile: warning: failed to install locale
     (simple-service
      'foreign-distro-glibc-locales
      home-profile-service-type
      (list glibc-locales))

     ;; The fix for ubuntu, as it doesn't set XCURSOR_PATH, but expects it
     ;; contains a /usr/share/icons if it set.
     (simple-service
      'xcursors-environment-variables-ubuntu-fix
      home-environment-variables-service-type
      `(("XCURSOR_PATH" .
         "/usr/share/icons${XCURSOR_PATH:+:}$XCURSOR_PATH")))))

  (feature
   (name 'foreign-distro)
   (home-services-getter get-home-services)
   (values `((foreign-distro . #t)))))
