;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (rde features libreoffice)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:export (feature-libreoffice))

;; The issue with Libreoffice is that like Chromium, all user data is
;; managed in a single directory in ~/.config by default. The content of this
;; single directory better matches XDG_STATE_HOME for the most part, but like
;; most non-XDG compliant big apps, we put it in XDG_DATA_HOME until they are.

;; Modifing the place of this directory is however more tedious, since the
;; startup flag -env:UserInstallation=file:///tmp/test alone doesn't work
;; in Guix.

;; As a workaround, we modify the `bootstraprc' file in Libreoffice's output.
;; But there doesn't seem to be an option for indicating bootstraprc location,
;; so we make a complete copy with the desired changes. This also enables us
;; to easily change the flags in the desktop files.

;; TODO Patch libreoffice upstream so that it reads /etc/libreoffice/bootstraprc
;; instead (will enable to go from copy to wrap in RDE).

(define (rde-libreoffice pkg data-home-suffix home flags)
  (package
    (inherit pkg)
    (name "libreoffice")
    (source (origin (inherit (package-source pkg))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((lo #$(this-package-native-input "libreoffice")))
            ;; Copy everything
            (copy-recursively lo #$output)
            ;; Symlinks are copied too... so they need to be remade.
            (delete-file (string-append #$output "/bin/libreoffice"))
            (delete-file (string-append #$output "/bin/soffice"))
            (symlink (string-append
                      #$output "/lib/libreoffice/program/soffice")
                     (string-append #$output "/bin/libreoffice"))
            (symlink (string-append
                      #$output "/lib/libreoffice/program/soffice")
                     (string-append #$output "/bin/soffice"))
            ;; Make the default user installation XDG_DATA_HOME
            (substitute* (string-append
                          #$output "/lib/libreoffice/program/bootstraprc")
              (("UserInstallation=.*")
               (string-append "UserInstallation=$SYSUSERHOME"
                              #$data-home-suffix "/libreoffice")))
            (substitute* (string-append
                          #$output "/lib/libreoffice/program/soffice")
              (("\"\\$\\@\"[[:space:]]$")
               (string-append "\"-env:UserInstallation=file://"
                              #$home #$data-home-suffix
                              "/libreoffice\" \"$@\"\n")))
            ;; Add other flags to applications desktop files.
            (substitute*
                (find-files (string-append #$output "/share/applications"))
              (("^Exec=([^ ]*) (.*)" all bin rest)
               (string-join
                (list "Exec=" (string-append #$output "/bin/libreoffice")
                      #$@flags rest))))))))
    ;; In native-inputs so that it can be collected if space is needed.
    (native-inputs (list pkg))
    (inputs '())
    (propagated-inputs '())))

(define* (feature-libreoffice
          #:key
          (libreoffice libreoffice)
          (startup-flags '("--nologo")))
  "Configure LibreOffice."
  (ensure-pred file-like? libreoffice)
  (ensure-pred list-of-strings? startup-flags)

  (define f-name 'libreoffice)

  (define (get-home-services config)
    "Return home services related to LibreOffice."
    (let* ((data-home
            (home-xdg-base-directories-configuration-data-home
             (get-value 'xdg-base-directories-configuration config))))
      (list
       (simple-service
        'add-libreoffice-package
        home-profile-service-type
        (list
         (rde-libreoffice
          libreoffice
          (string-drop data-home (string-length "$HOME"))
          (get-value 'home-directory config)
          startup-flags))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
