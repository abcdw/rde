;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xdisorg)

  #:use-module (srfi srfi-1)

  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (strings->packages
            strings->inferior-packages
            %rde-patch-path))

;; Utils

(define* (strings->packages #:rest lst)
  (map specification->package+output lst))

(define* (strings->inferior-packages
          #:key (commit "2b6af630d61dd5b16424be55088de2b079e9fbaf")
          #:rest lst)
  "Packages from specific guix channel version."
  (define channel-guix
    `((channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (commit ,commit))))

  (define inferior (inferior-for-channels channel-guix))
  (define (get-inferior-pkg pkg-name)
    (car (lookup-inferior-packages inferior pkg-name)))

  (map get-inferior-pkg lst))

(define %channel-root
  (find (lambda (path)
          (file-exists? (string-append path "/rde/packages.scm")))
        %load-path))

(define %rde-patch-path
  (list (string-append %channel-root "/rde/packages/patches")))

(define-public rde
  (package
    (name "rde")
    (version "0.6.0")
    (home-page "https://trop.in/rde")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference (url "https://git.sr.ht/~abcdw/rde")
                          (commit "580acbca3e8b6165cdbbb2543b9ce5516b79c5d2")))
      (sha256
       (base32
        "0rcyi4jj09yxj56hlr0v1z70qb4bidf9g4zzq4y4rxl4wdimh2qr"))
      (file-name (string-append "rde-" version "-checkout"))))
    (build-system guile-build-system)
    (outputs (list "out" "doc"))
    (native-inputs
     (list gnu-make guile-3.0 texinfo))
    (inputs
     (list guix))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'configure
            (lambda _
              (setenv "GUILE_LOAD_PATH"
                      (string-join
                       (list (string-append (getcwd) "/src")
                             (getenv "GUILE_LOAD_PATH"))
                       ":"))))
          (replace 'build
            (lambda args
              (with-directory-excursion "src"
                (apply (assoc-ref %standard-phases 'build) args))))
          (add-after 'build 'build-info
            (lambda _
              (invoke "make" "doc/rde.info")))
          (replace 'install-documentation
            (lambda _
              (install-file "doc/rde.info"
                            (string-append #$output:doc "/share/info")))))))
    (synopsis "Developers and power user friendly GNU/Linux distribution")
    (description "The GNU/Linux distribution, a set of tools for managing
development environments, home environments, and operating systems, a set of
predefined configurations, practices and workflows.")
    (license license:gpl3+)))

;; (define-public rde-latest
;;   (package
;;     (inherit rde)
;;     (source
;;      (local-file (dirname (dirname (current-filename))) #:recursive? #t))))

