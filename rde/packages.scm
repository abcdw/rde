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

(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages base)

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
  #:use-module ((guix licenses) #:prefix license:))

(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%rde-patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))

(define %channel-root
  (find (lambda (path)
          (file-exists? (string-append path "/rde/packages.scm")))
        %load-path))

(define %rde-patch-path
  (make-parameter
   (append
    (list (string-append %channel-root "rde/packages/patches"))
    (%patch-path))))

(define-public rofi-wayland
  (package
   (inherit rofi)
   (name "rofi-wayland")
   (version "1.7.2+wayland1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/lbonn/rofi"
                                "/releases/download/"
                                version "/rofi-" version ".tar.xz"))
            (sha256
             (base32
              "1smrxjq693z48c7n5pcfrvb0m0vsn6pxn7qpn8bm68j942n8rg3x"))))
   (build-system meson-build-system)
   (arguments
    (substitute-keyword-arguments (package-arguments rofi)
      ((#:configure-flags flags '())
       #~(list "-Dxcb=disabled"))))
    (inputs
     (list cairo
           glib
           libjpeg-turbo
           librsvg
           libxkbcommon
           wayland
           wayland-protocols
           pango
           startup-notification))
    (description "Rofi is a minimalist application launcher.  It memorizes which
applications you regularly use and also allows you to search for an application
by name.

This is a fork with added support for Wayland via layer shell protocol.")))

(define-public wtype
  (package
   (name "wtype")
   (version "0.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/atx/wtype"
                                "/archive/refs/tags/v" version ".tar.gz"))
            (sha256
             (base32
              "1ya6hxmgmsmxsy3yzssq4q2xm7lkfc253g44p0lnwslbh9npi4fs"))))
   (build-system meson-build-system)
   (inputs (list libxkbcommon wayland pkg-config))
   (home-page "https://github.com/atx/wtype")
   (synopsis "xdotool type for wayland")
   (description "wtype is a Wayland tool that allows you to simulate
keyboard input like xdotool type for X11.")
   (license license:gpl3+)))

(use-modules (guix build-system go)
             (gnu packages golang)
             (gnu packages syncthing))

(define-public go-gopkg-in-alecthomas-kingpin-v2
  (package
    (name "go-gopkg-in-alecthomas-kingpin-v2")
    (version "2.2.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/alecthomas/kingpin.v2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0mndnv3hdngr3bxp7yxfd47cas4prv98sqw534mx7vp38gd88n5r"))))
    (build-system go-build-system)
    (native-inputs
     (list go-github-com-alecthomas-template go-github-com-alecthomas-units
           go-github-com-stretchr-testify))
    (arguments
      '(#:import-path
        "gopkg.in/alecthomas/kingpin.v2"
        #:unpack-path
        "gopkg.in/alecthomas/kingpin.v2"
        #:phases %standard-phases))
    (home-page "https://gopkg.in/alecthomas/kingpin.v2")
    (synopsis "Kingpin - A Go (golang) command line and flag parser")
    (description "Package kingpin provides command line interfaces like this:")
    (license license:expat)))

(define-public clipman
  (package
    (name "clipman")
    (version "1.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yory8/clipman")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0b9kvj0dif4221dy6c1npknhhjxvbc4kygzhwxjirpwjws0yv6v9"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/yory8/clipman"))
    (propagated-inputs
     (list
      go-gopkg-in-alecthomas-kingpin-v2
      go-github-com-kballard-go-shellquote
      go-github-com-alecthomas-units
      go-github-com-alecthomas-template))
    (home-page "https://github.com/yory8/clipman")
    (synopsis "Clipman")
    (description "GPL v3.0 2019- (C) yory8 <yory8@users.noreply.github.com>")
    (license license:gpl3)))

(define-public rde
  (package
    (name "rde")
    (version "0.1.0")
    (home-page "https://trop.in/rde")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference (url "https://git.sr.ht/~abcdw/rde")
                          (commit "0d72e180b7eb92ea5a23d8dd481bde93b6ec252c")))
      (sha256
       (base32
        "0vcsgbziv6cm4b4sccllsg67anpxg0q9mm3d80nms60ng6ld3i6b"))
      (file-name (string-append "rde-" version "-checkout"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gnu-make texinfo))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (add-after 'install 'install-info
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (info (string-append out "/share/info")))
                (install-file "doc/rde.info" info)))))))
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



;;;
;;; Sway
;;;

(use-modules (gnu packages freedesktop)
             (gnu packages xdisorg)
             (gnu packages wm))

(define-public wayland-protocols-latest
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols")
    (version "1.26")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "04vgllmpmrv14x3x64ns01vgwx4hriljayjkz9idgbv83i63hly5"))))))

(define-public libdrm-latest
  (package
    (inherit libdrm)
    (name "libdrm")
    (version "2.4.112")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1zr0hi7k5s7my4q9hyj6ryzg89zyjx24zbqfv3c5rcq9pl87gc00"))))))

(define freshup-wayland-protocols
  (package-input-rewriting/spec
   `(("libdrm" . ,(const libdrm-latest))
     ("wayland-protocols" . ,(const wayland-protocols-latest)))))

(define-public wlroots-latest
  (freshup-wayland-protocols
   (package
     (inherit wlroots)
     (name "wlroots")
     (version "0.15.1")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00s73nhi3sc48l426jdlqwpclg41kx1hv0yk4yxhbzw19gqpfm1h")))))))

(define freshup-wlroots
  (package-input-rewriting/spec
   `(("libdrm" . ,(const libdrm-latest))
     ("wayland-protocols" . ,(const wayland-protocols-latest))
     ("wlroots" . ,(const wlroots-latest)))))

(define-public sway-latest
  (freshup-wlroots
   (package
     (inherit sway)
     (name "sway")
     (version "1.7")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/swaywm/sway")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ss3l258blyf2d0lwd7pi7ga1fxfj8pxhag058k7cmjhs3y30y5l")))))))

