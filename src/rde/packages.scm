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
  #:use-module ((guix licenses) #:prefix license:)
  #:export (strings->packages
            strings->inferior-packages))

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
    (version "0.3.0")
    (home-page "https://trop.in/rde")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference (url "https://git.sr.ht/~abcdw/rde")
                          (commit "bb580995a3ace2ecf4e3ef33d200d5b8faad751e")))
      (sha256
       (base32
        "16xs407jhf93m4nn6ysbyfbfnxf2g7g9c5sbiblngckdhiwkjll9"))
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
             (gnu packages pciutils)
             (gnu packages pcre)
             (gnu packages wm))

(define-public wayland-protocols-latest
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols")
    (version "1.30")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.freedesktop.org/wayland"
                           "/" name "/-/releases/" version "/downloads/"
                           name "-" version ".tar.xz"))
              (sha256
               (base32
                "0q5w66472j548pdabnycn9byjsjy477qfzkd6yq80azxcpxrh51w"))))))

(define-public libinput-minimal-latest
  (package
    (inherit libinput-minimal)
    (name "libinput-minimal")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.freedesktop.org/libinput/libinput/-/archive"
             "/" version "/libinput-" version ".tar.gz"))
       (sha256
        (base32
         "0yqdrp29v804vyy9n8wnbdimijmgbi8gpnvacrcjz8gjdjb5mgpb"))))))

(define-public libdrm-latest
  (package
    (inherit libdrm)
    (name "libdrm")
    (version "2.4.114")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "09nhk3jx3qzggl5vyii3yh4zm0npjqsbxhzvxrg2xla77a2cyj9h"))))))

(define-public wayland-latest
  (package
    (inherit wayland)
    (name "wayland")
    (version "1.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.freedesktop.org/wayland"
                           "/" name "/-/releases/" version "/downloads/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1b0ixya9bfw5c9jx8mzlr7yqnlyvd3jv5z8wln9scdv8q5zlvikd"))))))

(define freshup-wayland-protocols
  (package-input-rewriting/spec
   `(("libdrm" . ,(const libdrm-latest))
     ("libinput-minimal" . ,(const libinput-minimal-latest))
     ("wayland" . ,(const wayland-latest))
     ("wayland-protocols" . ,(const wayland-protocols-latest)))))

(define-public wlroots-latest
  (freshup-wayland-protocols
   (package
     (inherit wlroots)
     (name "wlroots")
     (version "0.16.0")
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'configure 'hardcode-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "backend/drm/meson.build"
                 (("/usr/share")
                  (string-append #$hwdata:pnp "/share")))
               (substitute* "xwayland/server.c"
                 (("Xwayland") (string-append (assoc-ref inputs
                                                         "xorg-server-xwayland")
                                              "/bin/Xwayland")))
               #t)))))
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "18rfr3wfm61dv9w8m4xjz4gzq2v3k5vx35ymbi1cggkgbk3lbc4k")))))))

(define freshup-wlroots
  (package-input-rewriting/spec
   `(("libdrm" . ,(const libdrm-latest))
     ("libinput-minimal" . ,(const libinput-minimal-latest))
     ("wayland" . ,(const wayland-latest))
     ("wayland-protocols" . ,(const wayland-protocols-latest))
     ("wlroots" . ,(const wlroots-latest)))))

(define-public sway-latest
  (freshup-wlroots
   (package
     (inherit sway)
     (name "sway")
     (version "1.8-rc1")
     (inputs (append (package-inputs sway)
                     (list (list "pcre" pcre2)
                           (list "glibc" glibc))))
         (arguments
     `(;; elogind is propagated by wlroots -> libseat
       ;; and would otherwise shadow basu.
       #:configure-flags '("-Dsd-bus-provider=basu")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hardcode path to swaybg.
             (substitute* "sway/config.c"
               (("strdup..swaybg..")
                (string-append "strdup(\"" (assoc-ref inputs "swaybg")
                               "/bin/swaybg\")")))
             ;; Hardcode path to scdoc.
             (substitute* "meson.build"
               (("scdoc.get_pkgconfig_variable..scdoc..")
                (string-append "'" (assoc-ref inputs "scdoc")
                               "/bin/scdoc'")))
             #t)))))
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/swaywm/sway")
              (commit "baf027fc5b32c914864e3ee34b85f96c3089cd1e")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0g7pjancp5xny0fjdy2vs4pnsp21rffdha3v4lkzja2yfbpj0yb1")))))))
