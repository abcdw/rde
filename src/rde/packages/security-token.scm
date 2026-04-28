;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (rde packages security-token)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages))


;;; Go libraries.

(define-public go-github-com-esiqveland-notify
  (package
    (name "go-github-com-esiqveland-notify")
    (version "0.13.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/esiqveland/notify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03d27a8rl55j6dqclg3233dj3j7v0i0p9cda06f9c4a21djamrfb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/esiqveland/notify"
      #:tests? #f))
    (propagated-inputs
     (list go-github-com-godbus-dbus-v5))
    (home-page "https://github.com/esiqveland/notify")
    (synopsis "D-Bus desktop notification client for Go")
    (description
     "This package provides a Go wrapper around godbus for the D-Bus desktop
notification interface.")
    (license license:bsd-3)))

(define-public go-github-com-rjeczalik-notify
  (package
    (name "go-github-com-rjeczalik-notify")
    (version "0.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rjeczalik/notify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yl4yqx3qxi2adl6hpg4kzx6crhaksr237wnzhqmj89gqvvwgxmr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rjeczalik/notify"
      #:tests? #f))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/rjeczalik/notify")
    (synopsis "File system event notification library for Go")
    (description
     "This package implements access to platform file system event
notifications for Go programs.")
    (license license:expat)))

(define-public go-github-com-vtolstov-go-ioctl
  (let ((version "0.0.0-20151206205506-6be9cced4810"))
    (package
      (name "go-github-com-vtolstov-go-ioctl")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://proxy.golang.org/github.com/vtolstov/go-ioctl/@v/v"
               version ".zip"))
         (file-name (string-append name "-" version ".zip"))
         (sha256
          (base32 "1pn4ahkharz9y9wzchmqvs9xmarxdxki1rgqakmm751qndg6vs96"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/vtolstov/go-ioctl"
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source import-path #:allow-other-keys)
                (let* ((tmpdir (string-append (getcwd) "/unpack"))
                       (dest (string-append (getenv "GOPATH") "/src/"
                                            import-path))
                       (module-dir
                        (string-append tmpdir
                                       "/github.com/vtolstov/go-ioctl@v"
                                       #$version)))
                  (mkdir-p tmpdir)
                  (mkdir-p dest)
                  (invoke "unzip" source "-d" tmpdir)
                  (copy-recursively module-dir dest)))))))
      (native-inputs
       (list unzip))
      (home-page "https://github.com/vtolstov/go-ioctl")
      (synopsis "Ioctl helper library for Go")
      (description
       "This package provides helpers for calling ioctl from Go programs.")
      (license license:expat))))


;;; Applications.

(define-public yubikey-touch-detector
  (let ((version "1.13.0"))
    (package
      (name "yubikey-touch-detector")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/max-baz/yubikey-touch-detector")
               (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zbd93s8cf41yykik1c0zf1lvzhz7cwxp6cdimhybhdcvcvp7615"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/maximbaz/yubikey-touch-detector"
        #:install-source? #f
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'set-version
              (lambda* (#:key import-path #:allow-other-keys)
                (substitute* (string-append "src/" import-path "/main.go")
                  (("var version = .*")
                   (string-append "var version = \"" #$version "\"")))))
            (add-after 'install 'install-extra-files
              (lambda* (#:key outputs import-path #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (src (string-append "src/" import-path))
                       (doc (string-append
                             out "/share/doc/yubikey-touch-detector-"
                             #$version))
                       (examples (string-append doc "/examples"))
                       (man1 (string-append out "/share/man/man1"))
                       (icons (string-append
                               out
                               "/share/icons/hicolor/128x128/apps")))
                  (mkdir-p doc)
                  (copy-file (string-append src "/README.md")
                             (string-append doc "/README.md"))
                  (mkdir-p examples)
                  (for-each
                   (lambda (file)
                     (copy-file (string-append src "/" file)
                                (string-append examples "/" file)))
                   '("service.conf.example"
                     "yubikey-touch-detector.service"
                     "yubikey-touch-detector.socket"))
                  (substitute*
                      (string-append examples
                                     "/yubikey-touch-detector.service")
                    (("/usr/bin/yubikey-touch-detector")
                     (string-append out "/bin/yubikey-touch-detector")))
                  (mkdir-p man1)
                  (with-input-from-file
                      (string-append src "/yubikey-touch-detector.1.scd")
                    (lambda _
                      (with-output-to-file
                          (string-append man1 "/yubikey-touch-detector.1")
                        (lambda _
                          (invoke "scdoc")))))
                  (mkdir-p icons)
                  (copy-file (string-append
                              src "/yubikey-touch-detector.png")
                             (string-append
                              icons "/yubikey-touch-detector.png"))))))))
      (native-inputs
       (list pkg-config
             scdoc
             go-github-com-coreos-go-systemd-v22
             go-github-com-deckarep-golang-set
             go-github-com-esiqveland-notify
             go-github-com-godbus-dbus-v5
             go-github-com-proglottis-gpgme
             go-github-com-rjeczalik-notify
             go-github-com-sirupsen-logrus
             go-github-com-vtolstov-go-ioctl))
      (inputs
       (list gpgme))
      (home-page "https://github.com/max-baz/yubikey-touch-detector")
      (synopsis "Detect when a YubiKey is waiting for a touch")
      (description
       "YubiKey touch detector monitors Linux YubiKey activity and emits events
when GPG, SSH, U2F, FIDO2, or HMAC operations are waiting for a physical touch.
It can notify through a Unix socket, D-Bus, stdout, or desktop notifications.")
      (license license:isc))))
