(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages texinfo)

  #:use-module (srfi srfi-1)

  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
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

(use-modules (gnu packages emacs))
(use-modules (guix utils))

(define-public emacs-next-pgtk-latest
  (let ((commit "172c055745b1eb32def7be8ddcaae975996a789f")
        (revision "1"))
    (package
      (inherit emacs-next-pgtk)
      (name "emacs-next-pgtk-latest")
      (version (git-version "29.0.50" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/emacs.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00qikl80lly6nz15b7pp7gpy28iw7fci05q6k1il20fkdx27fp4x"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-next)
         ((#:configure-flags flags ''())
          `(cons* "--with-pgtk" ,flags))))
      ;; (propagated-inputs
      ;;  (list gsettings-desktop-schemas glib-networking))
      (inputs
       (package-inputs emacs-next))
      )))

(use-modules (guix download)
             (guix build-system gnu)
             (gnu packages gnome)
             (gnu packages tls)
             (gnu packages gsasl)
             (gnu packages compression))

(define-public msmtp-latest
  (package
    (name "msmtp-latest")
    (version "1.8.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://marlam.de/msmtp/releases/"
                           "/msmtp-" version ".tar.xz"))
       (sha256
        (base32 "1klrj2a77671xb6xa0a0iyszhjb7swxhmzpzd4qdybmzkrixqr92"))
       (patches
        (search-patches "msmtpq-add-enqueue-option.patch"
                        "msmtpq-add-env-variables.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("libsecret" ,libsecret)
       ("gnutls" ,gnutls)
       ("zlib" ,zlib)
       ("gsasl" ,gsasl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://marlam.de/msmtp/")
    (arguments
     `(#:configure-flags (list "--with-libgsasl"
                               "--with-libidn"
                               "--with-tls=gnutls")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-additional-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/msmtp"))
                    (msmtpq "scripts/msmtpq")
                    (vimfiles (string-append out "/share/vim/vimfiles/plugin")))
               (install-file (string-append msmtpq "/msmtpq") bin)
               (install-file (string-append msmtpq "/msmtp-queue") bin)
               (install-file (string-append msmtpq "/README.msmtpq") doc)
               (install-file "scripts/vim/msmtp.vim" vimfiles)
               (substitute* (string-append bin "/msmtp-queue")
                 (("^exec msmtpq") (format #f "exec ~a/msmtpq" bin)))
               (substitute* (string-append bin "/msmtpq")
                 (("^MSMTP=msmtp") (format #f "MSMTP=~a/msmtp" bin))
                 ;; Make msmtpq quite by default, because Emacs treat output
                 ;; as an indicator of error.  Logging still works as it was.
                 (("^EMAIL_QUEUE_QUIET=\\$\\{MSMTPQ_QUIET:-\\}")
                  "EMAIL_QUEUE_QUIET=${MSMTPQ_QUIET:-t}")
                 ;; Use ping test instead of netcat by default, because netcat
                 ;; is optional and can be missing.
                 (("^EMAIL_CONN_TEST=\\$\\{MSMTPQ_CONN_TEST:-n\\}")
                  "EMAIL_CONN_TEST=${MSMTPQ_CONN_TEST:-p}"))
               #t))))))
    (synopsis
     "Simple and easy to use SMTP client with decent sendmail compatibility")
    (description
     "msmtp is an SMTP client.  In the default mode, it transmits a mail to
an SMTP server (for example at a free mail provider) which takes care of further
delivery.")
    (license license:gpl3+)))

(define-public emacs-consumer
  (package
   (name "emacs-consumer")
   (version "0.1.0")
   (source (local-file "./packages.scm"))
   (build-system trivial-build-system)
   (arguments
    `(#:builder
      (let ((out (assoc-ref %outputs "out")))
        (mkdir out)
        #t)))
   (native-search-paths
    (list (search-path-specification
           (variable "EMACSLOADPATH")
           (files '("share/emacs/site-lisp")))
          (search-path-specification
           (variable "INFOPATH")
           (files '("share/info")))))
   (license license:gpl3+)
   (home-page "https://sr.ht/~abcdw/rde")
   (synopsis "Apropriate values for @env{EMACSLOADPATH} and @env{INFOPATH}.")
   (description "This package helps to set environment variables, which make
emacs packages of current profile explorable by external Emacs.")))

(use-modules (gnu packages glib))
(define-public pipewire-media-session
  (package
    (name "pipewire-media-session")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/pipewire/media-session")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gjspcgl5z19j4m3jr0771a1cxiizzvkjsw2v4rq6d92760zp7bv"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list pipewire-0.3
           alsa-lib
           dbus))
    (home-page "https://pipewire.org/")
    (synopsis "PipeWire Media Session")
    (description #f)
    (license license:expat)))

(use-modules (gnu packages gtk)
             (gnu packages image))

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

(define-public go-github-com-yory8-clipman
  (package
    (name "go-github-com-yory8-clipman")
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

(use-modules (gnu packages base))

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

(define-public rde-latest
  (package
    (inherit rde)
    (source
     (local-file (dirname (dirname (current-filename))) #:recursive? #t))))

