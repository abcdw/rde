(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)

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
    (list (string-append %channel-root "/rde/packages/patches"))
    (%patch-path))))

(use-modules (gnu packages emacs))
(define-public emacs-next-pgtk-latest
  (let ((commit "01b0a909b5ca858a09484821cc866127652f4153")
        (revision "4"))
    (package
      (inherit emacs-next-pgtk)
      (name "emacs-next-pgtk-latest")
      (version (git-version "28.0.50" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/emacs.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1agfssdllfvjpq3vcwn5hi6cb7il042phl41y79b17gjg612qc6b")))))))

(use-modules (gnu packages emacs-xyz)
             (guix build-system emacs))

(define-public emacs-cyrillic-dvorak-im
  (package
    (name "emacs-cyrillic-dvorak-im")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xFA25E/cyrillic-dvorak-im")
             (commit version)))
       (sha256
        (base32 "12adszd4p9i9glx2chasgq68i6cnxcrwbf5c268jjb5dw4q7ci0n"))
       (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (home-page "https://github.com/xFA25E/cyrillic-dvorak-im")
    (synopsis "Cyrillic input method for dvorak layout")
    (description "Cyrillic input method for dvorak layout.")
    (license license:gpl3+)))

(define-public emacs-mini-frame
  (package
   (inherit emacs-unfill)
   (name "emacs-mini-frame")
   (version "1.0.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/muffinmad/emacs-mini-frame.git")
                  (commit "41afb3d79cd269726e955ef0896dc077562de0f5")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0yghz9pdjsm9v6lbjckm6c5h9ak7iylx8sqgyjwl6nihkpvv4jyp"))))))

(define-public emacs-hide-header-line
  (package
   (inherit emacs-hide-mode-line)
   (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-it-update-header-line
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "hide-mode-line.el"
	       ((" mode-line-format")
                " header-line-format"))
             #t)))))))

(use-modules (gnu packages shellutils)
             (guix utils))
(define-public zsh-autosuggestions-latest
  (package
   (inherit zsh-autosuggestions)
   (name "zsh-autosuggestions")
   (version "0.7.0")
   (arguments
    (substitute-keyword-arguments (package-arguments zsh-autosuggestions)
      ((#:phases phases)
       `(modify-phases ,phases
        (delete 'check)))))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/zsh-users/zsh-autosuggestions")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1g3pij5qn2j7v7jjac2a63lxd97mcsgw6xq6k5p7835q9fjiid98"))))))

(use-modules (guix build-system emacs)
             (gnu packages mail)
             (gnu packages texinfo))
(define-public emacs-git-email-latest
  (let* ((commit "b5ebade3a48dc0ce0c85699f25800808233c73be")
         (revision "0"))
    (package
      (name "emacs-git-email")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~yoctocell/git-email")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1lk1yds7idgawnair8l3s72rgjmh80qmy4kl5wrnqvpmjrmdgvnx"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; piem is not yet packaged in Guix.
           (add-after 'unpack 'remove-piem
             (lambda _
               (delete-file "git-email-piem.el")
               (delete-file "git-email-gnus.el")
               (delete-file "git-email-mu4e.el")))
           (add-before 'install 'makeinfo
             (lambda _
               (invoke "makeinfo" "doc/git-email.texi"))))))
      (native-inputs
       `(("texinfo" ,texinfo)))
      (inputs
       `(("emacs-magit" ,emacs-magit)
         ("notmuch" ,notmuch)))
      (license license:gpl3+)
      (home-page "https://sr.ht/~yoctocell/git-email")
      (synopsis "Format and send Git patches in Emacs")
      (description "This package provides utilities for formatting and
sending Git patches via Email, without leaving Emacs."))))

(define-public emacs-git-gutter-transient
  (package
   (name "emacs-git-gutter-transient")
   (version "0.1.0")
   (source
    (local-file "./features/emacs/git-gutter-transient" #:recursive? #t))
   (build-system emacs-build-system)
   (inputs
    `(("emacs-magit" ,emacs-magit)))
   (propagated-inputs
    `(("emacs-git-gutter" ,emacs-git-gutter)
      ("emacs-transient" ,emacs-transient)))
   (license license:gpl3+)
   (home-page "https://sr.ht/~abcdw/git-gutter-transient")
   (synopsis "Navigate, stage and revert hunks with ease")
   (description "This package provides transient interface for git-gutter function
to manipulate and navigate hunks.")))

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


