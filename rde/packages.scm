(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:))

(use-modules (gnu packages video)
             (gnu packages glib))
(define-public obs-latest
  (package
   (inherit obs)
   (name "obs")
   (version "27.0.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/obsproject/obs-studio")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1sy58mg9dris261ia6l6xaswl4ks76xh4fcsj81i2hfg1sjy1vxv"))
            (patches
             (search-patches "obs-modules-location.patch"))))
   (inputs (append
            (package-inputs obs)
            `(("wayland" ,wayland)
              ("pipewire" ,pipewire-0.3)
              ("glib" ,glib)
              ("wayland-protocols" ,wayland-protocols))))
   (arguments
    `(#:configure-flags
      (list (string-append "-DOBS_VERSION_OVERRIDE=" ,version)
            "-DENABLE_UNIT_TESTS=TRUE"
            "-DBUILD_BROWSER=FALSE"
            "-DBUILD_VST=FALSE")
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-executable
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (plugin-path (getenv "QT_PLUGIN_PATH")))
              (wrap-program (string-append out "/bin/obs")
                `("QT_PLUGIN_PATH" ":" prefix (,plugin-path))))
            #t)))))
   ;; (native-search-paths
   ;;  (list
   ;;   (search-path-specification
   ;;    (variable "OBS_PLUGINS_DATA_PATH")
   ;;    (files '("share/obs/obs-plugins")))
   ;;   (search-path-specification
   ;;    (variable "OBS_PLUGINS_PATH")
   ;;    (files '("lib/obs-plugins")))))
   ))

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

;; xdg-desktop-portal-latest
