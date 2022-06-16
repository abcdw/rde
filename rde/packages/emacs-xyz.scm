;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
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

(define-module (rde packages emacs-xyz)
  #:use-module (rde packages messaging)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages video)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-justify-kp
 (let ((commit "385e6b8b909ae0f570f30101cec3677e21c9e0a0"))
  (package
   (name "emacs-justify-kp")
   (version "20171119")
   (home-page "https://github.com/qzdl/justify-kp")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "13fylx4mvw7cgzd2mq060x43b1x7g5vdf16jm49c31f6b3jj1qi0"))))
   (build-system emacs-build-system)
   (inputs (list emacs-dash emacs-s))
   (synopsis "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (description
    "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (license license:gpl3+))))

(define-public emacs-org-modern
  (package
   (name "emacs-org-modern")
   (version "0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/minad/org-modern")
                  (commit "dc19304f409259d1b258c51cedd2d362e0ff9b98")))
            (sha256
             (base32 "1b0cis1n786c4lkrsi71ak2wv21mhgbfk3q2pp6qiqhddah0l1cg"))
            (file-name (git-file-name name version))))
   (build-system emacs-build-system)
   (license license:gpl3+)
   (home-page "https://github.com/minad/org-modern")
   (synopsis "")
   (description "")))

(define-public emacs-vertico-latest
  (let* ((commit "2de617a9199d152533ce280c6eb653147f15f8d1")
         (revision "2"))
    (package
     (inherit emacs-vertico)
     (name "emacs-vertico")
     (version (git-version "0.20" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minad/vertico")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08h1lv63dkrfk3m94z73xmjabch6699kd9qm3cvkcr8n67h6j6fp")))))))

(define-public emacs-marginalia-latest
  (let* ((commit "5767b6ff49e26ecd6aa26f552397d5d2b8213d25")
         (revision "0"))
    (package
     (inherit emacs-marginalia)
     (name "emacs-marginalia")
     (version (git-version "0.12" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minad/marginalia")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "143d57fy5i5ziwfxxix595k0f98ay5l57x5z69g8lkp6nb7b1rq7")))))))

(define-public emacs-vterm-latest
  (let ((version "0.0.1")
        (revision "1")
        (commit "a940dd2ee8a82684860e320c0f6d5e15d31d916f"))
    (package
     (inherit emacs-vterm)
     (name "emacs-vterm")
     (version (git-version version revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/akermu/emacs-libvterm")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r1iz92sn2ddi11arr9s8z7cdpjli7pn55yhaswvp4sdch7chb5r")))))))

(define-public emacs-consult-dir
  (package
   (name "emacs-consult-dir")
   (version "0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/karthink/consult-dir")
                  (commit (string-append "v" version))))
            (sha256
             (base32 "1cff4ssrn1mw2s5n090pdmwdirnfih8idg5f0ll2bi2djc4hq5kn"))
            (file-name (git-file-name name version))))
   (build-system emacs-build-system)
   (license license:gpl3+)
   (propagated-inputs (list emacs-consult))
   (home-page "https://github.com/karthink/consult-dir")
   (synopsis "Insert paths into minibuffer prompts in Emacs")
   (description "Consult-dir allows you to easily insert directory
paths into the minibuffer prompt in Emacs.

When using the minibuffer, you can switch - with completion and
filtering provided by your completion setup - to any directory you’ve
visited recently, or to a project or bookmarked directory. The
minibuffer prompt will be replaced with the directory you choose.")))

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
         ("notmuch" ,emacs-notmuch)))
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
    (local-file "../features/emacs/git-gutter-transient" #:recursive? #t))
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

(define-public emacs-consult-eglot
  (package
   (name "emacs-consult-eglot")
   (version "0.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/mohkale/consult-eglot")
                  (commit "0da8801dd8435160ce1f62ad8066bd52e38f5cbd")))
            (sha256
             (base32 "1qxk1npxbf8m3g9spikgdxcf6mzjx6cwy3f5vn6zz5ksh14xw3sd"))
            (file-name (git-file-name name version))))
   (build-system emacs-build-system)
   (license license:gpl3+)
   (propagated-inputs (list emacs-consult emacs-eglot))
   (home-page "https://github.com/mohkale/consult-eglot")
   (synopsis "Consulting-read interface for eglot")
   (description "This package acts as a parallel of consult-lsp for eglot and
provides a front-end interface for the workspace/symbols LSP procedure
call.")))

(define-public emacs-telega-server-latest
  (let ((commit "733194c4ed16f57e2b4e66a79be842a5d0731012")
        (revision "0"))
    (package
      (name "emacs-telega-server")
      (version (git-version "0.8.03" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zevlg/telega.el")
               (commit "733194c4ed16f57e2b4e66a79be842a5d0731012")))
         (sha256
          (base32 "0l3a2qbqwf6rrnnkranclqq9fig3ki176ddj02azr9360ps46xly"))
         (file-name (git-file-name "emacs-telega" version))
         (patches
          (search-patches "emacs-telega-path-placeholder.patch"
                          "emacs-telega-test-env.patch"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target))
                (string-append "INSTALL_PREFIX=" #$output "/bin"))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'enter-subdirectory
              (lambda _ (chdir "server")))
            (replace 'configure
              (lambda _
                (substitute* "run_tests.py"
                  (("^(TELEGA_SERVER = ).*$" _all prefix)
                   (string-append prefix
                                  "\"" #$output "/bin/telega-server\"\n")))))
            (delete 'check)
            (add-after 'install 'check
              (assoc-ref %standard-phases 'check))
            (add-before 'install-license-files 'leave-subdirectory
              (lambda _ (chdir ".."))))
        #:test-target "test"))
      (inputs
       (list tdlib-latest libappindicator))
      (native-inputs
       (list python pkg-config))
      (home-page "https://zevlg.github.io/telega.el/")
      (synopsis "Server process of Telega")
      (description "Telega-server is helper program to interact with Telegram
service, and connect it with Emacs via inter-process communication.")
      (license license:gpl3+))))

(define-public emacs-telega-latest
  (package
    (inherit emacs-telega-server-latest)
    (name "emacs-telega")
    (build-system emacs-build-system)
    (arguments
     `(#:emacs ,(if (target-64bit?)
                    emacs-minimal
                    ;; Require wide-int support for 32-bit platform.
                    emacs-wide-int)
       #:include (cons "^etc\\/" %default-include)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hard-code paths to `ffplay` and `ffmpeg`.
             (let* ((ffplay-bin (search-input-file inputs "/bin/ffplay"))
                    (ffmpeg-bin (search-input-file inputs "/bin/ffmpeg")))
               (substitute* '("telega-ffplay.el" "telega-vvnote.el")
                 (("(shell-command-to-string\|concat) \"(ffmpeg\|ffprobe)"
                   all func cmd)
                  (string-append func " \""
                                 (search-input-file
                                  inputs (string-append "/bin/" cmd))))
                 (("\\(executable-find \"ffplay\"\\)")
                  (string-append "(and (file-executable-p \"" ffplay-bin "\")"
                                 "\"" ffplay-bin "\")"))
                 (("\\(executable-find \"ffmpeg\"\\)")
                  (string-append "(and (file-executable-p \"" ffmpeg-bin "\")"
                                 "\"" ffmpeg-bin "\")"))))))
         (add-after 'unpack 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "telega-customize.el"
               (("@TELEGA_SERVER_BIN@")
                (search-input-file inputs "/bin/telega-server")))
             (substitute* "telega-util.el"
               (("@TELEGA_SHARE@")
                (string-append (elpa-directory (assoc-ref outputs "out"))
                               "/etc"))))))))
    (inputs
     (list emacs-telega-server-latest ffmpeg))
    (native-inputs '())
    (propagated-inputs
     (list emacs-visual-fill-column emacs-company
           emacs-rainbow-identifiers))
    (synopsis "GNU Emacs client for the Telegram messenger")
    (description "Telega is a full-featured, unofficial GNU Emacs-based client
for the Telegram messaging platform.")))

(define-public emacs-telega-contrib-latest
  (package
    (inherit emacs-telega-latest)
    (name "emacs-telega-contrib")
    (arguments
     `(#:exclude '("telega-live-location.el")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _ (chdir "contrib") #t))
         (add-before 'install-license-files 'leave-subdirectory
           (lambda _ (chdir "..") #t)))))
    (inputs '())
    (native-inputs '())
    (propagated-inputs
     (list emacs-alert emacs-all-the-icons emacs-dashboard emacs-telega-latest
           emacs-transient))
    (synopsis "Contributed packages to Telega")
    (description "Telega-contrib is a collection of third-party
contributed packages to Telega.")))
