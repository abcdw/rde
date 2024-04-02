;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2024 Demis Balbach <db@minikn.xyz>
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

(define-public emacs-json-simple-flymake
 (let ((commit "f3dacf070d1e04d5805323b0a95d58c5b9b7f607"))
  (package
   (name "emacs-json-simple-flymake")
   (version "20230802")
   (home-page "https://github.com/mokrates/json-simple-flymake")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "052c7xivwd2grxwpspjnfj3hzla2lgc3r8yq24i5jbyasdnpghbc"))))
   (build-system emacs-build-system)
   (synopsis "Really simple but standalone json flymake utilizing the
builtin json parser")
   (description
    "Really simple but standalone json flymake utilizing the builtin json
parser.")
   (license license:gpl3+))))

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

(define-public emacs-header-minions
  (package
    (inherit emacs-minions)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-it-update-header-line
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "minions.el"
	       (("mode-line-format")
                "header-line-format"))
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

(define-public emacs-transient-latest emacs-transient)

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
    `(("emacs-git-gutter" ,emacs-git-gutter)))
   (license license:gpl3+)
   (home-page "https://sr.ht/~abcdw/git-gutter-transient")
   (synopsis "Navigate, stage and revert hunks with ease")
   (description "This package provides transient interface for git-gutter function
to manipulate and navigate hunks.")))

(define-public emacs-gider
  (package
    (name "emacs-gider")
    (version "0.1.0")
    (source
     (local-file "../../../files/emacs/gider" #:recursive? #t))
    (arguments
     (list
      #:exclude #~(list "^\\.dir-locals\\.el$" "^test/")
      #:include #~(cons "^src/" %default-include)))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-geiser emacs-geiser-guile))
    (license license:gpl3+)
    (home-page "https://sr.ht/~abcdw/rde")
    (synopsis "Guile Interactive Development Enviroment")
    (description "Right now it's just a few helpers on top of geiser.")))

(define-public emacs-geiser-latest
  (let ((commit "bd12f2dc6c5949e260f094fb60737498cd0ae9a5")
        (revision "1"))
    (package
      (inherit emacs-geiser)
      (name "emacs-geiser")
      (version (git-version "0.28.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/emacs-geiser/geiser")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16qi3vk1yps4f5v98ipdl5kq0jq5qlnlpx8c598csj9yk86p1hsw")))))))

(define-public emacs-geiser-guile-latest
  ((package-input-rewriting/spec
    `(("emacs-geiser" . ,(const emacs-geiser-latest))))
   emacs-geiser-guile))

(define-public emacs-geiser-eros-latest
  ((package-input-rewriting/spec
    `(("emacs-geiser" . ,(const emacs-geiser-latest))))
   emacs-geiser-eros))

(define-public emacs-gider-latest
  ((package-input-rewriting/spec
    `(("emacs-geiser" . ,(const emacs-geiser-latest))
      ("emacs-geiser-guile" . ,(const emacs-geiser-guile-latest))))
   emacs-gider))

(define-public emacs-guix-latest
  ((package-input-rewriting/spec
    `(("emacs-geiser" . ,(const emacs-geiser-latest))))
   emacs-guix))

(define-public emacs-telega-server-latest emacs-telega-server)

(define-public emacs-telega-latest emacs-telega)

(define-public emacs-telega-contrib-latest emacs-telega-contrib)

(define-public emacs-consult-eglot-sans-eglot
  (package
    (inherit emacs-consult-eglot)
    (inputs
     (modify-inputs (package-inputs emacs-consult-eglot)
       (append emacs-eglot)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs emacs-consult-eglot)
       (delete "emacs-eglot")))))

(define-public emacs-dirvish-latest emacs-dirvish)

(define-public emacs-docker-latest
  (let ((commit "cc0046e6a557dce0ccc4108dd22e04f21ba8b0dc")
        (revision "0"))
    (package
      (inherit emacs-docker)
      (name "emacs-docker")
      (version (git-version "2.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Silex/docker.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11l8jpqj6m04ndhnfz41nhph1rqjvqbfd5vw334mph776aq1baln"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs emacs-docker)
         (delete "emacs-docker-tramp"))))))

(define-public emacs-clojure-ts-mode
  (package
    (name "emacs-clojure-ts-mode")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure-emacs/clojure-ts-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "101c1xwrmkb9rq713jij105117y2d0ffiplxsnb3z1h2pgcil0p8"))))
    (build-system emacs-build-system)
    (license license:gpl3+)
    (home-page "https://github.com/clojure-emacs/clojure-ts-mode.git")
    (synopsis "Major mode for Clojure code backed up by Tree-sitter")
    (description "\
clojure-ts-mode is an Emacs major mode that provides font-lock (syntax
highlighting), indentation, and navigation support for the Clojure(Script)
programming language, powered by the tree-sitter-clojure tree-sitter grammar.")))

(define-public emacs-zotra
  (let ((commit "c63e274950b5975c7d74f5d0df4b1a1e07f9b5f5")
        (revision "0"))
    (package
      (name "emacs-zotra")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mpedramfar/zotra")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jy4a8fhfdvk2i9ac18mka3b2hk3x3r8kmvxkhip1ks50s2m71cg"))))
      (build-system emacs-build-system)
      (license license:gpl3)
      (home-page "https://github.com/mpedramfar/zotra")
      (synopsis "Get bibliographic information from a url")
      (description
       "This emacs library provides functions to get bibliographic information
 from a url and save it into a bibtex file. It also provides a way to obtain a
 list of attachments (e.g. PDF files) associated with a url. This is done
 using Zotero translators, but without using the Zotero client."))))
