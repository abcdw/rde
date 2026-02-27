;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024, 2025 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2024 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2024, 2025 Nicolas Graves <ngraves@ngraves.fr>
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
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-arei-latest
  (let* ((commit "6741d93ebee0a3c9755b1b6722126d9e1145d310")
         (revision "3"))
    (package
      (inherit emacs-arei)
      (name "emacs-arei")
      (version (git-version "0.9.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~abcdw/emacs-arei")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1c2yx4wn04ns9igk6pw02ir68hwpb2wk0nqvm6hfx7zliy079h69"))))
      (arguments
       (list
        #:lisp-directory "lisp"
        #:tests? #f))
      (propagated-inputs (append
                          (package-propagated-inputs emacs-arei)
                          (list (list "emacs-consult" emacs-consult))))
      (build-system emacs-build-system))))

(define-public emacs-justify-kp
  (let ((commit "33a186e297c0359547820088669486afd7b5fddb")
        (revision "1"))
    (package
      (name "emacs-justify-kp")
      (version (git-version "0.0.1" revision commit))
      (home-page "https://github.com/Fuco1/justify-kp")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14k75m10lxfknij5np5s4hhl9d7qbmkdcqkq145hkhgp81qgld73"))))
      (build-system emacs-build-system)
      (inputs (list emacs-dash emacs-s))
      (synopsis "Paragraph justification for emacs using Knuth/Plass algorithm")
      (description
       "Paragraph justification for emacs using Knuth/Plass algorithm ")
      (license license:gpl3+))))

(define-public emacs-eslint-fix
 (let ((commit "636bf8d8797bdd58f1b543c9d3f4910e3ce879ab"))
  (package
   (name "emacs-eslint-fix")
   (version "1.0")
   (home-page "https://github.com/codesuki/eslint-fix")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "02hjm685fl4f33s5fi8nc088wwfzhyy6abx5g4i93b2dx3hr2lyi"))))
   (build-system emacs-build-system)
   (synopsis "Fix current file using ESLint --fix")
   (description
    "This packages provides `eslint-fix', which fixes the current file using ESLint.")
   (license license:gpl3+))))

(define-public emacs-flymake-eslint
 (let ((commit "6ab909b85a8e97815db9831cdd5f283a7830177f"))
  (package
   (name "emacs-flymake-eslint")
   (version "1.7.0")
   (home-page "https://github.com/orzechowskid/flymake-eslint")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "19jyim5rrmv8bdskaw8gxb6fch3jj3irqhddi2aaxvgdxn321yxm"))))
   (build-system emacs-build-system)
   (synopsis "Flymake backend for Javascript using eslint")
   (description
    "A backend for Flymake which uses eslint.  Enable it with M-x
flymake-eslint-enable RET.")
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
     (substitute-keyword-arguments (package-arguments emacs-minions)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'make-it-update-header-line
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* "minions.el"
	          (("mode-line-format")
                   "header-line-format"))))))))))

(define-public emacs-git-email-sans-mu4e
  (package
    (inherit emacs-git-email)
    (inputs (modify-inputs (package-inputs emacs-git-email)
              (delete "mu")))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-git-email)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'remove-mu4e
              (lambda _
                (delete-file "git-email-mu4e.el")))))))))

;; Note: There is also a channel with a development version
;; at https://codeberg.org/suhail/git-email
(define-public emacs-git-email-latest emacs-git-email-sans-mu4e)

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

(define-public emacs-srht-latest
  (let ((commit "d9a8f6a43671d67a86622507136d4195c2dcd149")
        (revision "1"))
    (package
      (inherit emacs-srht)
      (name "emacs-srht")
      (version (git-version "0.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~akagi/srht.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "08ysidwlz4z6baih2810fpr1679217lnsb0jhwyvvj05g25ysy5b")))))))

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

(define-public emacs-guix-minimal
  (package
    (inherit emacs-guix)
    (build-system emacs-build-system)
    (native-inputs '())
    (inputs '())
    (propagated-inputs '())
    (arguments
     (list
      #:include ''("\
^guix-(auto-mode|build-log|derivation|env-var|prettify|scheme|utils)\\.el")
      #:modules '((guix build emacs-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (srfi srfi-71)
                  (ice-9 regex)
                  (ice-9 textual-ports))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda _
              (chdir "elisp")
              (let* ((all (call-with-input-file "guix-utils.el"
                            get-string-all))
                     (match-str
                      (lambda (from to base)
                        (let ((res (string-match
                                    (string-append "(" from ".*)" to)
                                    base)))
                          (values (and res (match:substring res 1))
                                  (match:suffix res)))))
                     (pprint rest (match-str
                                   "\\(cl-defun guix-pretty-print-buffer"
                                   "\\(defun guix-pretty-print-file"
                                   all))
                     (search rest (match-str
                                   "\\(defmacro guix-while-search"
                                   "\\(defmacro guix-while-null"
                                   rest)))
                (substitute* "guix-build-log.el"
                  (("guix-find-file-or-url") "find-file-existing"))
                (substitute* "guix-derivation.el"
                  (("guix-find-file") "find-file-existing"))
                (call-with-output-file "guix-utils.el"
                  (lambda (port)
                    (display "(require 'cl-lib)\n\n" port)
                    (for-each
                     (cut display <> port)
                     (list pprint search
                           (match-str ";;; Fontification" ";;; Diff" rest)))
                    (display "(provide 'guix-utils)" port)))))))))
    (description
     (string-append (package-description emacs-guix) "

Note: This is a minimalist variant of emacs-guix, with simply
file prettification."))))

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

(define-public emacs-difftastic
  (let ((commit "b33554a22c637f147d07c15fa9539c72bcfcfca0")
        (revision "0"))
    (package
      (name "emacs-difftastic")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pkryger/difftastic.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rc5gnb1hdlh7gjpg8igrrzcvxk53h76mcbnbhmfdhxfixb2xh8r"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ;tests require cask
      (propagated-inputs (list emacs-compat emacs-magit emacs-transient))
      (home-page "https://github.com/pkryger/difftastic.el")
      (synopsis "Structural diff tool integration for Emacs")
      (description
       "This package provides Emacs integration for difftastic, a structural
diff tool that understands syntax.  It integrates with Magit to show
structural diffs in place of regular diffs.")
      (license license:gpl3+))))

(define-public emacs-arei-shepherd
  (package
    (name "emacs-arei-shepherd")
    (version "0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/cons-town/guile-debugger")
                    (commit (string-append "arei-shepherd-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f32ymabyhz0bczsic4z35l36vharlfjrsm0hdsj4hd1c836qacy"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'discover-package
            (lambda _
              (symlink "shepherd-nrepl/src/elisp/arei-shepherd.el"
                       "arei-shepherd.el"))))))
    (inputs (list emacs-arei-latest
                  emacs-embark))
    (home-page "https://codeberg.org/cons-town/guile-debugger")
    (synopsis "Shepherd interface for Arei")
    (description "arei-shepherd is an extension for Arei that allows to interract with
the shepherd via the ares-shepherd extension for the nREPL.")
    (license license:gpl3+)))
