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

(define-module (rde packages emacs)
  #:use-module (gnu packages emacs)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

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
       (package-inputs emacs-next)))))

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
