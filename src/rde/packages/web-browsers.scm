;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
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

(define-module (rde packages web-browsers)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public nyxt-next
  (let ((commit "44c60496773a944a193ed6a1f79fa116cbfe1ef8")
        (revision "0"))
    (package
      (inherit nyxt)
      (name "nyxt-next")
      (version (git-version "3.4" revision commit))
      (source
       (origin
         (inherit (package-source nyxt))
         (uri (git-reference
               (url "https://github.com/atlas-engineer/nyxt")
               (commit commit)))
         (file-name (git-file-name "nyxt" version))
         (sha256
          (base32
           "1a9zxcswi9k4mw8nidbr8dff3avc6z3p0nx092nq751j4xbcgxxa"))))
      (arguments
       (substitute-keyword-arguments (package-arguments nyxt)
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'configure-tests
                (lambda _
                  (setenv "NASDF_TESTS_NO_NETWORK" "1")
                  (setenv "NASDF_TESTS_QUIT_ON_FAIL" "1")))))))
      (native-inputs
       (modify-inputs (package-native-inputs nyxt)
         (delete sbcl-prove)
         (prepend sbcl-lisp-unit2)))
      (propagated-inputs
       (list
        gst-libav
        gst-plugins-bad
        gst-plugins-base
        gst-plugins-good
        gst-plugins-ugly
        aspell
        aspell-dict-en))
      (inputs
       (modify-inputs (package-inputs nyxt)
         (delete sbcl-cl-css)
         (prepend sbcl-cl-gopher
                  sbcl-nhooks
                  sbcl-nkeymaps
                  sbcl-phos
                  sbcl-dissect
                  sbcl-ndebug
                  sbcl-cl-tld
                  sbcl-history-tree
                  sbcl-montezuma
                  sbcl-nfiles
                  sbcl-ospm
                  sbcl-lass
                  sbcl-njson
                  sbcl-py-configparser
                  sbcl-cl-webkit
                  cl-nsymbols
                  sbcl-slynk
                  pkg-config
                  gcc-toolchain))))))
