;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)

  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-next-pgtk-stable
  emacs-next-pgtk)

(define-public emacs-next-pgtk-latest emacs-next-pgtk-stable)

(define-public emacs-consumer
  (package
    (inherit emacs)
    (name "emacs-consumer")
    (build-system trivial-build-system)
    (arguments
     `(#:builder
       (let ((out (assoc-ref %outputs "out")))
         (mkdir out)
         #t)))
    (license license:gpl3+)
    (home-page "https://sr.ht/~abcdw/rde")
    (synopsis "Apropriate values for @env{EMACSLOADPATH} and @env{INFOPATH}.")
    (description "This package helps to set environment variables, which make
emacs packages of current profile explorable by external Emacs.")))
