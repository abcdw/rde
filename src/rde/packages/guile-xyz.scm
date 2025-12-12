;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>
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

(define-module (rde packages guile-xyz)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system guile)
  #:use-module ((guix licenses) #:prefix license:))

(define-public guile-ares-rs-latest
  (let* ((commit "ce839bc8e74fbc5a625a148ab806277b24715d8c")
         (revision "1"))
    (package
      (inherit guile-ares-rs)
      (name "guile-ares-rs")
      (version (git-version "0.9.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~abcdw/guile-ares-rs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08zypn9wfddbpzcvm6h04jikj1bv0ixdy4r4x5sw7824m7v4a7i1"))))
      (arguments
       (list
        #:source-directory "src/guile"
        #:phases
        #~(modify-phases %standard-phases
            (delete 'install-script)))))))

(define-public guile-ares-shepherd
  (package
    (name "guile-ares-shepherd")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/cons-town/guile-debugger")
                    (commit (string-append "ares-shepherd-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1abnrp67zph4cc8x4pmmja52ma5ixf9ilcz13r19ga8ar8b36hv0"))))
    (build-system guile-build-system)
    (arguments
     (list #:source-directory "shepherd-nrepl/src/guile"))
    (native-inputs `(("guile" ,guile-next)))
    (inputs (list shepherd-1.0
                  guile-fibers
                  guile-ares-rs-latest))
    (home-page "https://codeberg.org/cons-town/guile-debugger")
    (synopsis "Shepherd interface for Ares")
    (description "ares-shepherd is an extension for Ares that adds the ability to
connect and interact to a shepherd via its nREPL service.")
    (license license:gpl3+)))

(define-public guile-libnotify-latest
  (let ((commit "f255d78d7b246aefc6223f770df19c48a942d209")
        (revision "0"))
    (package/inherit guile-libnotify
      (name "guile-libnotify")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ekaitz-zarraga/guile-libnotify")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "08k11bpqpxkj0gjnp48dc9jdpvwvqv2yvjwbki02aa6f3h06qljs")))))))
