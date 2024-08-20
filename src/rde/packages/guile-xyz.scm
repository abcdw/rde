;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download))

(define-public guile-ares-rs-latest
  (let* ((commit "a11f745d631ccc713ea1a97406d0586a65b16a02")
         (revision "2"))
    (package
      (inherit guile-ares-rs)
      (name "guile-ares-rs")
      (version (git-version "0.9.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~abcdw/guile-ares-rs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "12cx97whzlps409kqpn7gvb62r88xj46g4m1na1skcyjvnglfv9f")))))))
