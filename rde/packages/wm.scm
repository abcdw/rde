;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (rde packages wm)
  #:use-module (gnu packages wm)
  #:use-module (guix packages)
  #:use-module (guix git-download))

(define-public waybar-stable
  (package
    (inherit waybar)
    (name "waybar")
    (version "0.9.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; commit from fork with battery status fix
             ;; https://github.com/grfrederic/Waybar/tree/normalize-battery-capacity
             (url "https://github.com/Alexays/Waybar")
             (commit "2c80564c1b1009111a145368847015e8416da04f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "001m7gl04l71is4zla2219njyry7h3n1lpdfcfjbxhp3b0in254f"))))))
