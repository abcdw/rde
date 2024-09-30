;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde packages video)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public mpv-uosc
  (package
    (name "mpv-uosc")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://github.com/tomasklaen/uosc/releases/download/"
             version
             "/uosc.zip"))
       (sha256
        (base32 "13lnnyy9qv7pg3yjvavrjznwvdn9cwj2fzv46ac1cjy57fff3ak1"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/mpv/" #:exclude ("scripts/uosc/bin")))))
    (home-page "https://github.com/tomasklaen/uosc")
    (synopsis "Feature-rich minimalist proximity-based UI for MPV player")
    (description "Feature-rich minimalist proximity-based UI for MPV player.")
    (license license:gpl3+)))

(define-public mpv-thumbfast
  (let ((commit "f1fdf10b17f394f2d42520d0e9bf22feaa20a9f4")
        (revision "1"))
    (package
      (name "mpv-thumbfast")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/po5/thumbfast")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0wxcbq4ji6qbbml37w5pdhg43k3mixfn6p3hapf7wc0gxmzhna3k"))))
      (build-system copy-build-system)
      (arguments
       (list #:install-plan
             #~'(("thumbfast.lua" "share/mpv/scripts/"))))
      (home-page "https://github.com/po5/thumbfast")
      (synopsis "High-performance on-the-fly thumbnailer script for mpv")
      (description "High-performance on-the-fly thumbnailer script for mpv.")
      (license license:mpl2.0))))
