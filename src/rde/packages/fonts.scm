;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022, 2023, 2025 Andrew Tropin <andrew@trop.in>
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
(define-module (rde packages fonts)

  #:use-module (guix build-system trivial)
  #:use-module (guix build-system font)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public font-noto-color-emoji
  (package
    (name "font-noto-color-emoji")
    (version "2.034")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/googlefonts/noto-emoji")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d6zzk0ii43iqfnjbldwp8sasyx99lbjp1nfgqjla7ixld6yp98l"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules `((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out #$output)
                 (font-dir (string-append out "/share/fonts"))
                 (truetype-dir (string-append font-dir "/truetype")))
            (chdir (assoc-ref %build-inputs "source"))
            (install-file "fonts/NotoColorEmoji.ttf" truetype-dir)))))
    (home-page "https://github.com/googlefonts/noto-emoji")
    (synopsis "Noto Color Emoji fonts")
    (description "Noto Color Emoji fonts.")
    (license license:silofl1.1)))

(define-public font-noto-emoji
  (package
    (name "font-noto-emoji")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zjaco13/Noto-Emoji-Monochrome")
             (commit "b80db438fe644bd25e0032661ab66fa72f2af0e2")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1yanf66n00hjgy78czh51px86q5fihgwbd5w3lvsk1g7qmabls9n"))))
    (build-system font-build-system)
    (home-page "https://fonts.google.com/noto/specimen/Noto+Emoji")
    (synopsis "Noto Emoji fonts")
    (description "Monochrome version of Noto Color Emoji fonts.")
    (license license:silofl1.1)))

(define-public font-iosevka-nerd
  (package
    (name "font-iosevka-nerd")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
             version "/Iosevka.zip"))
       (sha256
        (base32 "09zmfksn1qyhc0pql5cvr0nmd8i8swjlipjfhwgf4zs6mdgac3fv"))))
    (build-system font-build-system)
    (home-page "https://github.com/ryanoasis/nerd-fonts")
    (synopsis "Iosevka Nerd Font")
    (description "Iosevka font with nerd icons.")
    (license license:silofl1.1)))
