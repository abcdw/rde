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

(define-module (rde packages aspell)
  #:use-module (guix build-system copy)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download))


(define* (hunspell-dict language synopsis
                        #:optional
                        (nick (string-map (lambda (chr)
                                            (if (char=? #\_ chr)
                                                #\-
                                                chr))
                                          (string-downcase language))))

  (package
    (name "hunspell-dict-ru")
    (version "7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LibreOffice/dictionaries")
                    (commit "libreoffice-7-4-branch-point")))
              (sha256
               (base32
                "1gbzmq9dcf1q2hndcs5pxdsn4ik27az2qdkj6hpz0vmp4z7v9n48"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'((#$(string-append language "/" language ".aff")
           "share/hunspell/")
          (#$(string-append language "/" language ".dic")
           "share/hunspell/"))

      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'add-my-spell
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((myspell (string-append #$output "/share/myspell"))
                    (hunspell (string-append #$output "/share/hunspell")))
                (mkdir-p myspell)
                (symlink hunspell (string-append myspell "/dicts"))))))))

    (synopsis synopsis)
    (description
     "This package provides a dictionary for the Hunspell spell-checking
library.")
    (home-page "https://github.com/LibreOffice/dictionaries")
    (license (non-copyleft "file://Copyright"
                           "Word lists come from several sources, all
under permissive licensing terms.  See the 'Copyright' file."))))

(define-public hunspell-dict-ru
  (hunspell-dict "ru_RU" "Hunspell dictionary for Russian"))
