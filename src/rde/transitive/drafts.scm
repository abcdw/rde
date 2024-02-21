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

(define-module (rde transitive drafts)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (guix modules)
  #:use-module (guix gexp))

(define (get-nix32-hash-binary)
  (car
   ((@ (rde api store) build-with-store)
    (program-file
     "get-hash"
     (with-extensions (list guile-json-4 guile-gcrypt)
       (with-imported-modules
           (source-module-closure
            '((guix build download)
              (guix base32)
              (gcrypt hash)))
         #~(let ((temp (port-filename (mkstemp "/tmp/hash-XXXXXX"))))
             (use-modules (guix build download)
                          (guix base32)
                          ((gcrypt hash) #:prefix gcrypt:))
             (parameterize ((current-output-port (current-error-port)))
               (url-fetch (cadr (command-line)) temp))
             (display
              (bytevector->nix-base32-string
               (gcrypt:file-sha256 temp))))))))))

(define (get-nix32-hash url)
  "Get nix-base32 encoded sha256 for a file at URL."
  (let* ((pipe (open-pipe* OPEN_READ (get-nix32-hash-binary) url))
         (hash (read-line pipe)))
    (close-pipe pipe)
    hash))

;; (get-nix32-hash "https://trop.in")
