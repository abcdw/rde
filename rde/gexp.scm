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

(define-module (rde gexp)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)

  #:export (slurp-file-like))

(define* (slurp-file-like file-like #:key (encoding "UTF-8"))
  "Returns a gexp, which reads all the content of the FILE-LIKE and returns it
as a string.  FILE-LIKE must be a file-like object."
  (when (not (file-like? file-like))
    (raise (formatted-message
            (G_ "~a is not a file-like object.")
            file-like)))
  #~(call-with-input-file #$file-like
      (@ (ice-9 textual-ports) get-string-all)
      #:encoding #$encoding))
