;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 conses <contact@conses.eu>
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
  #:use-module (ice-9 match)
  #:export (file-like->name
            slurp-file-like
            template-file))

(define (file-like->name file)
  "Takes a file-like FILE and returns the corresponding file-like name."
  (match file
    ((? local-file?)
     (local-file-name file))
    ((? plain-file?)
     (plain-file-name file))
    ((? computed-file?)
     (computed-file-name file))
    (_ (leave (G_ "~a is not a local-file, plain-file or \
computed-file object~%") file))))

(define* (slurp-file-like file #:key (encoding "UTF-8"))
  "Returns a gexp, which reads all the content of the FILE and returns it as a
string.  FILE must be a file-like object."
  (when (not (file-like? file))
    (raise (formatted-message
            (G_ "~a is not a file-like object.")
            file)))
  #~(call-with-input-file #$file
      (@ (ice-9 textual-ports) get-string-all)
      #:encoding #$encoding))

(define* (template-file name file substitutes
                        #:key (encoding "UTF-8"))
  "Accepts file-like FILE and produces file-like NAME, where all entries matched
with patterns from SUBSTITUTES, changed to corresponding values.  SUBSTITUTES
is alist of regex patterns and values."
  (mixed-text-file
   name
   #~(begin
       (use-modules (ice-9 regex)
                    (ice-9 textual-ports)
                    (srfi srfi-1))
       (fold-right
        (lambda (pattern str)
          (regexp-substitute/global
           #f
           (car pattern) str
           'pre (cadr pattern) 'post))
        (call-with-input-file #$file get-string-all #:encoding #$encoding)
        '#$substitutes))))
