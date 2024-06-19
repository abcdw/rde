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

(define-module (rde lib file)
  #:export (find-file-in-load-path))

(define (find-file-in-load-path file)
  "Find @code{file} with any extension in @code{%load-path}."
  ;; Guix sets %load-extensions to only .scm, which makes it impossible to use
  ;; %search-load-path function for finding resources.
  (let* ((old-load-extensions %load-extensions))
    (dynamic-wind
        (lambda () (set! %load-extensions '("")))
        (lambda () (%search-load-path file))
        (lambda () (set! %load-extensions old-load-extensions)))))
