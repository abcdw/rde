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

(define-module (rde packages package-management)
  #:use-module (rde lib file)
  #:use-module (rde packages guix)
  #:export (guix-from-channels-lock)
  #:declarative? #f)

(define my-channels
  (load (canonicalize-path (find-file-in-load-path "channels-lock.scm"))))

(define guix-from-channels-lock
  (make-guix-package my-channels))
