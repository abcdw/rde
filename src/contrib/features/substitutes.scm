;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (contrib features substitutes)
  #:use-module (rde features)
  #:use-module (contrib services substitutes)
  #:export (feature-guix-north-america))

(define (feature-guix-north-america)
  "This feature simply provides substitutes from Guix North America."
  (feature
   (name 'guix-north-america)
   (system-services-getter
    (const (list guix-north-america-substitutes-service)))))
