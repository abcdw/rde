;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features-test)
  #:use-module (rde features)
  #:use-module (rde tests))


(define feature-1
  (feature
   (values '((a . b)
             (c . d)))
   (home-services-getter (lambda (x) '(ha hb)))
   (system-services-getter (lambda (x) '(sa sb)))))

(define feature-2
  (feature
   (values '((e . f)
             (g . h)))
   (home-services-getter (lambda (x) '(hc hd)))
   (system-services-getter (lambda (x) '(sc sd)))))

(define super-feature
  (merge-features (list feature-1 feature-2)))

(define-test merged-features
  (test-group "merged features"
    (test-equal "values combined"
      '((a . b)
        (c . d)
        (e . f)
        (g . h))
      (feature-values super-feature))
    (test-equal "home services combined"
      '(ha hb hc hd)
      ((feature-home-services-getter super-feature) #f))
    (test-equal "system services combined"
      '(sa sb sc sd)
      ((feature-system-services-getter super-feature) #f))))
