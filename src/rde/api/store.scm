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

(define-module (rde api store)
  #:use-module (ice-9 match)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix status)
  #:use-module (guix derivations)
  #:export (build-with-store
            eval-with-store))

(define %build-verbosity 10)

(define* (default-guile-derivation store #:optional (system (%current-system)))
  "Return the derivation of the default "
  (package-derivation store (default-guile) system))

(define* (evaluate-with-store mvalue #:key build?)
  "Run monadic value MVALUE in the store monad and print its value."
  (with-store store
    (set-build-options store
                       #:print-build-trace #t
                       #:print-extended-build-trace? #t
                       #:multiplexed-build-output? #t)
    (with-status-verbosity %build-verbosity
      (let* ((guile  (or (%guile-for-build)
                         (default-guile-derivation store)))
             (values (run-with-store store
                       (if build?
                           (mlet %store-monad ((obj mvalue))
                             (if (derivation? obj)
                                 (mbegin %store-monad
                                   (built-derivations (list obj))
                                   (return
                                    (match (derivation->output-paths obj)
                                      (((_ . files) ...) files))))
                                 (return (list obj))))
                           (mlet %store-monad ((obj mvalue))
                             (return (list obj))))
                       #:guile-for-build guile)))
        values))))

(define-syntax-rule (save-load-path-excursion body ...)
  "Save the current values of '%load-path' and '%load-compiled-path', run
BODY..., and restore them."
  (let ((path %load-path)
        (cpath %load-compiled-path))
    (dynamic-wind
      (const #t)
      (lambda ()
        body ...)
      (lambda ()
        (set! %load-path path)
        (set! %load-compiled-path cpath)))))

(define (local-eval exp)
  "Evaluate EXP, a G-Expression, in-place."
  (mlet* %store-monad ((lowered (lower-gexp exp))
                       (_ (built-derivations (lowered-gexp-inputs lowered))))
    (save-load-path-excursion
     (set! %load-path (lowered-gexp-load-path lowered))
     (set! %load-compiled-path (lowered-gexp-load-compiled-path lowered))
     (return (primitive-eval (lowered-gexp-sexp lowered))))))

(define (build-with-store obj)
  (evaluate-with-store (lower-object obj) #:build? #t))

(define (eval-with-store gexp)
  (car (evaluate-with-store (local-eval gexp))))
