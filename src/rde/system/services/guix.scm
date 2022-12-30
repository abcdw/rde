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

(define-module (rde system services guix)
  #:use-module (gnu services shepherd)

  #:use-module (guix gexp)
  #:use-module (guix modules)

  #:export (cow-store-service-type))


;;;
;;; COW store.
;;;

(define %backing-directory
  ;; Sub-directory used as the backing store for copy-on-write.
  "/tmp/guix-inst")

(define cow-store-service-type
  (shepherd-service-type
   'cow-store
   (lambda _
     (define (import-module? module)
       ;; Since we don't use deduplication support in 'populate-store', don't
       ;; import (guix store deduplication) and its dependencies, which
       ;; includes Guile-Gcrypt.
       (and (guix-module-name? module)
            (not (equal? module '(guix store deduplication)))))

     (shepherd-service
      (requirement '(root-file-system user-processes))
      (provision '(cow-store))
      (documentation
       "Make the store copy-on-write, with writes going to \
the given target.")

      ;; This is meant to be explicitly started by the user.
      (auto-start? #f)

      (modules `((gnu build install)
                 ,@%default-modules))
      (start
       (with-imported-modules (source-module-closure
                               '((gnu build install))
                               #:select? import-module?)
         #~(case-lambda
             ((target)
              (mount-cow-store target #$%backing-directory)
              target)
             (else
              ;; Do nothing, and mark the service as stopped.
              #f))))
      (stop #~(lambda (target)
                ;; Delete the temporary directory, but leave everything
                ;; mounted as there may still be processes using it since
                ;; 'user-processes' doesn't depend on us.  The 'user-file-systems'
                ;; service will unmount TARGET eventually.
                (delete-file-recursively
                 (string-append target #$%backing-directory))))))
   #f
   (description "Make the store copy-on-write, with writes going to \
the given target.")))
