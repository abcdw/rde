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

(define-module (rde system services admin)
  #:use-module (rde serializers utils)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages admin)
  #:use-module (gnu system shadow)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (sudoers-service-type))


;;;
;;; Sudoers.
;;;

(define default-sudoers-content
  "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL\n")

(define (validated-sudoers-file file)
  "Return a copy of FILE, a sudoers file, after checking that it is
syntactically correct."
  (computed-file "sudoers"
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils))

                       (invoke #+(file-append sudo "/sbin/visudo")
                               "--check" "--file" #$file)
                       (copy-file #$file #$output)))))

(define (sudoers-file config)
  `(("sudoers"
     ,(validated-sudoers-file
       (mixed-text-file
        "sudoers"
        (serialize-gexp-text-config #f config))))))

(define sudoers-service-type
  (service-type (name 'sudoers)
                (extensions
                 (list (service-extension etc-service-type sudoers-file)))
                (default-value (list default-sudoers-content))
                (compose concatenate)
                (extend append)
                (description
                 "Manage the content of @file{/etc/sudoers}.")))
