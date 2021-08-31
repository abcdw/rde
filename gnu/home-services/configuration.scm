;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu home-services configuration)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)

  #:export (filter-configuration-fields

            interpose
            list-of

            list-of-strings?
            alist?
            string-or-gexp?
	    serialize-string-or-gexp
	    text-config?
            serialize-text-config
            generic-serialize-alist-entry
            generic-serialize-alist))

(define* (filter-configuration-fields configuration-fields fields
				      #:optional negate?)
  "Retrieve the fields listed in FIELDS from CONFIGURATION-FIELDS.
If NEGATE? is @code{#t}, retrieve all fields except FIELDS."
  (filter (lambda (field)
            (let ((member? (member (configuration-field-name field) fields)))
              (if (not negate?) member? (not member?))))
          configuration-fields))


(define* (interpose ls  #:optional (delimiter "\n") (grammar 'infix))
  "Same as @code{string-join}, but without join and string, returns an
DELIMITER interposed LS.  Support 'infix and 'suffix GRAMMAR values."
  (when (not (member grammar '(infix suffix)))
    (raise
     (formatted-message
      (G_ "The GRAMMAR value must be 'infix or 'suffix, but ~a provided.")
      grammar)))
  (fold-right (lambda (e acc)
		(cons e
		      (if (and (null? acc) (eq? grammar 'infix))
			  acc
			  (cons delimiter acc))))
	      '() ls))

(define (list-of pred?)
  "Return a procedure that takes a list and check if all the elements of
the list result in @code{#t} when applying PRED? on them."
    (lambda (x)
      (if (list? x)
          (every pred? x)
          #f)))


(define list-of-strings?
  (list-of string?))

(define alist? list?)

(define (string-or-gexp? sg) (or (string? sg) (gexp? sg)))
(define (serialize-string-or-gexp field-name val) "")

(define (text-config? config)
  (and (list? config) (every string-or-gexp? config)))
(define (serialize-text-config field-name val)
  #~(string-append #$@(interpose val "\n" 'suffix)))

(define ((generic-serialize-alist-entry serialize-field) entry)
  "Apply the SERIALIZE-FIELD procedure on the field and value of ENTRY."
  (match entry
    ((field . val) (serialize-field field val))))

(define (generic-serialize-alist combine serialize-field fields)
  "Generate a configuration from an association list FIELDS.

SERIALIZE-FIELD is a procedure that takes two arguments, it will be
applied on the fields and values of FIELDS using the
@code{generic-serialize-alist-entry} procedure.

COMBINE is a procedure that takes one or more arguments and combines
all the alist entries into one value, @code{string-append} or
@code{append} are usually good candidates for this.

See the @code{serialize-alist} procedure in `@code{(gnu home-services
version-control}' for an example usage.)}"
  (apply combine
         (map (generic-serialize-alist-entry serialize-field) fields)))
