;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
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

(define-module (gnu home-services utils)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (maybe-object->string
            object->snake-case-string
            object->camel-case-string))

(define (maybe-object->string object)
  "Like @code{object->string} but don't do anyting if OBJECT already is
a string."
  (if (string? object)
      object
      (object->string object)))

;; Snake case: <https://en.wikipedia.org/wiki/Snake_case>
(define* (object->snake-case-string object #:optional (style 'lower))
  "Convert the object OBJECT to the equivalent string in ``snake
case''.  STYLE can be three `@code{lower}', `@code{upper}', or
`@code{capitalize}', defaults to `@code{lower}'.

@example
(object->snake-case-string 'variable-name 'upper)
@result{} \"VARIABLE_NAME\" @end example"
  (if (not (member style '(lower upper capitalize)))
      (error 'invalid-style (format #f "~a is not a valid style" style))
      (let ((stringified (maybe-object->string object)))
        (string-replace-substring
         (cond
          ((equal? style 'lower) stringified)
          ((equal? style 'upper) (string-upcase stringified))
          (else (string-capitalize stringified)))
         "-" "_"))))

(define* (object->camel-case-string object #:optional (style 'lower))
  "Convert the object OBJECT to the equivalent string in ``camel case''.
STYLE can be three `@code{lower}', `@code{upper}', defaults to
`@code{lower}'.

@example
(object->camel-case-string 'variable-name 'upper)
@result{} \"VariableName\"
@end example"
  (if (not (member style '(lower upper)))
      (error 'invalid-style (format #f "~a is not a valid style" style))
      (let ((stringified (maybe-object->string object)))
        (cond
         ((eq? style 'upper)
          (string-concatenate
           (map string-capitalize
                (string-split stringified (cut eqv? <> #\-)))))
         ((eq? style 'lower)
          (let ((splitted-string (string-split stringified (cut eqv? <> #\-))))
            (string-concatenate
             (cons (first splitted-string)
                   (map string-capitalize
                        (cdr splitted-string))))))))))
