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

(define-module (rde serializers lisp-test)
  #:use-module (srfi srfi-64)
  #:use-module (rde tests)
  #:use-module (rde api store)

  #:use-module (rde serializers lisp)

  #:use-module (guix gexp)
  #:use-module (guix read-print))

;; TODO: Figure out the problem and remove this import
;; WHAT THE HECK? Tests start failing without this use-modules.
(use-modules (ice-9 pretty-print))

(define (serialize-config config)
  "Returns a string representing serialized config."
  (eval-with-store (serialize-sexp-config #f config)))

(define (serialize-and-read-config config)
  "Returns a list of forms read from serialized config."
  (call-with-input-string (serialize-config config)
    (lambda (port) (read-with-comments/sequence port))))

(define-test basic-types
  (test-group "basic types"
    (test-equal "symbols"
      '(hello there)
      (serialize-and-read-config `(hello there)))

    (test-equal "numbers"
      '(1 1/3 4.5)
      (serialize-and-read-config `(1 1/3 4.5)))

    (test-equal "booleans"
      '(#f #t)
      (serialize-and-read-config `(#f #t)))

    (test-equal "strings"
      '("hello" "there")
      (serialize-and-read-config `("hello" "there")))))

(define-test reader-macros
  (test-group "reader macros"
    (test-equal "quote"
      "'(hello there)\n"
      (serialize-config `('(hello there))))

    (test-equal "quasiquote and unquote"
      "`(hello ,there)\n"
      (serialize-config `(`(hello ,there))))

    ;; TODO: Move it elisp serializer test
    (test-expect-fail 1)
    (test-equal "square brackets"
      "[hello there]\n"
      (serialize-config `([hello there])))

    (test-expect-fail 1)
    ;; We should update pretty-printer used in serialization code or workaround
    ;; it some other way.  Now, it's impossible to use this construction with
    ;; emacs-lisp.
    (test-equal "syntax"
      "#'symbol\n"
      (serialize-config `(#'symbol)))))

(define-test gexps
  (test-group "gexps"
    (test-equal "newlines and comments"
      "
;;; Hello there

;; more comments here
"
      (serialize-config `(,#~""
                          ,#~";;; Hello there"
                          ,#~""
                          ,#~";; more comments here")))

    (test-equal "top-level gexp eval"
      ";; hello there\n"
      (serialize-config `(,#~(format #f ";; hello there"))))

    (test-expect-fail 1)
    ;; Due to incomplete implementation of the serializer this test fails, more
    ;; accurate implementation should pass this test.
    (test-equal "nested gexp eval"
      "(message \"message\")\n"
      (serialize-config `((message ,#~(format #f "~s" "message")))))))
