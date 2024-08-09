;;; rde --- Reproducible development environment.
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>

(define-module (rde exceptions)
  #:use-module (ice-9 exceptions)
  #:export (config-exception))

(define-exception-type
  &config-exception &exception %make-config-exception config-exception?)

(define (config-exception message irritants)
  (make-exception
   (%make-config-exception)
   (make-exception-with-message message)
   (make-exception-with-irritants irritants)))

;; (raise-exception (config-exception "Something unexpected with ~a" (list 1)))
