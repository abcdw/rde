;;; rde --- Reproducible development environment.
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>


(define-module (rde-configs env dev packages)
  #:use-module (rde lib file)
  #:use-module (rde-configs env guix channels)
  #:use-module (rde packages guix)
  #:export (guix-package
            channels-package))

(define guix-package
  (make-guix-package core-channels))

(define channels-package
  (make-channels-package core-channels))
