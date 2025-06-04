;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rde-configs env guix channels-ci)
  #:use-module (guix channels)
  #:use-module ((rde env guix channels) #:prefix rde:)
  #:use-module (srfi srfi-1))

(define core-channels-with-local-rde
  (cons
   (channel
    (name 'rde)
    ;; RDE source code with patches applied will be located here
    (url (string-append "file://" (getenv "HOME") "/rde"))
    (introduction
     (make-channel-introduction
      "257cebd587b66e4d865b3537a9a88cccd7107c95"
      (openpgp-fingerprint
       "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
   rde:core-channels))

core-channels-with-local-rde
