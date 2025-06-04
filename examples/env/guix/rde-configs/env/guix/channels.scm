;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rde-configs env guix channels)
  #:use-module ((rde env guix channels) #:prefix rde:)
  #:use-module (guix channels)
  #:export (core-channels))

(define core-channels
  (cons
   (channel
    (name 'rde)
    (url "https://git.sr.ht/~abcdw/rde")
    (branch "master")
    (commit
     "5669a5f2e729635746bd9015e50bd9e3abe14b3a")
    (introduction
     (make-channel-introduction
      "257cebd587b66e4d865b3537a9a88cccd7107c95"
      (openpgp-fingerprint
       "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
   rde:core-channels))

core-channels
