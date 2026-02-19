;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rde-configs env guix channels-ci-latest-guix)
  #:use-module (guix channels))

(define core-channels-with-local-rde-and-latest-guix
  (list
   (channel
    (name 'rde)
    ;; RDE source code with patches applied will be located here
    (url (string-append "file://" (getenv "HOME") "/rde"))
    (introduction
     (make-channel-introduction
      "257cebd587b66e4d865b3537a9a88cccd7107c95"
      (openpgp-fingerprint
       "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
   (channel
    (name 'guix)
    (url "https://git.guix.gnu.org/guix.git")
    (branch "master")
    (introduction
     (make-channel-introduction
      "9edb3f66fd807b096b48283debdcddccfea34bad"
      (openpgp-fingerprint
       "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

core-channels-with-local-rde-and-latest-guix
