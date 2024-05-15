(use-modules (guix ci)
             (guix channels)
             (srfi srfi-1))

(define channels
  (load "channels-lock.scm"))

(define channels-with-local-rde
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
   (remove
    (lambda (c) (equal? (channel-name c) 'rde))
    channels)))

channels-with-local-rde
