;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rde packages guix)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages guile)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system guile)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1)
  #:export (make-guix-package
            make-channels-package)
  #:declarative? #f)

(define (get-guix-channel channels)
  (car
   (filter (lambda (x) (equal? (channel-name x) 'guix)) channels)))

(define (make-guix-package channels)
  (let ((commit (channel-commit (get-guix-channel channels))))
    (package
      (inherit guix)
      (version
       (string-append (car (string-split (package-version guix) #\-))
                      "-" (string-take commit 7)))
      (source
       (git-checkout
        (url "https://codeberg.org/guix/guix-mirror")
        (commit commit)))
      (arguments
       (substitute-keyword-arguments (package-arguments guix)
         ((#:tests? _)
          #f)
         ((#:phases phases)
          #~(modify-phases #$phases (delete 'check)))
         ((#:configure-flags flags #~'())
          #~(append
             #$flags
             (list
              #$(string-append "--with-channel-commit=" commit))))))

      (inputs (modify-inputs (package-inputs guix)
                (replace "guile" guile-next))))))

(define (channel->git-checkout channel)
  (git-checkout
   (url (channel-url channel))
   (commit (channel-commit channel))))

(define* (channels-union name channels
                         #:key
                         (quiet? #f)
                         (resolve-collision 'resolve-collision/default))
  "Return a directory that is the union of CHANNELS sources."
  (define log-port
    (if quiet?
        (gexp (%make-void-port "w"))
        (gexp (current-error-port))))

  (computed-file
   name
   (with-imported-modules '((guix build union))
     (gexp
      (begin
        (use-modules (guix build union)
                     (srfi srfi-1)) ;for 'first' and 'last'

        (define (thing->srcs thing)
          (with-input-from-file (string-append thing "/.guix-channel")
            (lambda ()
              (let ((dirs (assoc-ref (cdr (read)) 'directory)))
                (if dirs
                    (map (lambda (x) (string-append thing "/" x)) dirs)
                    (list thing))))))

        (union-build (ungexp output)
                     (append-map thing->srcs '#$channels)

                     #:log-port (ungexp log-port)
                     #:symlink symlink
                     #:resolve-collision
                     (ungexp resolve-collision)))))))

(define (channels->combined-source-code channels)
  (channels-union
   "channels-sources"
   (map channel->git-checkout channels)))

(define (make-channels-package channels)
  (package
    (name "channels")
    (version "0.1.0")
    (source (channels->combined-source-code
             (remove guix-channel? channels)))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "."
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (mkdir "source")
              (chdir "source")
              (copy-recursively source "."
                                #:keep-mtime? #t
                                #:follow-symlinks? #t)
              (for-each (lambda (f)
                          (false-if-exception (make-file-writable f)))
                        (find-files ".")))))))
    (inputs `(("guile" ,guile-next)
              ("guix" ,(make-guix-package channels))))
    (home-page "https://git.sr.ht/~abcdw/rde")
    (synopsis "Combined package for channel source and bytecode files")
    (description "Combined package for channel source and bytecode files.")
    (license license:gpl3+)))
