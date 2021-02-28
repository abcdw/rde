(define-module (gnu home-services files)
  #:use-module (gnu home-services)
  #:use-module (gnu services)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix sets)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (home-files-service-type))

(define-public (files->files-directory files)
  "Return a @code{files} directory that contains FILES."
  (define (assert-no-duplicates files)
    (let loop ((files files)
               (seen (set)))
      (match files
        (() #t)
        (((file _) rest ...)
         (when (set-contains? seen file)
           (raise (formatted-message (G_ "duplicate '~a' entry for files/")
                                     file)))
         (loop rest (set-insert file seen))))))

  ;; Detect duplicates early instead of letting them through, eventually
  ;; leading to a build failure of "files.drv".
  (assert-no-duplicates files)

  (file-union "files" files))

(define (files-entry files)
  "Return an entry for the @file{~/.guix-home-environment/files}
directory containing FILES."
  (with-monad %store-monad
    (return `(("files" ,(files->files-directory files))))))

(define home-files-service-type
  (service-type (name 'home-files)
                (extensions
                 (list (service-extension home-service-type
                                          files-entry)))
                (compose concatenate)
                (extend append)
		(default-value '())
                (description "Configuration files for programs, stored in
@file{~/.guix-home-environment/files}.")))
