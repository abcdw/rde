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

(define-module (guix scripts home import)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (import-manifest))

;;; Commentary:
;;;
;;; This module provides utilities for generating home service
;;; configurations from existing "dotfiles".
;;;
;;; Code:


(define (generate-bash-module+configuration)
  (let ((rc (string-append (getenv "HOME") "/.bashrc"))
        (profile (string-append (getenv "HOME") "/.bash_profile"))
        (logout (string-append (getenv "HOME") "/.bash_logout")))
    `((gnu home-services bash)
      (service home-bash-service-type
                 (home-bash-configuration
                  ,@(if (file-exists? rc)
                        `((bashrc
                           (list (slurp-file-gexp (local-file ,rc)))))
                        '())
                  ,@(if (file-exists? profile)
                        `((bash-profile
                           (list (slurp-file-gexp
                                  (local-file ,profile)))))
                        '())
                  ,@(if (file-exists? logout)
                        `((bash-logout
                           (list (slurp-file-gexp
                                  (local-file ,logout)))))
                        '()))))))


(define %files-configurations-alist
  `((".bashrc" . ,generate-bash-module+configuration)
    (".bash_profile" . ,generate-bash-module+configuration)
    (".bash_logout" . ,generate-bash-module+configuration)))

(define (modules+configurations)
  (let ((configurations (delete-duplicates
                         (filter-map (match-lambda
                                ((file . proc)
                                 (if (file-exists?
                                      (string-append (getenv "HOME") "/" file))
                                     proc
                                     #f)))
                                     %files-configurations-alist)
                         (lambda (x y)
                           (equal? (procedure-name x) (procedure-name y))))))
    (map (lambda (proc) (proc)) configurations)))

;; Based on `manifest->code' from (guix profiles)
;; MAYBE: Upstream it?
(define* (manifest->code manifest
                         #:key
                         (entry-package-version (const ""))
                         (home-environment? #f))
  "Return an sexp representing code to build an approximate version of
MANIFEST; the code is wrapped in a top-level 'begin' form.  If
HOME-ENVIRONMENT? is #t, return an <home-environment> definition.
Call ENTRY-PACKAGE-VERSION to determine the version number to use in
the spec for a given entry; it can be set to 'manifest-entry-version'
for fully-specified version numbers, or to some other procedure to
disambiguate versions for packages for which several versions are
available."
  (define (entry-transformations entry)
    ;; Return the transformations that apply to ENTRY.
    (assoc-ref (manifest-entry-properties entry) 'transformations))

  (define transformation-procedures
    ;; List of transformation options/procedure name pairs.
    (let loop ((entries (manifest-entries manifest))
               (counter 1)
               (result  '()))
      (match entries
        (() result)
        ((entry . tail)
         (match (entry-transformations entry)
           (#f
            (loop tail counter result))
           (options
            (if (assoc-ref result options)
                (loop tail counter result)
                (loop tail (+ 1 counter)
                      (alist-cons options
                                  (string->symbol
                                   (format #f "transform~a" counter))
                                  result)))))))))

  (define (qualified-name entry)
    ;; Return the name of ENTRY possibly with "@" followed by a version.
    (match (entry-package-version entry)
      (""      (manifest-entry-name entry))
      (version (string-append (manifest-entry-name entry)
                              "@" version))))

  (if (null? transformation-procedures)
      (let ((specs (map (lambda (entry)
                          (match (manifest-entry-output entry)
                            ("out"  (qualified-name entry))
                            (output (string-append (qualified-name entry)
                                                   ":" output))))
                        (manifest-entries manifest))))
        (if home-environment?
            (let ((modules+configurations (modules+configurations)))
              `(begin
               (use-modules (gnu home)
                            (gnu packages)
                            ,@(map first modules+configurations))
               ,(home-environment-template
                 #:specs specs
                 #:services (map second modules+configurations))))
            `(begin
               (use-modules (gnu packages))

               (specifications->manifest
                (list ,@specs)))))
      (let* ((transform (lambda (options exp)
                         (if (not options)
                             exp
                             (let ((proc (assoc-ref transformation-procedures
                                                    options)))
                               `(,proc ,exp)))))
            (packages (map (lambda (entry)
                                   (define options
                                     (entry-transformations entry))

                                   (define name
                                     (qualified-name entry))

                                   (match (manifest-entry-output entry)
                                     ("out"
                                      (transform options
                                                 `(specification->package ,name)))
                                     (output
                                      `(list ,(transform
                                               options
                                               `(specification->package ,name))
                                             ,output))))
                           (manifest-entries manifest)))
            (transformations (map (match-lambda
                         ((options . name)
                          `(define ,name
                             (options->transformation ',options))))
                       transformation-procedures)))
        (if home-environment?
            (let ((modules+configurations (modules+configurations)))
              `(begin
                 (use-modules (guix transformations)
                              (gnu home)
                              (gnu packages)
                              ,@(map first modules+configurations))

                 ,@transformations

                 ,(home-environment-template
                   #:packages packages
                   #:services (map second modules+configurations))))
            `(begin
               (use-modules (guix transformations)
                            (gnu packages))

                ,@transformations

                (packages->manifest
                 (list ,@packages)))))))

(define* (home-environment-template #:key (packages #f) (specs #f) services)
  "Return an S-exp containing a <home-environment> declaration
containing PACKAGES, or SPECS (package specifications), and SERVICES."
  `(home-environment
     (packages
      ,@(if packages
            `((list ,@packages))
            `((map specification->package
                   (list ,@specs)))))
     (services (list ,@services))))

(define* (import-manifest
          manifest
          #:optional (port (current-output-port)))
  "Write to PORT a <home-environment> corresponding to MANIFEST."
  (define (version-spec entry)
    (let ((name (manifest-entry-name entry)))
      (match (map package-version (find-packages-by-name name))
        ((_)
         ;; A single version of NAME is available, so do not specify the
         ;; version number, even if the available version doesn't match ENTRY.
         "")
        (versions
         ;; If ENTRY uses the latest version, don't specify any version.
         ;; Otherwise return the shortest unique version prefix.  Note that
         ;; this is based on the currently available packages, which could
         ;; differ from the packages available in the revision that was used
         ;; to build MANIFEST.
         (let ((current (manifest-entry-version entry)))
           (if (every (cut version>? current <>)
                      (delete current versions))
               ""
               (version-unique-prefix (manifest-entry-version entry)
                                      versions)))))))

  (match (manifest->code manifest
                         #:entry-package-version version-spec
                         #:home-environment? #t)
    (('begin exp ...)
     (format port (G_ "\
;; This \"home-environment\" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is \"symbolic\": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by \"guix describe\".
;; See the \"Replicating Guix\" section in the manual.\n"))
     (for-each (lambda (exp)
                 (newline port)
                 (pretty-print exp port))
               exp))))
