;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde transitive elixir)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system rebar)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module (gnu packages elixir)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match))

;; TODO: [Andrew Tropin, 2024-02-21] Define mixlocktosexp package

(define mixlocktosexp-package
  (package
    (name "mixlocktosexp")
    (version "0.1.0")
    (source
     (local-file (%search-load-path "rde/transitive/elixir/mixlocktosexp.ex")
                 ;; peserve executable bit
                 #:recursive? #t))
    (arguments (list
                #:patch-shebangs? #t
                #:install-plan #~`(("mixlocktosexp.ex" "bin/mixlocktosexp"))))
    (build-system copy-build-system)
    (inputs (list elixir))
    (home-page "https://trop.in/rde")
    (license #f)
    (synopsis "Transform @file{mix.lock} into s-expressions")
    (description "Transform @file{mix.lock} into s-expressions.")))

(define (mixlocktosexp-binary)
  (string-append
   (car ((@ (rde api store) build-with-store) mixlocktosexp-package))
   "/bin/mixlocktosexp"))

(define (mixlocktosexp file)
 (let* ((pipe (open-pipe* OPEN_READ (mixlocktosexp-binary) file))
        (result (read pipe)))
   (close-pipe pipe)
   result))

(define (base16->nix-base32 hash)
  (bytevector->nix-base32-string
   (base16-string->bytevector hash)))
(define* (get-package-sexp
          #:key
          name (hex-name name) version hash build-system (dependencies '())
          (cc? #f)
          (source `(origin
                     (method url-fetch)
                     (uri ,(hexpm-uri hex-name version))
                     (sha256 (base32 ,hash)))))
  `(define-public ,(string->symbol name)
     (package
       (name ,name)
       (version ,version)
       (source ,source)
       (build-system ,build-system)
       (arguments (list #:tests? #f
                        ,@(if cc?
                              `(#:phases
                                #~(modify-phases %standard-phases
                                    (add-before 'build 'set-CC
                                      (lambda _
                                        (setenv "CC" "gcc")))))
                              '())))
       (inputs (list ,@dependencies))
       (synopsis ,(format #f "~a package" name))
       (description ,(format #f "~a package." name))
       (home-page "")
       (license #f))))

(define (mix-sexp->package-sexp lock-line)
  (match lock-line
    ((name . #("git" url commit #(deps ...)))
     (get-package-sexp
      #:name name
      #:version (git-version "0.1.0" "0" commit)
      #:source `(git-checkout
                 (url ,url)
                 (commit ,commit))
      #:build-system 'mix-build-system
      #:dependencies (map string->symbol deps)))
    ((name . #(build hex-name version hash #(deps ...)))
     (get-package-sexp
      #:name name
      #:hex-name hex-name
      #:version version
      #:hash (base16->nix-base32 hash)
      #:build-system (if (string= "mix" build)
                         'mix-build-system
                         'rebar-build-system)
      #:dependencies (map string->symbol deps)))))

;; (define pkgs (mixlocktosexp (%search-load-path "rde/transitive/elixir/mix.lock")))

(define preamble
  "(define-module (PROJECT packages PROJECT-deps)
  #:use-module (guix build-system mix)
  #:use-module (guix build-system rebar)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages))

")

(define (go)
  (call-with-output-file "PROJECT-deps.scm"
    (lambda (port)
      (format port preamble)
      (for-each (lambda (x)
                  (format port "~y\n"
                          (mix-sexp->package-sexp x))) (reverse pkgs)))))
