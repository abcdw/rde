;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
;;;
;;; This file is a part of rde.
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

;;; Packaged by migalmoreno
;;; Source: https://lists.sr.ht/~abcdw/rde-devel/patches/47025
(define-module (contrib packages node)
  #:use-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))


(define-public llhttp-bootstrap-2.1.4
  (package
    (name "llhttp")
    (version "2.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nodejs/llhttp.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "115mwyds9655p76lhglxg2blc1ksgrix6zhigaxnc2q6syy3pa6x"))
              (patches
               (parameterize
                   ((%patch-path
                     (map (lambda (directory)
                            (string-append directory "/contrib/packages/patches"))
                          %load-path)))
                 (search-patches "llhttp-bootstrap-CVE-2020-8287.patch")))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Fix imports for esbuild.
                  ;; https://github.com/evanw/esbuild/issues/477
                  (substitute* "src/llhttp/http.ts"
                    (("\\* as assert") "assert"))
                  (substitute* "Makefile"
                    (("npx ts-node bin/generate.ts")
                     "node bin/generate.js"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "CLANG=" ,(cc-for-target))
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((esbuild (search-input-file (or native-inputs inputs)
                                               "/bin/esbuild")))
               (invoke esbuild
                       "--platform=node"
                       "--outfile=bin/generate.js"
                       "--bundle" "bin/generate.ts"))))
         (add-before 'install 'create-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (dir)
                           (mkdir-p (string-append out dir)))
                         (list "/lib" "/include" "/src"))
               #t)))
         (add-after 'install 'install-src
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src-dir (string-append out "/src")))
               (install-file "build/c/llhttp.c" src-dir)
               (install-file "src/native/api.c" src-dir)
               (install-file "src/native/http.c" src-dir)
               #t))))))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("node" ,node-bootstrap)
       ("node-semver" ,node-semver-bootstrap)
       ("node-llparse-bootstrap" ,node-llparse-bootstrap)))
    (home-page "https://github.com/nodejs/llhttp")
    (properties '((hidden? . #t)))
    (synopsis "Parser for HTTP messages")
    (description "This is a rewrite of
@url{https://github.com/nodejs/http-parser, http-parser} using
@url{https://github.com/nodejs/llparse, llparse} to generate the C
source files.")
    (license license:expat)))

(define-public node-stable
  (package
    (inherit node)
    (version "14.19.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.xz"))
              (sha256
               (base32
                "15691j5zhiikyamiwwd7f282g6d9acfhq91nrwx54xya38gmpx2w"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bundled software, where possible
                  (for-each delete-file-recursively
                            '("deps/cares"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/openssl"
                              "deps/zlib"))
                  (substitute* "Makefile"
                    ;; Remove references to bundled software.
                    (("deps/uv/uv.gyp") "")
                    (("deps/zlib/zlib.gyp") ""))
                  #t))))
    (arguments
     (append (list #:tests? #f)
         (substitute-keyword-arguments (package-arguments node)
           ((#:configure-flags configure-flags)
            ''("--shared-cares"
               "--shared-libuv"
               "--shared-nghttp2"
               "--shared-openssl"
               "--shared-zlib"
               "--shared-brotli"
               "--with-intl=system-icu"))
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'set-bootstrap-host-rpath
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   (let* ((inputs        (or native-inputs inputs))
                          (c-ares        (assoc-ref inputs "c-ares"))
                          (brotli        (assoc-ref inputs "brotli"))
                          (icu4c         (assoc-ref inputs "icu4c"))
                          (nghttp2       (assoc-ref inputs "nghttp2"))
                          (openssl       (assoc-ref inputs "openssl"))
                          (libuv         (assoc-ref inputs "libuv"))
                          (zlib          (assoc-ref inputs "zlib"))
                          (host-binaries '("torque"
                                           "bytecode_builtins_list_generator"
                                           "gen-regexp-special-case"
                                           "node_mksnapshot"
                                           "mksnapshot")))
                     (substitute* '("node.gyp" "tools/v8_gypfiles/v8.gyp")
                       (((string-append "'target_name': '("
                                        (string-join host-binaries "|")
                                        ")',")
                         target)
                        (string-append target
                                       "'ldflags': ['-Wl,-rpath="
                                       c-ares "/lib:"
                                       brotli "/lib:"
                                       icu4c "/lib:"
                                       nghttp2 "/lib:"
                                       openssl "/lib:"
                                       libuv "/lib:"
                                       zlib "/lib"
                                       "'],"))))))
               (delete 'patch-additional-hardcoded-program-references)
               (replace 'delete-problematic-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; FIXME: These tests fail in the build container, but they don't
                   ;; seem to be indicative of real problems in practice.
                   (for-each delete-file
                             '("test/parallel/test-cluster-master-error.js"
                               "test/parallel/test-cluster-master-kill.js"))

                   ;; These require a DNS resolver.
                   (for-each delete-file
                             '("test/parallel/test-dns.js"
                               "test/parallel/test-dns-lookupService-promises.js"))

                   ;; These tests require networking.
                   (for-each delete-file
                             '("test/parallel/test-https-agent-unref-socket.js"
                               "test/parallel/test-corepack-yarn-install.js"))

                   ;; This test is timing-sensitive, and fails sporadically on
                   ;; slow, busy, or even very fast machines.
                   (delete-file "test/parallel/test-fs-utimes.js")

                   ;; FIXME: This test fails randomly:
                   ;; https://github.com/nodejs/node/issues/31213
                   (delete-file "test/parallel/test-net-listen-after-destroying-stdin.js")

                   ;; FIXME: These tests fail on armhf-linux:
                   ;; https://github.com/nodejs/node/issues/31970
                   ,@(if (target-arm32?)
                         '((for-each delete-file
                                     '("test/parallel/test-zlib.js"
                                       "test/parallel/test-zlib-brotli.js"
                                       "test/parallel/test-zlib-brotli-flush.js"
                                       "test/parallel/test-zlib-brotli-from-brotli.js"
                                       "test/parallel/test-zlib-brotli-from-string.js"
                                       "test/parallel/test-zlib-convenience-methods.js"
                                       "test/parallel/test-zlib-random-byte-pipes.js"
                                       "test/parallel/test-zlib-write-after-flush.js")))
                         '())

                   ;; These tests have an expiry date: they depend on the validity of
                   ;; TLS certificates that are bundled with the source.  We want this
                   ;; package to be reproducible forever, so remove those.
                   ;; TODO: Regenerate certs instead.
                   (for-each delete-file
                             '("test/parallel/test-tls-passphrase.js"
                               "test/parallel/test-tls-server-verify.js"))))
               (add-after 'delete-problematic-tests 'replace-llhttp-sources
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Replace pre-generated llhttp sources
                   (let ((llhttp (assoc-ref inputs "llhttp")))
                     (copy-file (string-append llhttp "/src/llhttp.c")
                                "deps/llhttp/src/llhttp.c")
                     (copy-file (string-append llhttp "/src/api.c")
                                "deps/llhttp/src/api.c")
                     (copy-file (string-append llhttp "/src/http.c")
                                "deps/llhttp/src/http.c")
                     (copy-file (string-append llhttp "/include/llhttp.h")
                                "deps/llhttp/include/llhttp.h")))))))))
    (native-inputs
     (list ;; Runtime dependencies for binaries used as a bootstrap.
      c-ares
      brotli
      icu4c
      libuv-for-node
      `(,nghttp2 "lib")
      openssl-1.1
      zlib
      ;; Regular build-time dependencies.
      perl
      pkg-config
      procps
      python
      util-linux))
    (inputs
     (list bash-minimal
           coreutils
           c-ares
           icu4c
           libuv-for-node
           llhttp-bootstrap-2.1.4
           brotli
           `(,nghttp2 "lib")
           openssl-1.1
           zlib))))
