(define-module (rde packages messaging)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages php)
  #:use-module (gnu packages tls)

  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public tdlib-latest
  (let ((commit "047246f366d94b55ea7b6b93b3a4baa9a3380154")
        (revision "0"))
    (package
      (name "tdlib")
      (version (git-version "1.8.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tdlib/td")
               (commit commit)))
         (sha256
          (base32 "0iy6c6937schr8yd4ni3zc16gcp1ighmf69d5p0b23lrq2b9jq5k"))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f
                  #:configure-flags
                  (list "-DCMAKE_BUILD_TYPE=Release"
                        "-DTD_ENABLE_LTO=OFF")     ; FIXME: Get LTO to work.
                  #:phases
                  (modify-phases %standard-phases
                    (add-after 'unpack 'remove-failing-tests
                      (lambda _
                        (substitute* "test/CMakeLists.txt"
                          ;; The test cases are compiled into a distinct binary
                          ;; which uses mtproto.cpp to attempt to connect to
                          ;; a remote server. Removing this file from the sources
                          ;; list disables those specific test cases.
                          (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/mtproto.cpp") "")))))))
      (native-inputs
       (list gperf openssl zlib php doxygen))
      (synopsis "Cross-platform library for building Telegram clients")
      (description "Tdlib is a cross-platform library for creating custom
Telegram clients following the official Telegram API.  It can be easily used
from almost any programming language with a C-FFI and features first-class
support for high performance Telegram Bot creation.")
      (home-page "https://core.telegram.org/tdlib")
      (license license:boost1.0))))
