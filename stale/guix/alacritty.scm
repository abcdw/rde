(define-module (alacritty))

(use-modules
 (gnu packages python)
 (gnu packages rust)
 (gnu packages ruby)
 (guix import crate)
 (guix packages)
 (guix git-download)
 (guix download)
 (guix build-system cargo)
 ((guix licenses) #:prefix license:))

(define-public rust-euclid-macros
  (package
   (name "rust-euclid-macros")
   (version "0.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "euclid_macros" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "05dy60mxw2yg26m1ssqd5v7an0wly97rn0r3b8f7l0x5iv0q9jzx"))))
   (build-system cargo-build-system)
   (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
   (home-page "")
   (synopsis "Euclid implementation detail")
   (description "Euclid implementation detail")
   (license #f)))

(define-public rust-mint
  (package
   (name "rust-mint")
   (version "0.5.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "mint" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0z2akkbail867wr8f0hvwwz6fxf90q254q8sx8bm25dh9fdw5rn9"))))
   (build-system cargo-build-system)
   (home-page "https://github.com/kvark/mint")
   (synopsis "Math interoperability standard types")
   (description
    "Math interoperability standard types")
   (license #f)))

(define-public rust-num-traits
  (package
   (name "rust-num-traits")
   (version "0.2.6")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "num-traits" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1qdym9m6sbzna1pq3s21cbjgyjakyjds33xwp7c30vbxr5y5sfhb"))))
   (build-system cargo-build-system)
   (home-page
    "https://github.com/rust-num/num-traits")
   (synopsis
    "Numeric traits for generic mathematics")
   (description
    "Numeric traits for generic mathematics")
   (license #f)))

(define-public rust-euclid
  (package
   (name "rust-euclid")
   (version "0.19.8")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "euclid" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0wqwnp1snvaww1m9lw9vx3gn65jqqv92nk3q6gyd6zm68jjijivs"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-rand" ,rust-rand "src")
      ("rust-serde-test" ,rust-serde-test "src")))
   (inputs
    `(("rust-euclid-macros" ,rust-euclid-macros "src")
      ("rust-mint" ,rust-mint "src")
      ("rust-num-traits" ,rust-num-traits "src")
      ("rust-serde" ,rust-serde "src")))
   (home-page "https://github.com/servo/euclid")
   (synopsis "Geometry primitives")
   (description "Geometry primitives")
   (license #f)))



(define-public
  alacritty
  (package
   (name "alacritty")
   (version "0.3.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/jwilm/alacritty/archive/v" version ".tar.gz"))
            ;; (uri (git-reference
            ;;       (url "https://github.com/jwilm/alacritty.git")
            ;;       (commit (string-append "v" version))))
            (sha256
             (base32 "0iwasbac4zffj0by6yqir1l6cz7krmi2wmkyvh930mgba6knzyb2"))))
   (build-system cargo-build-system)
   (synopsis "A cross-platform, GPU-accelerated terminal emulator ")
   (description "A cross-platform, GPU-accelerated terminal emulator ")
   (home-page "https://github.com/qwilm/alacritty")
   (license license:asl2.0)
   (inputs
    `(("python" ,python)
      ("ruby" ,ruby)
      ("ruste-euclid" ,rust-euclid)))
   ;; (arguments
   ;;  `(#:phases
   ;;    (modify-phases %standard-phases
   ;;                   (replace 'configure
   ;;                            (lambda* (#:key outputs inputs #:allow-other-keys)
   ;;                              ;; add write for user, to prevent a failure in the install phase
   ;;                              (for-each
   ;;                               (lambda (file)
   ;;                                 (let ((stat (stat file)))
   ;;                                   (chmod file (+ #o200 (stat:mode stat)))))
   ;;                               (find-files "." "."))
   ;;                              ))
   ;;                   )))
   ))

(define-public rust-serde
  (package
   (name "rust-serde")
   (version "1.0.90")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "serde" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1x40hvjd60jcgdl3ishnbv0k2yn7z2sklz075jyvlx84h8h7qpxa"))))
   (build-system cargo-build-system)
   ;; (native-inputs
   ;;  `(("rust-serde-derive" ,rust-serde-derive "src")))
   ;; (inputs
   ;; `(("rust-serde-derive" ,rust-serde-derive "src")))
   (home-page "https://serde.rs")
   (synopsis
    "A generic serialization/deserialization framework")
   (description
    "This package provides a generic serialization/deserialization framework")
   (license #f)))

(define-public rust-rand
  (package
   (name "rust-rand")
   (version "0.6.5")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rand" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1jl4449jcl4wgmzld6ffwqj5gwxrp8zvx8w573g1z368qg6xlwbd"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-autocfg" ,rust-autocfg "src")
      ("rust-average" ,rust-average "src")
      ("rust-rand-xoshiro" ,rust-rand-xoshiro "src")))
   (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-log" ,rust-log "src")
      ("rust-packed-simd" ,rust-packed-simd "src")
      ("rust-rand-chacha" ,rust-rand-chacha "src")
      ("rust-rand-core" ,rust-rand-core "src")
      ("rust-rand-hc" ,rust-rand-hc "src")
      ("rust-rand-isaac" ,rust-rand-isaac "src")
      ("rust-rand-jitter" ,rust-rand-jitter "src")
      ("rust-rand-os" ,rust-rand-os "src")
      ("rust-rand-pcg" ,rust-rand-pcg "src")
      ("rust-rand-xorshift" ,rust-rand-xorshift "src")
      ("rust-winapi" ,rust-winapi "src")))
   (home-page "https://crates.io/crates/rand")
   (synopsis
    "Random number generators and other randomness functionality.
")
   (description
    "Random number generators and other randomness functionality.
")
   (license #f)))

(define-public rust-serde-test
  (package
   (name "rust-serde-test")
   (version "1.0.90")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "serde_test" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "02r4vlil53w0j85zba593i24ma0x25n8lxqsf3sffqgs0z97xqbc"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
   (inputs `(("rust-serde" ,rust-serde "src")))
   (home-page "https://serde.rs")
   (synopsis
    "Token De/Serializer for testing De/Serialize implementations")
   (description
    "Token De/Serializer for testing De/Serialize implementations")
   (license #f)))

(define-public rust-libc
  (package
   (name "rust-libc")
   (version "0.2.51")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "libc" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "05v9j6xbxgwzw8qkm5lfaldxlqy1xaz5l17ydx45crh716lcgp5y"))))
   (build-system cargo-build-system)
   (inputs
    `(("rust-rustc-std-workspace-core"
       ,rust-rustc-std-workspace-core
       "src")))
   (home-page "https://github.com/rust-lang/libc")
   (synopsis
    "Raw FFI bindings to platform libraries like libc.
")
   (description
    "Raw FFI bindings to platform libraries like libc.
")
   (license #f)))

(define-public rust-log
  (package
   (name "rust-log")
   (version "0.4.6")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "log" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1ihpib899i38dlgwvyjy3hfkcn1zpqfv0m4p68xah2lm4ysw8kn8"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-serde-test" ,rust-serde-test "src")))
   (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-serde" ,rust-serde "src")))
   (home-page "https://github.com/rust-lang/log")
   (synopsis
    "A lightweight logging facade for Rust
")
   (description
    "This package provides a lightweight logging facade for Rust
")
   (license #f)))
(define-public rust-packed-simd
  (package
   (name "rust-packed-simd")
   (version "0.3.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "packed_simd" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0822wqf6kzw4ig9ykndg348w2bxkhs3x64brzsvdxh2a1pyajpm8"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-arrayvec" ,rust-arrayvec "src")
      ("rust-paste" ,rust-paste "src")
      ("rust-wasm-bindgen" ,rust-wasm-bindgen "src")
      ("rust-wasm-bindgen-test"
       ,rust-wasm-bindgen-test
       "src")))
   (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-core-arch" ,rust-core-arch "src")
      ("rust-sleef-sys" ,rust-sleef-sys "src")))
   (home-page
    "https://github.com/rust-lang-nursery/packed_simd")
   (synopsis "Portable Packed SIMD vectors")
   (description "Portable Packed SIMD vectors")
   (license #f)))
(define-public rust-rand-chacha
  (package
   (name "rust-rand-chacha")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rand_chacha" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1vxwyzs4fy1ffjc8l00fsyygpiss135irjf7nyxgq2v0lqf3lvam"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-autocfg" ,rust-autocfg "src")))
   (inputs
    `(("rust-rand-core" ,rust-rand-core "src")))
   (home-page
    "https://crates.io/crates/rand_chacha")
   (synopsis "ChaCha random number generator
")
   (description "ChaCha random number generator
")
   (license #f)))
(define-public rust-rand-core
  (package
   (name "rust-rand-core")
   (version "0.4.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rand_core" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1h3dbrhi5qgflqnzzd86s48v1dn1l17bmdssi5q170whsm4sbryh"))))
   (build-system cargo-build-system)
   (inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
   (home-page "https://crates.io/crates/rand_core")
   (synopsis
    "Core random number generator traits and tools for implementation.
")
   (description
    "Core random number generator traits and tools for implementation.
")
   (license #f)))
(define-public rust-rand-hc
  (package
   (name "rust-rand-hc")
   (version "0.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rand_hc" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1i0vl8q5ddvvy0x8hf1zxny393miyzxkwqnw31ifg6p0gdy6fh3v"))))
   (build-system cargo-build-system)
   (inputs
    `(("rust-rand-core" ,rust-rand-core "src")))
   (home-page "https://crates.io/crates/rand_hc")
   (synopsis "HC128 random number generator
")
   (description "HC128 random number generator
")
   (license #f)))

(define-public rust-rand-isaac
  (package
   (name "rust-rand-isaac")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rand_isaac" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "027flpjr4znx2csxk7gxb7vrf9c7y5mydmvg5az2afgisp4rgnfy"))))
   (build-system cargo-build-system)
   (native-inputs
    `(("rust-bincode" ,rust-bincode "src")))
   (inputs
    `(("rust-rand-core" ,rust-rand-core "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
   (home-page "https://crates.io/crates/rand_isaac")
   (synopsis "ISAAC random number generator
")
   (description "ISAAC random number generator
")
   (license #f)))

(define-public rust-rand-jitter
  (package
   (name "rust-rand-jitter")
   (version "0.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rand_jitter" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0cmqy4za1wjlllcljiky4abihblx4vdjv58d7s115zif51cag7kv"))))
   (build-system cargo-build-system)
   (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-log" ,rust-log "src")
      ("rust-rand-core" ,rust-rand-core "src")
      ("rust-winapi" ,rust-winapi "src")))
   (home-page "https://github.com/rust-random/rand")
   (synopsis
    "Random number generator based on timing jitter")
   (description
    "Random number generator based on timing jitter")
   (license #f)))



;; (define-macro (def-rst name)
;;   `(define-public ,(string->symbol (string-append "rust-" name))
;;      (crate->guix-package (string ,name))))

(use-modules (language tree-il))

;; (let ((output-port (open-file "alacritty.scm" "a")))
;;   (display
;;    (tree-il->scheme
;;     (macroexpand '(def-rst "rand-os")))
;;    output-port)
;;   (newline output-port)
;;   (newline output-port)
;;   (close output-port))

;; (def-rst "rand-pcg")
;; (def-rst "rand-xorshift")
;; (def-rst "winapi")
;; (def-rst "autocfg")

;; (define-macro (def-rsts . names)
;;   `(begin ,(map
;;             (lambda (x) `(def-rst x))
;;             names)))

;; (def-rsts '("rand-os"))

;; (define-public ,(string->symbol (string-append "rust-" "rand-os"))
;;   (crate->guix-package "rand-os"))

;; (define-public (string-symbol "test-val") "test-val")

;; (map
;;  def-rst
;;  '("rand-os"))

(define-public rust-autocfg
  (package
   (name "rust-autocfg")
   (version "0.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "autocfg" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "16fprz5qi7paij5swyxb2hjris6d7bjzm9v8805gcjfswaz41mm6"))))
   (build-system cargo-build-system)
   (home-page "https://github.com/cuviper/autocfg")
   (synopsis
    "Automatic cfg for Rust compiler features")
   (description
    "Automatic cfg for Rust compiler features")
   (license #f)))

;; alacritty



(define-public rust-rand-os
(package
  (name "rust-rand-os")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rand_os" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0wahppm0s64gkr2vmhcgwc0lij37in1lgfxg5rbgqlz0l5vgcxbv"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-cloudabi" ,rust-cloudabi "src")
      ("rust-fuchsia-cprng" ,rust-fuchsia-cprng "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-log" ,rust-log "src")
      ("rust-rand-core" ,rust-rand-core "src")
      ("rust-rdrand" ,rust-rdrand "src")
      ("rust-stdweb" ,rust-stdweb "src")
      ("rust-wasm-bindgen" ,rust-wasm-bindgen "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://crates.io/crates/rand_os")
  (synopsis "OS backed Random Number Generator")
  (description "OS backed Random Number Generator")
  (license #f))
)

(define-public rust-rand-pcg
(package
  (name "rust-rand-pcg")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rand_pcg" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0i0bdla18a8x4jn1w0fxsbs3jg7ajllz6azmch1zw33r06dv1ydb"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-autocfg" ,rust-autocfg "src")
      ("rust-bincode" ,rust-bincode "src")))
  (inputs
    `(("rust-rand-core" ,rust-rand-core "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (home-page "https://crates.io/crates/rand_pcg")
  (synopsis
    "Selected PCG random number generators
")
  (description
    "Selected PCG random number generators
")
  (license #f))
)

(define-public rust-rand-xorshift
(package
  (name "rust-rand-xorshift")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rand_xorshift" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0p2x8nr00hricpi2m6ca5vysiha7ybnghz79yqhhx6sl4gkfkxyb"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bincode" ,rust-bincode "src")))
  (inputs
    `(("rust-rand-core" ,rust-rand-core "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (home-page
    "https://crates.io/crates/rand_xorshift")
  (synopsis "Xorshift random number generator
")
  (description
    "Xorshift random number generator
")
  (license #f))
)

(define-public rust-winapi
(package
  (name "rust-winapi")
  (version "0.3.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "winapi" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0w7pbhcl087lkggxdflfp2i92rq89ahsfdkjkv44fgmiy9m3h3pi"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-winapi-i686-pc-windows-gnu"
       ,rust-winapi-i686-pc-windows-gnu
       "src")
      ("rust-winapi-x86-64-pc-windows-gnu"
       ,rust-winapi-x86-64-pc-windows-gnu
       "src")))
  (home-page
    "https://github.com/retep998/winapi-rs")
  (synopsis
    "Raw FFI bindings for all of Windows API.")
  (description
    "Raw FFI bindings for all of Windows API.")
  (license #f))
)

(define-public rust-average
(package
  (name "rust-average")
  (version "0.9.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "average" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nnrriiddar4d9b7770m8lib7nvfrrlf21mr4ss33rqffdq8jyn8"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bencher" ,rust-bencher "src")
      ("rust-quantiles" ,rust-quantiles "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-streaming-stats"
       ,rust-streaming-stats
       "src")))
  (inputs
    `(("rust-conv" ,rust-conv "src")
      ("rust-float-ord" ,rust-float-ord "src")
      ("rust-num-integer" ,rust-num-integer "src")
      ("rust-num-traits" ,rust-num-traits "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (home-page "https://github.com/vks/average")
  (synopsis "Calculate statistics iteratively")
  (description "Calculate statistics iteratively")
  (license #f))
)

(define-public rust-rand-xoshiro
(package
  (name "rust-rand-xoshiro")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rand_xoshiro" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ac9ha6ll8b6l1930bd99k29jrjpsbpddvr6ycrnbi5rkwb1id03"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-rand-core" ,rust-rand-core "src")))
  (home-page
    "https://crates.io/crates/rand_xoshiro")
  (synopsis
    "Xoshiro, xoroshiro and splitmix64 random number generators")
  (description
    "Xoshiro, xoroshiro and splitmix64 random number generators")
  (license #f))
)

(define-public rust-conv
(package
  (name "rust-conv")
  (version "0.3.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "conv" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "168j1npqrif1yqxbgbk0pdrx9shzhs5ylc5a4xw49b6hbxi11zvq"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-winapi" ,rust-winapi "src")))
  (inputs
    `(("rust-custom-derive" ,rust-custom-derive "src")))
  (home-page
    "https://github.com/DanielKeep/rust-conv")
  (synopsis
    "This crate provides a number of conversion traits with more specific semantics than those provided by 'as' or 'From'/'Into'.")
  (description
    "This crate provides a number of conversion traits with more specific semantics than those provided by 'as' or 'From'/'Into'.")
  (license #f))
)

(define-public rust-float-ord
(package
  (name "rust-float-ord")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "float-ord" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0kin50365sr3spnbscq43lksymybi99ai9rkqdw90m6vixhlibbv"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (home-page
    "https://github.com/notriddle/rust-float-ord")
  (synopsis
    "A total ordering for floating-point numbers")
  (description
    "This package provides a total ordering for floating-point numbers")
  (license #f))
)

(define-public rust-num-integer
(package
  (name "rust-num-integer")
  (version "0.1.39")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "num-integer" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1shc9hfykcywgd86h2w6939d436gpmx2pbqbay653w3p4s6m4gg8"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-num-traits" ,rust-num-traits "src")))
  (home-page
    "https://github.com/rust-num/num-integer")
  (synopsis "Integer traits and functions")
  (description "Integer traits and functions")
  (license #f))
)

(define-public rust-serde-derive
(package
  (name "rust-serde-derive")
  (version "1.0.90")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_derive" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0yfysbp1l27rcyh4dld92nd6wwpmidfb8qqr7nr6iwa4qaz85z2q"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde" ,rust-serde "src")))
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://serde.rs")
  (synopsis
    "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
  (description
    "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
  (license #f))
)

(define-public rust-bencher
(package
  (name "rust-bencher")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bencher" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1x8p2xblgqssay8cdykp5pkfc0np0jk5bs5cx4f5av097aav9zbx"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/bluss/bencher/")
  (synopsis
    "A port of the libtest (unstable Rust) benchmark runner to Rust stable releases. Supports running benchmarks and filtering based on the name. Benchmark execution works exactly the same way and no more (caveat: black_box is still missing!).")
  (description
    "This package provides a port of the libtest (unstable Rust) benchmark runner to Rust stable releases.  Supports running benchmarks and filtering based on the name.  Benchmark execution works exactly the same way and no more (caveat: black_box is still missing!).")
  (license #f))
)

(define-public rust-quantiles
(package
  (name "rust-quantiles")
  (version "0.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "quantiles" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1wjp16a3d4bmldq9w2wds0q4gjz4mnsqac3g38r6ryr6zc9sh3y1"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")))
  (inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (home-page
    "https://github.com/postmates/quantiles")
  (synopsis
    "a collection of approximate quantile algorithms")
  (description
    "a collection of approximate quantile algorithms")
  (license #f))
)

(define-public rust-serde-json
(package
  (name "rust-serde-json")
  (version "1.0.39")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_json" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "03fl9l680gij0hrsr2csfm8nm858igvfy05czbdkzm54siqsl8ss"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-automod" ,rust-automod "src")
      ("rust-compiletest-rs"
       ,rust-compiletest-rs
       "src")
      ("rust-serde-bytes" ,rust-serde-bytes "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-stacker" ,rust-serde-stacker "src")))
  (inputs
    `(("rust-indexmap" ,rust-indexmap "src")
      ("rust-itoa" ,rust-itoa "src")
      ("rust-ryu" ,rust-ryu "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/serde-rs/json")
  (synopsis "A JSON serialization file format")
  (description
    "This package provides a JSON serialization file format")
  (license #f))
)

(define-public rust-streaming-stats
(package
  (name "rust-streaming-stats")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "streaming-stats" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0l7xz4g6709s80zqpvlhrg0qhgz64r94cwhmfsg8xhabgznbp2px"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-num-traits" ,rust-num-traits "src")))
  (home-page
    "https://github.com/BurntSushi/rust-stats")
  (synopsis
    "Experimental crate for computing basic statistics on streams.")
  (description
    "Experimental crate for computing basic statistics on streams.")
  (license #f))
)

(define-public rust-quickcheck
(package
  (name "rust-quickcheck")
  (version "0.8.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "quickcheck" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1p0p1vvyxy99m2prcxkb94yxrymcyn5pfqph3gvyx3s215aaws1m"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-env-logger" ,rust-env-logger "src")
      ("rust-log" ,rust-log "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-rand-core" ,rust-rand-core "src")))
  (home-page
    "https://github.com/BurntSushi/quickcheck")
  (synopsis
    "Automatic property based testing with shrinking.")
  (description
    "Automatic property based testing with shrinking.")
  (license #f))
)

(define-public rust-env-logger
(package
  (name "rust-env-logger")
  (version "0.6.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "env_logger" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0nh7ja7kikq551x16k1zgcn61xgsiip590cm1aimv52a0a8sh7xn"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-atty" ,rust-atty "src")
      ("rust-humantime" ,rust-humantime "src")
      ("rust-log" ,rust-log "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-termcolor" ,rust-termcolor "src")))
  (home-page
    "https://github.com/sebasmagri/env_logger/")
  (synopsis
    "A logging implementation for `log` which is configured via an environment
variable.
")
  (description
    "This package provides a logging implementation for `log` which is configured via an environment
variable.
")
  (license #f))
)

(define-public rust-atty
(package
  (name "rust-atty")
  (version "0.2.11")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "atty" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0lln6vaczj521qqjbaqnb81w5p6xk4fjfkg33r0m22cm4f3mnzcs"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-termion" ,rust-termion "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/softprops/atty")
  (synopsis "A simple interface for querying atty")
  (description
    "This package provides a simple interface for querying atty")
  (license #f))
)

(define-public rust-humantime
(package
  (name "rust-humantime")
  (version "1.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "humantime" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "057ilhy6vc9iqhhby5ymh45m051pgxwq2z437gwkbnqhw7rfb9rw"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-chrono" ,rust-chrono "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-time" ,rust-time "src")))
  (inputs
    `(("rust-quick-error" ,rust-quick-error "src")))
  (home-page
    "https://github.com/tailhook/humantime")
  (synopsis
    "    A parser and formatter for std::time::{Duration, SystemTime}
")
  (description
    "    A parser and formatter for std::time::{Duration, SystemTime}
")
  (license #f))
)

(define-public rust-regex
(package
  (name "rust-regex")
  (version "1.1.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "regex" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0n0csrjlyjl1i98zhlkk5v7bkppd8dpsx7psaiy1szgxnb50n2lg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-aho-corasick" ,rust-aho-corasick "src")
      ("rust-memchr" ,rust-memchr "src")
      ("rust-regex-syntax" ,rust-regex-syntax "src")
      ("rust-thread-local" ,rust-thread-local "src")
      ("rust-utf8-ranges" ,rust-utf8-ranges "src")))
  (home-page "https://github.com/rust-lang/regex")
  (synopsis
    "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
  (description
    "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
  (license #f))
)

(define-public rust-termcolor
(package
  (name "rust-termcolor")
  (version "1.0.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "termcolor" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0bvzdmna2qjgdj6yasjyczic30fwhr8bvkgxya4j4qhj0vbsv5j0"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-wincolor" ,rust-wincolor "src")))
  (home-page
    "https://github.com/BurntSushi/termcolor")
  (synopsis
    "A simple cross platform library for writing colored text to a terminal.
")
  (description
    "This package provides a simple cross platform library for writing colored text to a terminal.
")
  (license #f))
)

(define-public rust-termion
(package
  (name "rust-termion")
  (version "1.5.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "termion" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "15i0x5vcwb8bkd72g1vlafz3qza1g165rpw7pj9gsfdlmbgkp6k8"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-redox-syscall" ,rust-redox-syscall "src")
      ("rust-redox-termios" ,rust-redox-termios "src")))
  (home-page "https://github.com/ticki/termion")
  (synopsis
    "A bindless library for manipulating terminals.")
  (description
    "This package provides a bindless library for manipulating terminals.")
  (license #f))
)

(define-public rust-rustc-std-workspace-core
(package
  (name "rust-rustc-std-workspace-core")
  (version "1.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-std-workspace-core" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1309xhwyai9xpz128xrfjqkmnkvgjwddznmj7brbd8i8f58zamhr"))))
  (build-system cargo-build-system)
  (home-page "")
  (synopsis
    "Explicitly empty crate for rust-lang/rust integration
")
  (description
    "Explicitly empty crate for rust-lang/rust integration
")
  (license #f))
)

(define-public rust-redox-syscall
(package
  (name "rust-redox-syscall")
  (version "0.1.54")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "redox_syscall" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0lp22pjvmj33a2fa7y1h9cgxmnfd8whbb8s6n0f4yp7nl0a9q8hj"))))
  (build-system cargo-build-system)
  (home-page
    "https://gitlab.redox-os.org/redox-os/syscall")
  (synopsis
    "A Rust library to access raw Redox system calls")
  (description
    "This package provides a Rust library to access raw Redox system calls")
  (license #f))
)

(define-public rust-redox-termios
(package
  (name "rust-redox-termios")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "redox_termios" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0xhgvdh62mymgdl3jqrngl8hr4i8xwpnbsxnldq0l47993z1r2by"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-redox-syscall" ,rust-redox-syscall "src")))
  (home-page "https://github.com/redox-os/termios")
  (synopsis
    "A Rust library to access Redox termios functions")
  (description
    "This package provides a Rust library to access Redox termios functions")
  (license #f))
)

(define-public rust-winapi-i686-pc-windows-gnu
(package
  (name "rust-winapi-i686-pc-windows-gnu")
  (version "0.4.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "winapi-i686-pc-windows-gnu" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/retep998/winapi-rs")
  (synopsis
    "Import libraries for the i686-pc-windows-gnu target. Please don't use this crate directly, depend on winapi instead.")
  (description
    "Import libraries for the i686-pc-windows-gnu target.  Please don't use this crate directly, depend on winapi instead.")
  (license #f))
)

(define-public rust-winapi-x86-64-pc-windows-gnu
(package
  (name "rust-winapi-x86-64-pc-windows-gnu")
  (version "0.4.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri
             "winapi-x86_64-pc-windows-gnu"
             version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/retep998/winapi-rs")
  (synopsis
    "Import libraries for the x86_64-pc-windows-gnu target. Please don't use this crate directly, depend on winapi instead.")
  (description
    "Import libraries for the x86_64-pc-windows-gnu target.  Please don't use this crate directly, depend on winapi instead.")
  (license #f))
)


(define-public rust-quick-error
(package
  (name "rust-quick-error")
  (version "1.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "quick-error" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1w6kgwwv7p7zr0yyg5rb315lkk24bimywklwx7fsvsbwi10bjx4j"))))
  (build-system cargo-build-system)
  (home-page
    "http://github.com/tailhook/quick-error")
  (synopsis
    "    A macro which makes error types pleasant to write.
")
  (description
    "    A macro which makes error types pleasant to write.
")
  (license #f))
)

(define-public rust-chrono
(package
  (name "rust-chrono")
  (version "0.4.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "chrono" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0y1qcgnr7g9zgnmlzcrn31vn91x1vakpph9qgjnnzchw2a0ji4a5"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bincode" ,rust-bincode "src")
      ("rust-num-iter" ,rust-num-iter "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs
    `(("rust-num-integer" ,rust-num-integer "src")
      ("rust-num-traits" ,rust-num-traits "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-time" ,rust-time "src")))
  (home-page
    "https://github.com/chronotope/chrono")
  (synopsis "Date and time library for Rust")
  (description "Date and time library for Rust")
  (license #f))
)

(define-public rust-time
(package
  (name "rust-time")
  (version "0.1.42")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "time" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0vsbvsz0ryxb35dy9j4anxvy8zlaplmjmi0a4z4l64bc135cz3fv"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-log" ,rust-log "src")
      ("rust-winapi" ,rust-winapi "src")))
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-redox-syscall" ,rust-redox-syscall "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/rust-lang/time")
  (synopsis
    "Utilities for working with time-related functions in Rust.
")
  (description
    "Utilities for working with time-related functions in Rust.
")
  (license #f))
)

(define-public rust-rustc-serialize
(package
  (name "rust-rustc-serialize")
  (version "0.3.24")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-serialize" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nkg3vasg7nk80ffkazizgiyv3hb1l9g3d8h17cajbkx538jiwfw"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (home-page
    "https://github.com/rust-lang/rustc-serialize")
  (synopsis
    "Generic serialization/deserialization support corresponding to the
`derive(RustcEncodable, RustcDecodable)` mode in the compiler. Also includes
support for hex, base64, and json encoding and decoding.
")
  (description
    "Generic serialization/deserialization support corresponding to the
`derive(RustcEncodable, RustcDecodable)` mode in the compiler.  Also includes
support for hex, base64, and json encoding and decoding.
")
  (license #f))
)

(define-public rust-bincode
(package
  (name "rust-bincode")
  (version "1.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bincode" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0dgzpvs4hc9msd9gihmklx02sbg0mnf5mw4mxgz2yhddq5a8x74m"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-autocfg" ,rust-autocfg "src")
      ("rust-serde-bytes" ,rust-serde-bytes "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/TyOverby/bincode")
  (synopsis
    "A binary serialization / deserialization strategy that uses Serde for transforming structs into bytes and vice versa!")
  (description
    "This package provides a binary serialization / deserialization strategy that uses Serde for transforming structs into bytes and vice versa!")
  (license #f))
)

(define-public rust-num-iter
(package
  (name "rust-num-iter")
  (version "0.1.37")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "num-iter" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09115d12q1bcgig3ivnnbs8vz9kwqc78c0vvqm6ld9ci6aydngxg"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-num-integer" ,rust-num-integer "src")
      ("rust-num-traits" ,rust-num-traits "src")))
  (home-page
    "https://github.com/rust-num/num-iter")
  (synopsis
    "External iterators for generic mathematics")
  (description
    "External iterators for generic mathematics")
  (license #f))
)

(define-public rust-byteorder
(package
  (name "rust-byteorder")
  (version "1.3.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "byteorder" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1syvclxqjwf6qfq98y3fiy82msjp7q8wh7qkvf9b5pkw585b26d0"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")))
  (home-page
    "https://github.com/BurntSushi/byteorder")
  (synopsis
    "Library for reading/writing numbers in big-endian and little-endian.")
  (description
    "Library for reading/writing numbers in big-endian and little-endian.")
  (license #f))
)

(define-public rust-serde-bytes
(package
  (name "rust-serde-bytes")
  (version "0.11.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_bytes" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0sc5n336i7q4fiij4l8f892zcirgybrbxzl8bp51qxzqdvdlgzxa"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bincode" ,rust-bincode "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-test" ,rust-serde-test "src")))
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/serde-rs/bytes")
  (synopsis
    "Optimized handling of `&[u8]` and `Vec<u8>` for Serde")
  (description
    "Optimized handling of `&[u8]` and `Vec<u8>` for Serde")
  (license #f))
)

; alacritty

(define-public rust-proc-macro2
(package
  (name "rust-proc-macro2")
  (version "0.4.27")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "proc-macro2" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "05c92v787snyaq4ss16vxc9mdv6zndfgsdq8k3hnnyffmsf7ycad"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quote" ,rust-quote "src")))
  (inputs
    `(("rust-unicode-xid" ,rust-unicode-xid "src")))
  (home-page
    "https://github.com/alexcrichton/proc-macro2")
  (synopsis
    "A stable implementation of the upcoming new `proc_macro` API. Comes with an
option, off by default, to also reimplement itself in terms of the upstream
unstable API.
")
  (description
    "This package provides a stable implementation of the upcoming new `proc_macro` API.  Comes with an
option, off by default, to also reimplement itself in terms of the upstream
unstable API.
")
  (license #f))
)

(define-public rust-quote
(package
  (name "rust-quote")
  (version "0.6.12")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "quote" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nw0klza45hf127kfyrpxsxd5jw2l6h21qxalil3hkr7bnf7kx7s"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")))
  (home-page "https://github.com/dtolnay/quote")
  (synopsis "Quasi-quoting macro quote!(...)")
  (description "Quasi-quoting macro quote!(...)")
  (license #f))
)

(define-public rust-syn
(package
  (name "rust-syn")
  (version "0.15.32")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "syn" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1vdhyx6aizpi94xzq4r5z8xak27bxszr7wzgf309j5bcabn20rl4"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-colored" ,rust-colored "src")
      ("rust-insta" ,rust-insta "src")
      ("rust-rayon" ,rust-rayon "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-walkdir" ,rust-walkdir "src")))
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-unicode-xid" ,rust-unicode-xid "src")))
  (home-page "https://github.com/dtolnay/syn")
  (synopsis "Parser for Rust source code")
  (description "Parser for Rust source code")
  (license #f))
)

(define-public rust-unicode-xid
(package
  (name "rust-unicode-xid")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-xid" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1z57lqh4s18rr4x0j4fw4fmp9hf9346h0kmdgqsqx0fhjr3k0wpw"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/unicode-rs/unicode-xid")
  (synopsis
    "Determine whether characters have the XID_Start
or XID_Continue properties according to
Unicode Standard Annex #31.
")
  (description
    "Determine whether characters have the XID_Start
or XID_Continue properties according to
Unicode Standard Annex #31.
")
  (license #f))
)

(define-public rust-colored
(package
  (name "rust-colored")
  (version "1.7.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "colored" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1a9840vbdn4fl73n6rkkwciy5cc0qcw6496h5zhp2hka2mg4b6kf"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-ansi-term" ,rust-ansi-term "src")
      ("rust-rspec" ,rust-rspec "src")))
  (inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")))
  (home-page "https://github.com/mackwic/colored")
  (synopsis
    "The most simple way to add colors in your terminal")
  (description
    "The most simple way to add colors in your terminal")
  (license #f))
)

(define-public rust-insta
(package
  (name "rust-insta")
  (version "0.7.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "insta" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0jkk5ww4mjwnhr9dp0xnhhpfprj3pr8n2lcqss5wwhnkhy5dirq3"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-chrono" ,rust-chrono "src")
      ("rust-ci-info" ,rust-ci-info "src")
      ("rust-console" ,rust-console "src")
      ("rust-difference" ,rust-difference "src")
      ("rust-failure" ,rust-failure "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-pest" ,rust-pest "src")
      ("rust-pest-derive" ,rust-pest-derive "src")
      ("rust-ron" ,rust-ron "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-serde-yaml" ,rust-serde-yaml "src")
      ("rust-uuid" ,rust-uuid "src")))
  (home-page "https://github.com/mitsuhiko/insta")
  (synopsis "A snapshot testing library for Rust")
  (description
    "This package provides a snapshot testing library for Rust")
  (license #f))
)

(define-public rust-rayon
(package
  (name "rust-rayon")
  (version "1.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rayon" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0wq41f15y05nlarijn9c1vxscxj5sazn3lhd6mmnicj5fzr18f1p"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-docopt" ,rust-docopt "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (inputs
    `(("rust-crossbeam-deque"
       ,rust-crossbeam-deque
       "src")
      ("rust-either" ,rust-either "src")
      ("rust-rayon-core" ,rust-rayon-core "src")))
  (home-page "https://github.com/rayon-rs/rayon")
  (synopsis
    "Simple work-stealing parallelism for Rust")
  (description
    "Simple work-stealing parallelism for Rust")
  (license #f))
)

(define-public rust-walkdir
(package
  (name "rust-walkdir")
  (version "2.2.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "walkdir" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "18cz2a7cjxiikz8jp9xnr54wqxh675d104v2552a2a8j8g9px7cx"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-docopt" ,rust-docopt "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (inputs
    `(("rust-same-file" ,rust-same-file "src")
      ("rust-winapi" ,rust-winapi "src")
      ("rust-winapi-util" ,rust-winapi-util "src")))
  (home-page
    "https://github.com/BurntSushi/walkdir")
  (synopsis "Recursively walk a directory.")
  (description "Recursively walk a directory.")
  (license #f))
)

(define-public rust-ansi-term
(package
  (name "rust-ansi-term")
  (version "0.11.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ansi_term" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
  (build-system cargo-build-system)
  (inputs `(("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/ogham/rust-ansi-term")
  (synopsis
    "Library for ANSI terminal colours and styles (bold, underline)")
  (description
    "Library for ANSI terminal colours and styles (bold, underline)")
  (license #f))
)

(define-public rust-rspec
(package
  (name "rust-rspec")
  (version "1.0.0-beta.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rspec" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1abfzwkbxlwahb243k8d3fp6i135lx1aqmbfl79w9zlpng182ndk"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-clippy" ,rust-clippy "src")))
  (inputs
    `(("rust-colored" ,rust-colored "src")
      ("rust-derive-new" ,rust-derive-new "src")
      ("rust-derive-builder"
       ,rust-derive-builder
       "src")
      ("rust-expectest" ,rust-expectest "src")
      ("rust-rayon" ,rust-rayon "src")))
  (home-page "https://mackwic.github.io/rspec")
  (synopsis
    "Write Rspec-like tests with stable rust")
  (description
    "Write Rspec-like tests with stable rust")
  (license #f))
)

(define-public rust-lazy-static
(package
  (name "rust-lazy-static")
  (version "1.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "lazy_static" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "052ac27w189hrf1j3hz7sga46rp84zl2hqnzyihxv78mgzr2jmxw"))))
  (build-system cargo-build-system)
  (inputs `(("rust-spin" ,rust-spin "src")))
  (home-page
    "https://github.com/rust-lang-nursery/lazy-static.rs")
  (synopsis
    "A macro for declaring lazily evaluated statics in Rust.")
  (description
    "This package provides a macro for declaring lazily evaluated statics in Rust.")
  (license #f))
)

(define-public rust-clippy
(package
  (name "rust-clippy")
  (version "0.0.302")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "clippy" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1562x3sq9mgmc8j39gd34wqm7ybrdvpmj7cc1n450gwsawayw4fr"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-term" ,rust-term "src")))
  (home-page
    "https://github.com/rust-lang-nursery/rust-clippy")
  (synopsis
    "A bunch of helpful lints to avoid common pitfalls in Rust.")
  (description
    "This package provides a bunch of helpful lints to avoid common pitfalls in Rust.")
  (license #f))
)

(define-public rust-derive-new
(package
  (name "rust-derive-new")
  (version "0.5.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "derive-new" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0z6qwdlxfdjrdpfgb7acxvfn1kxfv99g92fpyi32a1xfjvl1993c"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://github.com/nrc/derive-new")
  (synopsis
    "`#[derive(new)]` implements simple constructor functions for structs and enums.")
  (description
    "`#[derive(new)]` implements simple constructor functions for structs and enums.")
  (license #f))
)

(define-public rust-derive-builder
(package
  (name "rust-derive-builder")
  (version "0.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "derive_builder" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "078c4wmvmy7a52d8l4sfa2sch50bmvi5qlw5hl8gjy5vd8z57jm0"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-env-logger" ,rust-env-logger "src")
      ("rust-log" ,rust-log "src")
      ("rust-pretty-assertions"
       ,rust-pretty-assertions
       "src")
      ("rust-skeptic" ,rust-skeptic "src")))
  (inputs
    `(("rust-compiletest-rs"
       ,rust-compiletest-rs
       "src")
      ("rust-darling" ,rust-darling "src")
      ("rust-derive-builder-core"
       ,rust-derive-builder-core
       "src")
      ("rust-env-logger" ,rust-env-logger "src")
      ("rust-log" ,rust-log "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-skeptic" ,rust-skeptic "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/colin-kiegel/rust-derive-builder")
  (synopsis
    "Rust macro to automatically implement the builder pattern for arbitrary structs.")
  (description
    "Rust macro to automatically implement the builder pattern for arbitrary structs.")
  (license #f))
)

(define-public rust-expectest
(package
  (name "rust-expectest")
  (version "0.11.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "expectest" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "00dv47irmsyq7brzjhz4xns3p722gm98zp39h9hq2mrzd5marpgq"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-num-traits" ,rust-num-traits "src")))
  (home-page
    "https://github.com/zummenix/expectest")
  (synopsis
    "Crate provides matchers and matcher functions for unit testing.")
  (description
    "Crate provides matchers and matcher functions for unit testing.")
  (license #f))
)

(define-public rust-term
(package
  (name "rust-term")
  (version "0.5.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "term" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0hkgjrfisj6zjwz525639pmsvzhlc48a0h65nw87qrdp6jihdlgd"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-dirs" ,rust-dirs "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/Stebalien/term")
  (synopsis "A terminal formatting library
")
  (description
    "This package provides a terminal formatting library
")
  (license #f))
)

(define-public rust-dirs
(package
  (name "rust-dirs")
  (version "1.0.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "dirs" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "009rrhzj9pxyncmm2vhlj70npg0cgggv2hjbbkiwdl9vccq8kmrz"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-redox-users" ,rust-redox-users "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/soc/dirs-rs")
  (synopsis
    "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
  (description
    "This package provides a tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
  (license #f))
)

(define-public rust-redox-users
(package
  (name "rust-redox-users")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "redox_users" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0a1q5jv76vj1mwmqf2mmhknmkpw5wndx91gjfgg7vs8p79621r9z"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-argon2rs" ,rust-argon2rs "src")
      ("rust-failure" ,rust-failure "src")
      ("rust-rand-os" ,rust-rand-os "src")
      ("rust-redox-syscall" ,rust-redox-syscall "src")))
  (home-page
    "https://gitlab.redox-os.org/redox-os/users")
  (synopsis
    "A Rust library to access Redox users and groups functionality")
  (description
    "This package provides a Rust library to access Redox users and groups functionality")
  (license #f))
)

(define-public rust-argon2rs
(package
  (name "rust-argon2rs")
  (version "0.2.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "argon2rs" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "14mkgkrjd4b4zy92pflz6yb4j1wn2chbd8jczxknxbkdm2vb0rrz"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cargon" ,rust-cargon "src")))
  (inputs
    `(("rust-blake2-rfc" ,rust-blake2-rfc "src")
      ("rust-scoped-threadpool"
       ,rust-scoped-threadpool
       "src")))
  (home-page "https://github.com/bryant/argon2rs")
  (synopsis
    "The pure Rust password hashing library that runs on Argon2.")
  (description
    "The pure Rust password hashing library that runs on Argon2.")
  (license #f))
)

(define-public rust-failure
(package
  (name "rust-failure")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "failure" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1qppmgv4i5jj6vrss91qackqnl0a12h7lnby4l7j5fdy78yxhnvr"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-backtrace" ,rust-backtrace "src")
      ("rust-failure-derive"
       ,rust-failure-derive
       "src")))
  (home-page
    "https://rust-lang-nursery.github.io/failure/")
  (synopsis
    "Experimental error handling abstraction.")
  (description
    "Experimental error handling abstraction.")
  (license #f))
)

(define-public rust-cargon
(package
  (name "rust-cargon")
  (version "0.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cargon" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1cszlab7jk736p0lb50ag4l9nv72m7j41bwrmygl0lr4iz0350w2"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-gcc" ,rust-gcc "src")))
  (home-page "https://github.com/bryant/argon2rs")
  (synopsis
    "Thin wrapper around the Argon2 C library. Used in argon2rs' bench suite.")
  (description
    "Thin wrapper around the Argon2 C library.  Used in argon2rs' bench suite.")
  (license #f))
)

(define-public rust-blake2-rfc
(package
  (name "rust-blake2-rfc")
  (version "0.2.18")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "blake2-rfc" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0034g47hyq2bzmk40895ill1mbnpmmjakdq3dmm9clidvl5m6vax"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-data-encoding" ,rust-data-encoding "src")))
  (inputs
    `(("rust-arrayvec" ,rust-arrayvec "src")
      ("rust-clippy" ,rust-clippy "src")
      ("rust-constant-time-eq"
       ,rust-constant-time-eq
       "src")))
  (home-page
    "https://github.com/cesarb/blake2-rfc")
  (synopsis
    "A pure Rust implementation of BLAKE2 based on RFC 7693.")
  (description
    "This package provides a pure Rust implementation of BLAKE2 based on RFC 7693.")
  (license #f))
)

(define-public rust-scoped-threadpool
(package
  (name "rust-scoped-threadpool")
  (version "0.1.9")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "scoped_threadpool" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1a26d3lk40s9mrf4imhbik7caahmw2jryhhb6vqv6fplbbgzal8x"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")))
  (home-page
    "https://github.com/Kimundi/scoped-threadpool-rs")
  (synopsis
    "A library for scoped and cached threadpools.")
  (description
    "This package provides a library for scoped and cached threadpools.")
  (license #f))
)

(define-public rust-gcc
(package
  (name "rust-gcc")
  (version "0.3.55")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "gcc" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1hng1sajn4r67hndvhjysswz8niayjwvcj42zphpxzhbz89kjpwg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs `(("rust-rayon" ,rust-rayon "src")))
  (home-page
    "https://github.com/alexcrichton/gcc-rs")
  (synopsis
    "**Deprecated** crate, renamed to `cc`

A build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.
")
  (description
    "**Deprecated** crate, renamed to `cc`

A build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.
")
  (license #f))
)

(define-public rust-tempdir
(package
  (name "rust-tempdir")
  (version "0.3.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tempdir" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1n5n86zxpgd85y0mswrp5cfdisizq2rv3la906g6ipyc03xvbwhm"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-rand" ,rust-rand "src")
      ("rust-remove-dir-all"
       ,rust-remove-dir-all
       "src")))
  (home-page
    "https://github.com/rust-lang/tempdir")
  (synopsis
    "A library for managing a temporary directory and deleting all contents when it's
dropped.
")
  (description
    "This package provides a library for managing a temporary directory and deleting all contents when it's
dropped.
")
  (license #f))
)

(define-public rust-remove-dir-all
(package
  (name "rust-remove-dir-all")
  (version "0.5.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "remove_dir_all" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1icr4xfsv0cnavqp838kqzrcnbbml5v85h648n3d7110k8dvm21l"))))
  (build-system cargo-build-system)
  (inputs `(("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/Aaronepower/remove_dir_all.git")
  (synopsis
    "A safe, reliable implementation of remove_dir_all for Windows")
  (description
    "This package provides a safe, reliable implementation of remove_dir_all for Windows")
  (license #f))
)

(define-public rust-docopt
(package
  (name "rust-docopt")
  (version "1.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "docopt" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0s9rcpmnnivs502q69lc1h1wrwapkq09ikgbfbgqf31idmc5llkz"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-strsim" ,rust-strsim "src")))
  (home-page "https://github.com/docopt/docopt.rs")
  (synopsis "Command line argument parsing.")
  (description "Command line argument parsing.")
  (license #f))
)

(define-public rust-crossbeam-deque
(package
  (name "rust-crossbeam-deque")
  (version "0.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-deque" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0was9x71cz5g1y3670cyy6jdmsdfg6k9mbf0ddz2k1mdd7hx535i"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-crossbeam-epoch"
       ,rust-crossbeam-epoch
       "src")
      ("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")))
  (home-page
    "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
  (synopsis "Concurrent work-stealing deque")
  (description "Concurrent work-stealing deque")
  (license #f))
)

(define-public rust-either
(package
  (name "rust-either")
  (version "1.5.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "either" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0yyggfd5yq9hyyp0bd5jj0fgz3rwws42d19ri0znxwwqs3hcy9sm"))))
  (build-system cargo-build-system)
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/bluss/either")
  (synopsis
    "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.
")
  (description
    "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.
")
  (license #f))
)

(define-public rust-rayon-core
(package
  (name "rust-rayon-core")
  (version "1.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rayon-core" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0mkkabm3h4xvrkvjp675c07zcpcb7jk09rlg9mbpfs5s5blx2mdh"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-crossbeam-deque"
       ,rust-crossbeam-deque
       "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-num-cpus" ,rust-num-cpus "src")))
  (home-page "https://github.com/rayon-rs/rayon")
  (synopsis "Core APIs for Rayon")
  (description "Core APIs for Rayon")
  (license #f))
)

(define-public rust-strsim
(package
  (name "rust-strsim")
  (version "0.9.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "strsim" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0hvnnpaywxiphafrb2z2yinq8nnvc9p7pm2cplxsjfj2n5m6db1l"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-hashbrown" ,rust-hashbrown "src")
      ("rust-ndarray" ,rust-ndarray "src")))
  (home-page "https://github.com/dguo/strsim-rs")
  (synopsis
    "Implementations of string similarity metrics.
Includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro, and Jaro-Winkler.
")
  (description
    "Implementations of string similarity metrics.
Includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro, and Jaro-Winkler.
")
  (license #f))
)

(define-public rust-spin
(package
  (name "rust-spin")
  (version "0.5.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "spin" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0m9clchsj0rf13bggsgvbv9haiy0f6rhvnvkpvkk8720a5pkydj4"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/mvdnes/spin-rs.git")
  (synopsis
    "Synchronization primitives based on spinning.
They may contain data, are usable without `std`,
and static initializers are available.
")
  (description
    "Synchronization primitives based on spinning.
They may contain data, are usable without `std`,
and static initializers are available.
")
  (license #f))
)

(define-public rust-aho-corasick
(package
  (name "rust-aho-corasick")
  (version "0.7.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "aho-corasick" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0k1nqabiz37mnfnlrn084qi9yf8pj6a38qgbb3lc5zlr1jp89x76"))))
  (build-system cargo-build-system)
  (inputs `(("rust-memchr" ,rust-memchr "src")))
  (home-page
    "https://github.com/BurntSushi/aho-corasick")
  (synopsis "Fast multiple substring searching.")
  (description
    "Fast multiple substring searching.")
  (license #f))
)

(define-public rust-memchr
(package
  (name "rust-memchr")
  (version "2.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "memchr" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0f8wdra7yaggsr4jzlrvpd8yknnqhd990iijdr6llgc8gk2ppz1f"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")))
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/BurntSushi/rust-memchr")
  (synopsis "Safe interface to memchr.")
  (description "Safe interface to memchr.")
  (license #f))
)

(define-public rust-regex-syntax
(package
  (name "rust-regex-syntax")
  (version "0.6.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "regex-syntax" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "15ha1mlyp77h49lbkmqsclvj21df9afqd644v60pnadyxs0qdzfw"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-ucd-util" ,rust-ucd-util "src")))
  (home-page "https://github.com/rust-lang/regex")
  (synopsis "A regular expression parser.")
  (description
    "This package provides a regular expression parser.")
  (license #f))
)

(define-public rust-thread-local
(package
  (name "rust-thread-local")
  (version "0.3.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "thread_local" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "06rzik99p8c5js8238yhc8rk6np543ylb1dy9nrw5v80j0r3xdf6"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")))
  (home-page
    "https://github.com/Amanieu/thread_local-rs")
  (synopsis "Per-object thread-local storage")
  (description "Per-object thread-local storage")
  (license #f))
)

(define-public rust-utf8-ranges
(package
  (name "rust-utf8-ranges")
  (version "1.0.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "utf8-ranges" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0drp4j55bz7gzk5sbrk6g1nd0p3xm2an9q77mpvhjxpqpr47wvvr"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")))
  (home-page
    "https://github.com/BurntSushi/utf8-ranges")
  (synopsis
    "Convert ranges of Unicode codepoints to UTF-8 byte ranges.")
  (description
    "Convert ranges of Unicode codepoints to UTF-8 byte ranges.")
  (license #f))
)

(define-public rust-ucd-util
(package
  (name "rust-ucd-util")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ucd-util" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "11lgx380zgqsm265cg78w2mcjpmldbwbi01lb5w48hyqwi720p2k"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/BurntSushi/ucd-generate")
  (synopsis
    "A small utility library for working with the Unicode character database.
")
  (description
    "This package provides a small utility library for working with the Unicode character database.
")
  (license #f))
)

(define-public rust-hashbrown
(package
  (name "rust-hashbrown")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "hashbrown" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0y8z3frb69k45ixpwmx77w34ccj3125fq24aqwaks2z8lh7r1r31"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-rayon" ,rust-rayon "src")
      ("rust-rustc-hash" ,rust-rustc-hash "src")
      ("rust-serde-test" ,rust-serde-test "src")))
  (inputs
    `(("rust-compiler-builtins"
       ,rust-compiler-builtins
       "src")
      ("rust-rayon" ,rust-rayon "src")
      ("rust-rustc-std-workspace-alloc"
       ,rust-rustc-std-workspace-alloc
       "src")
      ("rust-rustc-std-workspace-core"
       ,rust-rustc-std-workspace-core
       "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/Amanieu/hashbrown")
  (synopsis
    "A Rust port of Google's SwissTable hash map")
  (description
    "This package provides a Rust port of Google's SwissTable hash map")
  (license #f))
)

(define-public rust-ndarray
(package
  (name "rust-ndarray")
  (version "0.12.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ndarray" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0a5rfwcbqnvbwi3nw5sfz6kf0flhmjxs64s0b4kxc6lhmyl81wvw"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-defmac" ,rust-defmac "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rawpointer" ,rust-rawpointer "src")))
  (inputs
    `(("rust-blas-src" ,rust-blas-src "src")
      ("rust-cblas-sys" ,rust-cblas-sys "src")
      ("rust-itertools" ,rust-itertools "src")
      ("rust-matrixmultiply"
       ,rust-matrixmultiply
       "src")
      ("rust-num-complex" ,rust-num-complex "src")
      ("rust-num-traits" ,rust-num-traits "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/rust-ndarray/ndarray")
  (synopsis
    "An n-dimensional array for general elements and for numerics. Lightweight array views and slicing; views support chunking and splitting.")
  (description
    "An n-dimensional array for general elements and for numerics.  Lightweight array views and slicing; views support chunking and splitting.")
  (license #f))
)

(define-public rust-rustc-hash
(package
  (name "rust-rustc-hash")
  (version "1.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-hash" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1f4cnbcmz2c3zjidqszc9c4fip37ch4xl74nkkp9dw291j5zqh3m"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")))
  (home-page
    "https://github.com/rust-lang-nursery/rustc-hash")
  (synopsis
    "speed, non-cryptographic hash used in rustc")
  (description
    "speed, non-cryptographic hash used in rustc")
  (license #f))
)

(define-public rust-compiler-builtins
(package
  (name "rust-compiler-builtins")
  (version "0.1.10")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "compiler_builtins" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0iai33730z89iic7jww4kag8ijdala69i3qp16c22s4mcan57nja"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (inputs
    `(("rust-rustc-std-workspace-core"
       ,rust-rustc-std-workspace-core
       "src")))
  (home-page
    "https://github.com/rust-lang-nursery/compiler-builtins")
  (synopsis
    "Compiler intrinsics used by the Rust compiler. Also available for other targets
if necessary!
")
  (description
    "Compiler intrinsics used by the Rust compiler.  Also available for other targets
if necessary!
")
  (license #f))
)

(define-public rust-rustc-std-workspace-alloc
(package
  (name "rust-rustc-std-workspace-alloc")
  (version "1.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-std-workspace-alloc" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "11psmqk6glglxl3zwh8slz6iynfxaifh4spd2wcnws552dqdarpz"))))
  (build-system cargo-build-system)
  (home-page "")
  (synopsis "workspace hack")
  (description "workspace hack")
  (license #f))
)

(define-public rust-cc
(package
  (name "rust-cc")
  (version "1.0.35")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cc" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10wcqmgfszsqfb3b7ygza0w97vmxhq9f9wc14x634q7bbvp3ypsy"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs `(("rust-rayon" ,rust-rayon "src")))
  (home-page
    "https://github.com/alexcrichton/cc-rs")
  (synopsis
    "A build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.
")
  (description
    "This package provides a build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.
")
  (license #f))
)

(define-public rust-defmac
(package
  (name "rust-defmac")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "defmac" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "01ff3jdmcc5waffkwllndnx5hsn414r7x1rq4ib73n7awsyzxkxv"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/bluss/defmac")
  (synopsis
    "A macro to define lambda-like macros inline.")
  (description
    "This package provides a macro to define lambda-like macros inline.")
  (license #f))
)

(define-public rust-rawpointer
(package
  (name "rust-rawpointer")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rawpointer" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "06ghpm9y7gacks78s3maakha07kbnwrxif5q37r2l7z1sali3b7b"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/bluss/rawpointer/")
  (synopsis
    "Extra methods for raw pointers.

For example `.post_inc()` and `.pre_dec()` (c.f. `ptr++` and `--ptr`) and
`ptrdistance`.
")
  (description
    "Extra methods for raw pointers.

For example `.post_inc()` and `.pre_dec()` (c.f. `ptr++` and `--ptr`) and
`ptrdistance`.
")
  (license #f))
)

(define-public rust-blas-src
(package
  (name "rust-blas-src")
  (version "0.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "blas-src" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0l9c1gjhld3ajalak1ipklxfjvwqyy3l7xl019spdbqlrk8r9f57"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-accelerate-src"
       ,rust-accelerate-src
       "src")
      ("rust-intel-mkl-src" ,rust-intel-mkl-src "src")
      ("rust-netlib-src" ,rust-netlib-src "src")
      ("rust-openblas-src" ,rust-openblas-src "src")))
  (home-page
    "https://github.com/blas-lapack-rs/blas-src")
  (synopsis
    "The package provides a BLAS source of choice.")
  (description
    "The package provides a BLAS source of choice.")
  (license #f))
)

(define-public rust-cblas-sys
(package
  (name "rust-cblas-sys")
  (version "0.1.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cblas-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0rgsn3klhhh09d8qf3b87zl4rwk93l2g0qzh9hhb0lff5kcfrzmn"))))
  (build-system cargo-build-system)
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/blas-lapack-rs/cblas-sys")
  (synopsis
    "The package provides bindings to CBLAS (C).")
  (description
    "The package provides bindings to CBLAS (C).")
  (license #f))
)

(define-public rust-itertools
(package
  (name "rust-itertools")
  (version "0.8.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "itertools" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0n2k13b6w4x2x6np2lykh9bj3b3z4hwh2r4cn3z2dgnfq7cng12v"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-permutohedron" ,rust-permutohedron "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs `(("rust-either" ,rust-either "src")))
  (home-page
    "https://github.com/bluss/rust-itertools")
  (synopsis
    "Extra iterator adaptors, iterator methods, free functions, and macros.")
  (description
    "Extra iterator adaptors, iterator methods, free functions, and macros.")
  (license #f))
)

(define-public rust-matrixmultiply
(package
  (name "rust-matrixmultiply")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "matrixmultiply" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "16sgc1j87hmsqmhlqpqgcpbrb00f267ikbr55fhxla8nhwnxgznw"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bencher" ,rust-bencher "src")
      ("rust-itertools" ,rust-itertools "src")))
  (inputs
    `(("rust-rawpointer" ,rust-rawpointer "src")))
  (home-page
    "https://github.com/bluss/matrixmultiply/")
  (synopsis
    "General matrix multiplication for f32 and f64 matrices. Operates on matrices with general layout (they can use arbitrary row and column stride). Detects and uses AVX or SSE2 on x86 platforms transparently for higher performance. Uses a microkernel strategy, so that the implementation is easy to parallelize and optimize.")
  (description
    "General matrix multiplication for f32 and f64 matrices.  Operates on matrices with general layout (they can use arbitrary row and column stride).  Detects and uses AVX or SSE2 on x86 platforms transparently for higher performance.  Uses a microkernel strategy, so that the implementation is easy to parallelize and optimize.")
  (license #f))
)

(define-public rust-num-complex
(package
  (name "rust-num-complex")
  (version "0.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "num-complex" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1y5sv21a9f05crr78ih3gwi7hi8in1svcxw2d0q1jj6jdkl9nyqh"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-num-traits" ,rust-num-traits "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/rust-num/num-complex")
  (synopsis
    "Complex numbers implementation for Rust")
  (description
    "Complex numbers implementation for Rust")
  (license #f))
)

(define-public rust-accelerate-src
(package
  (name "rust-accelerate-src")
  (version "0.3.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "accelerate-src" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "17fiqyq7f9k41pbsyrvk9pxyx9z6fw399wq036cvwkbmb14xcpj1"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/blas-lapack-rs/accelerate-src")
  (synopsis
    "The package provides a source of BLAS and LAPACK via the Accelerate framework.")
  (description
    "The package provides a source of BLAS and LAPACK via the Accelerate framework.")
  (license #f))
)

(define-public rust-intel-mkl-src
(package
  (name "rust-intel-mkl-src")
  (version "0.2.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "intel-mkl-src" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nsiwf5cn54njjyy1ng6afk5dvax9xy3krspb3fkk93yag6xr7gh"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-rust-crypto" ,rust-rust-crypto "src")))
  (home-page
    "https://github.com/termoshtt/rust-intel-mkl")
  (synopsis
    "Redistribution of Intel(R) MKL as a crate")
  (description
    "Redistribution of Intel(R) MKL as a crate")
  (license #f))
)

(define-public rust-netlib-src
(package
  (name "rust-netlib-src")
  (version "0.7.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "netlib-src" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "112hwfw1zzdj10h3j213xxqjrq38iygb3nb3ijay65ycmrg819s4"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cmake" ,rust-cmake "src")
      ("rust-libc" ,rust-libc "src")))
  (home-page "https://github.com/cmr/netlib-src")
  (synopsis
    "The package provides a source of BLAS and LAPACK via Netlib.")
  (description
    "The package provides a source of BLAS and LAPACK via Netlib.")
  (license #f))
)

(define-public rust-openblas-src
(package
  (name "rust-openblas-src")
  (version "0.7.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "openblas-src" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1dyf7yh6rmkk7k3pgcp5p8248f08hhajkigw42bfwjw1d3jk6d8b"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/blas-lapack-rs/openblas-src")
  (synopsis
    "The package provides a source of BLAS and LAPACK via OpenBLAS.")
  (description
    "The package provides a source of BLAS and LAPACK via OpenBLAS.")
  (license #f))
)

(define-public rust-rust-crypto
(package
  (name "rust-rust-crypto")
  (version "0.2.36")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rust-crypto" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0aias7gh2ypj4skmh6hfsjli4fhnvcvf9s1ljjpz9m9zk79havgp"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-gcc" ,rust-gcc "src")))
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-time" ,rust-time "src")))
  (home-page
    "https://github.com/DaGenix/rust-crypto/")
  (synopsis
    "A (mostly) pure-Rust implementation of various common cryptographic algorithms.")
  (description
    "This package provides a (mostly) pure-Rust implementation of various common cryptographic algorithms.")
  (license #f))
)

(define-public rust-cfg-if
(package
  (name "rust-cfg-if")
  (version "0.1.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cfg-if" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1x6i0lyf5minisrd20m5ng17pvbl8cp39rjwnkpjx1vf75ak7m0i"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/alexcrichton/cfg-if")
  (synopsis
    "A macro to ergonomically define an item depending on a large number of #[cfg]
parameters. Structured like an if-else chain, the first matching branch is the
item that gets emitted.
")
  (description
    "This package provides a macro to ergonomically define an item depending on a large number of #[cfg]
parameters.  Structured like an if-else chain, the first matching branch is the
item that gets emitted.
")
  (license #f))
)

(define-public rust-cmake
(package
  (name "rust-cmake")
  (version "0.1.38")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cmake" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1j6fggcv675nwncflv6sbwh81ajf89lkg9jj0kygphsgagn0w8cn"))))
  (build-system cargo-build-system)
  (inputs `(("rust-cc" ,rust-cc "src")))
  (home-page
    "https://github.com/alexcrichton/cmake-rs")
  (synopsis
    "A build dependency for running `cmake` to build a native library
")
  (description
    "This package provides a build dependency for running `cmake` to build a native library
")
  (license #f))
)

(define-public rust-permutohedron
(package
  (name "rust-permutohedron")
  (version "0.2.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "permutohedron" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0b1pzh48j86v46wxngch6k1kx9cdw3jr3lwa86gd6jd4bmxzz1xn"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/bluss/permutohedron")
  (synopsis
    "Generate permutations of sequences. Either lexicographical order permutations, or a minimal swaps permutation sequence implemented using Heap's algorithm.")
  (description
    "Generate permutations of sequences.  Either lexicographical order permutations, or a minimal swaps permutation sequence implemented using Heap's algorithm.")
  (license #f))
)

(define-public rust-crossbeam-epoch
(package
  (name "rust-crossbeam-epoch")
  (version "0.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-epoch" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1d408b9x82mdbnb405gw58v5mmdbj2rl28a1h7b9rmn25h8f7j84"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-arrayvec" ,rust-arrayvec "src")
      ("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-memoffset" ,rust-memoffset "src")
      ("rust-scopeguard" ,rust-scopeguard "src")))
  (home-page
    "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
  (synopsis "Epoch-based garbage collection")
  (description "Epoch-based garbage collection")
  (license #f))
)

(define-public rust-crossbeam-utils
(package
  (name "rust-crossbeam-utils")
  (version "0.6.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-utils" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0p5aa8k3wpsn17md4rx038ac2azm9354knbxdfvn7dd7yk76yc7q"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-lazy-static" ,rust-lazy-static "src")))
  (home-page
    "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
  (synopsis "Utilities for concurrent programming")
  (description
    "Utilities for concurrent programming")
  (license #f))
)

(define-public rust-arrayvec
(package
  (name "rust-arrayvec")
  (version "0.4.10")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "arrayvec" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0wcch3ca9qvkixgdbd2afrv1xa27l83vpraf7frsh9l8pivgpiwj"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bencher" ,rust-bencher "src")
      ("rust-matches" ,rust-matches "src")
      ("rust-serde-test" ,rust-serde-test "src")))
  (inputs
    `(("rust-nodrop" ,rust-nodrop "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/bluss/arrayvec")
  (synopsis
    "A vector with fixed capacity, backed by an array (it can be stored on the stack too). Implements fixed capacity ArrayVec and ArrayString.")
  (description
    "This package provides a vector with fixed capacity, backed by an array (it can be stored on the stack too).  Implements fixed capacity ArrayVec and ArrayString.")
  (license #f))
)

(define-public rust-memoffset
(package
  (name "rust-memoffset")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "memoffset" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1q0c4sci6x7x5hr5vz889kwskqc52p2psyzj488gq4m69j8wryky"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rustc-version" ,rust-rustc-version "src")))
  (home-page "https://github.com/Gilnaa/memoffset")
  (synopsis
    "offset_of functionality for Rust structs.")
  (description
    "offset_of functionality for Rust structs.")
  (license #f))
)

(define-public rust-scopeguard
(package
  (name "rust-scopeguard")
  (version "1.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "scopeguard" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "03aay84r1f6w87ckbpj6cc4rnsxkxcfs13n5ynxjia0qkgjiabml"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/bluss/scopeguard")
  (synopsis
    "A RAII scope guard that will run a given closure when it goes out of scope,
even if the code between panics (assuming unwinding panic).

Defines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as
shorthands for guards with one of the implemented strategies.
")
  (description
    "This package provides a RAII scope guard that will run a given closure when it goes out of scope,
even if the code between panics (assuming unwinding panic).

Defines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as
shorthands for guards with one of the implemented strategies.
")
  (license #f))
)

(define-public rust-matches
(package
  (name "rust-matches")
  (version "0.1.8")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "matches" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "020axl4q7rk9vz90phs7f8jas4imxal9y9kxl4z4v7a6719mrz3z"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/SimonSapin/rust-std-candidates")
  (synopsis
    "A macro to evaluate, as a boolean, whether an expression matches a pattern.")
  (description
    "This package provides a macro to evaluate, as a boolean, whether an expression matches a pattern.")
  (license #f))
)

(define-public rust-nodrop
(package
  (name "rust-nodrop")
  (version "0.1.13")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "nodrop" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0if9ifn6rvar5jirx4b3qh4sl5kjkmcifycvzhxa9j3crkfng5ig"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-nodrop-union" ,rust-nodrop-union "src")))
  (home-page "https://github.com/bluss/arrayvec")
  (synopsis
    "A wrapper type to inhibit drop (destructor). Use std::mem::ManuallyDrop instead!")
  (description
    "This package provides a wrapper type to inhibit drop (destructor).  Use std::mem::ManuallyDrop instead!")
  (license #f))
)

(define-public rust-nodrop-union
(package
  (name "rust-nodrop-union")
  (version "0.1.10")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "nodrop-union" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0jsnkdn9l8jlmb9h4wssi76sxnyxwnyi00p6y1p2gdq7c1gdw2b7"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/bluss/arrayvec")
  (synopsis
    "A wrapper type to inhibit drop (destructor). Implementation crate for nodrop, the untagged unions implementation (which is unstable / requires nightly) as of this writing.")
  (description
    "This package provides a wrapper type to inhibit drop (destructor).  Implementation crate for nodrop, the untagged unions implementation (which is unstable / requires nightly) as of this writing.")
  (license #f))
)

(define-public rust-rustc-version
(package
  (name "rust-rustc-version")
  (version "0.2.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc_version" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))))
  (build-system cargo-build-system)
  (inputs `(("rust-semver" ,rust-semver "src")))
  (home-page
    "https://github.com/Kimundi/rustc-version-rs")
  (synopsis
    "A library for querying the version of a installed rustc compiler")
  (description
    "This package provides a library for querying the version of a installed rustc compiler")
  (license #f))
)

(define-public rust-semver
(package
  (name "rust-semver")
  (version "0.9.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "semver" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "00q4lkcj0rrgbhviv9sd4p6qmdsipkwkbra7rh11jrhq5kpvjzhx"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-crates-index" ,rust-crates-index "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-semver-parser" ,rust-semver-parser "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://docs.rs/crate/semver/")
  (synopsis
    "Semantic version parsing and comparison.
")
  (description
    "Semantic version parsing and comparison.
")
  (license #f))
)

(define-public rust-crates-index
(package
  (name "rust-crates-index")
  (version "0.13.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crates-index" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ad3ga7nc351vqmycchfcs8w8hasg56f0d1jzvck930x9ayx0hzb"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-error-chain" ,rust-error-chain "src")
      ("rust-git2" ,rust-git2 "src")
      ("rust-glob" ,rust-glob "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (home-page
    "https://github.com/frewsxcv/rust-crates-index")
  (synopsis
    "Library for retrieving and interacting with the crates.io index")
  (description
    "Library for retrieving and interacting with the crates.io index")
  (license #f))
)

(define-public rust-semver-parser
(package
  (name "rust-semver-parser")
  (version "0.9.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "semver-parser" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1ahqhvgpzhcsd28id7xnrjv4419i9yyalhm7d7zi430qx0hi2vml"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/steveklabnik/semver-parser")
  (synopsis "Parsing of the semver spec.
")
  (description "Parsing of the semver spec.
")
  (license #f))
)

(define-public rust-error-chain
(package
  (name "rust-error-chain")
  (version "0.12.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "error-chain" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "00kfj096lq5y19rlhandl9nn110kxxjvciiqqiviq94npv9r3rq7"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-backtrace" ,rust-backtrace "src")))
  (home-page
    "https://github.com/rust-lang-nursery/error-chain")
  (synopsis
    "Yet another error boilerplate library.")
  (description
    "Yet another error boilerplate library.")
  (license #f))
)

(define-public rust-git2
(package
  (name "rust-git2")
  (version "0.8.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "git2" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1c0a16k6gwlpmy901f9z8ndli3qzs5h1aca468i00jm1pwlr6cy7"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-docopt" ,rust-docopt "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-tempdir" ,rust-tempdir "src")
      ("rust-thread-id" ,rust-thread-id "src")
      ("rust-time" ,rust-time "src")))
  (inputs
    `(("rust-bitflags" ,rust-bitflags "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-libgit2-sys" ,rust-libgit2-sys "src")
      ("rust-log" ,rust-log "src")
      ("rust-openssl-probe" ,rust-openssl-probe "src")
      ("rust-openssl-sys" ,rust-openssl-sys "src")
      ("rust-url" ,rust-url "src")))
  (home-page
    "https://github.com/alexcrichton/git2-rs")
  (synopsis
    "Bindings to libgit2 for interoperating with git repositories. This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.
")
  (description
    "Bindings to libgit2 for interoperating with git repositories.  This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.
")
  (license #f))
)

(define-public rust-glob
(package
  (name "rust-glob")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "glob" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0x25wfr7vg3mzxc9x05dcphvd3nwlcmbnxrvwcvrrdwplcrrk4cv"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (home-page "https://github.com/rust-lang/glob")
  (synopsis
    "Support for matching file paths against Unix shell style patterns.
")
  (description
    "Support for matching file paths against Unix shell style patterns.
")
  (license #f))
)

(define-public rust-backtrace
(package
  (name "rust-backtrace")
  (version "0.3.15")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "backtrace" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ds6lvdhsfhn3kvhin7kp9yr2mabni3wqdjxvz0cvbq46qmc01pi"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-autocfg" ,rust-autocfg "src")))
  (inputs
    `(("rust-addr2line" ,rust-addr2line "src")
      ("rust-backtrace-sys" ,rust-backtrace-sys "src")
      ("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-cpp-demangle" ,rust-cpp-demangle "src")
      ("rust-findshlibs" ,rust-findshlibs "src")
      ("rust-gimli" ,rust-gimli "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-memmap" ,rust-memmap "src")
      ("rust-object" ,rust-object "src")
      ("rust-rustc-demangle"
       ,rust-rustc-demangle
       "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/alexcrichton/backtrace-rs")
  (synopsis
    "A library to acquire a stack trace (backtrace) at runtime in a Rust program.
")
  (description
    "This package provides a library to acquire a stack trace (backtrace) at runtime in a Rust program.
")
  (license #f))
)

(define-public rust-addr2line
(package
  (name "rust-addr2line")
  (version "0.8.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "addr2line" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "088xddd9bgqphwslmdvgbgvgwx0xd2bqmwi3j0djff620djjv54k"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-backtrace" ,rust-backtrace "src")
      ("rust-clap" ,rust-clap "src")
      ("rust-findshlibs" ,rust-findshlibs "src")
      ("rust-memmap" ,rust-memmap "src")
      ("rust-rustc-test" ,rust-rustc-test "src")))
  (inputs
    `(("rust-cpp-demangle" ,rust-cpp-demangle "src")
      ("rust-fallible-iterator"
       ,rust-fallible-iterator
       "src")
      ("rust-gimli" ,rust-gimli "src")
      ("rust-intervaltree" ,rust-intervaltree "src")
      ("rust-lazycell" ,rust-lazycell "src")
      ("rust-object" ,rust-object "src")
      ("rust-rustc-demangle"
       ,rust-rustc-demangle
       "src")
      ("rust-smallvec" ,rust-smallvec "src")))
  (home-page
    "https://github.com/gimli-rs/addr2line")
  (synopsis
    "A cross-platform symbolication library written in Rust, using `gimli`")
  (description
    "This package provides a cross-platform symbolication library written in Rust, using `gimli`")
  (license #f))
)

(define-public rust-backtrace-sys
(package
  (name "rust-backtrace-sys")
  (version "0.1.28")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "backtrace-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1dk9sx6sbm73aihlln8l2m9ia1s4vqmqdfd7z2kr5k2wq8586z3r"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/alexcrichton/backtrace-rs")
  (synopsis
    "Bindings to the libbacktrace gcc library
")
  (description
    "Bindings to the libbacktrace gcc library
")
  (license #f))
)

(define-public rust-cpp-demangle
(package
  (name "rust-cpp-demangle")
  (version "0.2.12")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cpp_demangle" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0a4hqsfc0sfdwy7pcr0rc1fjp2j47fxbkqfc2lfrbi4zlm5hq36k"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-clap" ,rust-clap "src")
      ("rust-diff" ,rust-diff "src")
      ("rust-glob" ,rust-glob "src")))
  (inputs
    `(("rust-afl" ,rust-afl "src")
      ("rust-cfg-if" ,rust-cfg-if "src")))
  (home-page
    "https://github.com/gimli-rs/cpp_demangle")
  (synopsis "A crate for demangling C++ symbols")
  (description
    "This package provides a crate for demangling C++ symbols")
  (license #f))
)

(define-public rust-findshlibs
(package
  (name "rust-findshlibs")
  (version "0.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "findshlibs" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "045csyaxhygdiwsr21mqcd9m4c3r270xg3vrv6rssaz5nzwmhzrg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bindgen" ,rust-bindgen "src")
      ("rust-cfg-if" ,rust-cfg-if "src")))
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/gimli-rs/findshlibs")
  (synopsis
    "Find the set of shared libraries loaded in the current process with a cross platform API")
  (description
    "Find the set of shared libraries loaded in the current process with a cross platform API")
  (license #f))
)

(define-public rust-gimli
(package
  (name "rust-gimli")
  (version "0.17.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "gimli" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1wx4y6cjhfy2bzm14w8vpsisq7dx6lpn09qdl2d3wxx3ihhl6cpb"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-crossbeam" ,rust-crossbeam "src")
      ("rust-getopts" ,rust-getopts "src")
      ("rust-memmap" ,rust-memmap "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-object" ,rust-object "src")
      ("rust-rayon" ,rust-rayon "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-test-assembler"
       ,rust-test-assembler
       "src")
      ("rust-typed-arena" ,rust-typed-arena "src")))
  (inputs
    `(("rust-arrayvec" ,rust-arrayvec "src")
      ("rust-byteorder" ,rust-byteorder "src")
      ("rust-fallible-iterator"
       ,rust-fallible-iterator
       "src")
      ("rust-indexmap" ,rust-indexmap "src")
      ("rust-stable-deref-trait"
       ,rust-stable-deref-trait
       "src")))
  (home-page "https://github.com/gimli-rs/gimli")
  (synopsis
    "A library for reading and writing the DWARF debugging format.")
  (description
    "This package provides a library for reading and writing the DWARF debugging format.")
  (license #f))
)

(define-public rust-memmap
(package
  (name "rust-memmap")
  (version "0.7.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "memmap" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ns7kkd1h4pijdkwfvw4qlbbmqmlmzwlq3g2676dcl5vwyazv1b5"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/danburkert/memmap-rs")
  (synopsis
    "Cross-platform Rust API for memory-mapped file IO")
  (description
    "Cross-platform Rust API for memory-mapped file IO")
  (license #f))
)

(define-public rust-object
(package
  (name "rust-object")
  (version "0.11.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "object" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0whvxpk9d0hp5bm0l1yx8c7b85xry4yj9v96g5ivinb9fqk38y9f"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-memmap" ,rust-memmap "src")))
  (inputs
    `(("rust-flate2" ,rust-flate2 "src")
      ("rust-goblin" ,rust-goblin "src")
      ("rust-parity-wasm" ,rust-parity-wasm "src")
      ("rust-scroll" ,rust-scroll "src")
      ("rust-uuid" ,rust-uuid "src")))
  (home-page "https://github.com/gimli-rs/object")
  (synopsis
    "A unified interface for parsing object file formats.")
  (description
    "This package provides a unified interface for parsing object file formats.")
  (license #f))
)

(define-public rust-rustc-demangle
(package
  (name "rust-rustc-demangle")
  (version "0.1.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-demangle" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "123jlmvra36pk9rw5pm8mxpjv9dibjg8kkzzkklg6yydbbyqpiyc"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-compiler-builtins"
       ,rust-compiler-builtins
       "src")
      ("rust-rustc-std-workspace-core"
       ,rust-rustc-std-workspace-core
       "src")))
  (home-page
    "https://github.com/alexcrichton/rustc-demangle")
  (synopsis "Rust compiler symbol demangling.
")
  (description
    "Rust compiler symbol demangling.
")
  (license #f))
)

(define-public rust-clap
(package
  (name "rust-clap")
  (version "2.33.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "clap" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nf6ld3bims1n5vfzhkvcb55pdzh04bbhzf8nil5vvw05nxzarsh"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-version-sync" ,rust-version-sync "src")))
  (inputs
    `(("rust-ansi-term" ,rust-ansi-term "src")
      ("rust-atty" ,rust-atty "src")
      ("rust-bitflags" ,rust-bitflags "src")
      ("rust-clippy" ,rust-clippy "src")
      ("rust-strsim" ,rust-strsim "src")
      ("rust-term-size" ,rust-term-size "src")
      ("rust-textwrap" ,rust-textwrap "src")
      ("rust-unicode-width" ,rust-unicode-width "src")
      ("rust-vec-map" ,rust-vec-map "src")
      ("rust-yaml-rust" ,rust-yaml-rust "src")))
  (home-page "https://clap.rs/")
  (synopsis
    "A simple to use, efficient, and full-featured Command Line Argument Parser
")
  (description
    "This package provides a simple to use, efficient, and full-featured Command Line Argument Parser
")
  (license #f))
)

(define-public rust-rustc-test
(package
  (name "rust-rustc-test")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-test" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0a27mlcg0ck0hgsdvwk792x9z1k1qq1wj091f1l5yggbdbcsnx5w"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rustc-version" ,rust-rustc-version "src")))
  (inputs
    `(("rust-getopts" ,rust-getopts "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-term" ,rust-term "src")
      ("rust-time" ,rust-time "src")))
  (home-page
    "https://github.com/SimonSapin/rustc-test")
  (synopsis
    "A fork of Rustâ\x80\x99s `test` crate that doesnâ\x80\x99t require unstable language features.")
  (description
    "This package provides a fork of Rustâ\x80\x99s `test` crate that doesnâ\x80\x99t require unstable language features.")
  (license #f))
)

(define-public rust-fallible-iterator
(package
  (name "rust-fallible-iterator")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fallible-iterator" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1xq759lsr8gqss7hva42azn3whgrbrs2sd9xpn92c5ickxm1fhs4"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/sfackler/rust-fallible-iterator")
  (synopsis "Fallible iterator traits")
  (description "Fallible iterator traits")
  (license #f))
)

(define-public rust-intervaltree
(package
  (name "rust-intervaltree")
  (version "0.2.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "intervaltree" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10k40gsv79kwnsqrzwmnmm6psa5fqws8yggavmbggvymv16hffdg"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-smallvec" ,rust-smallvec "src")))
  (home-page
    "https://github.com/main--/rust-intervaltree")
  (synopsis
    "A simple and generic implementation of an immutable interval tree.")
  (description
    "This package provides a simple and generic implementation of an immutable interval tree.")
  (license #f))
)

(define-public rust-lazycell
(package
  (name "rust-lazycell")
  (version "1.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "lazycell" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0gvqycmpv7parc98i6y64ai7rvxrn1947z2a6maa02g4kvxdd55j"))))
  (build-system cargo-build-system)
  (inputs `(("rust-clippy" ,rust-clippy "src")))
  (home-page "https://github.com/indiv0/lazycell")
  (synopsis
    "A library providing a lazily filled Cell struct")
  (description
    "This package provides a library providing a lazily filled Cell struct")
  (license #f))
)

(define-public rust-smallvec
(package
  (name "rust-smallvec")
  (version "0.6.9")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "smallvec" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1gh2j3546vxvz60zw0sj98sxmyj8ixv5f8lq64vl17f4a3lqlj64"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bincode" ,rust-bincode "src")))
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/servo/rust-smallvec")
  (synopsis
    "'Small vector' optimization: store up to a small number of items on the stack")
  (description
    "'Small vector' optimization: store up to a small number of items on the stack")
  (license #f))
)

(define-public rust-version-sync
(package
  (name "rust-version-sync")
  (version "0.8.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "version-sync" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "01pq0ia7ak7d69c3chjgdmaaq271yrspgbzmk6wmrwb74hx3skw4"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-itertools" ,rust-itertools "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-pulldown-cmark"
       ,rust-pulldown-cmark
       "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-semver-parser" ,rust-semver-parser "src")
      ("rust-syn" ,rust-syn "src")
      ("rust-toml" ,rust-toml "src")
      ("rust-url" ,rust-url "src")))
  (home-page
    "https://github.com/mgeisler/version-sync")
  (synopsis
    "Simple crate for ensuring that version numbers in README files are
updated when the crate version changes.
")
  (description
    "Simple crate for ensuring that version numbers in README files are
updated when the crate version changes.
")
  (license #f))
)

(define-public rust-bitflags
(package
  (name "rust-bitflags")
  (version "1.0.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bitflags" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "04nfhscc9mxwhmai5xgwh4q458rjszmwsvkpf752g1j6dyklg012"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/bitflags/bitflags")
  (synopsis
    "A macro to generate structures which behave like bitflags.
")
  (description
    "This package provides a macro to generate structures which behave like bitflags.
")
  (license #f))
)

(define-public rust-term-size
(package
  (name "rust-term-size")
  (version "1.0.0-beta1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "term_size" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "13w9cqjhzh3mmx6zami8lxyf42xx53yy866zxhxqcm71k637v8d8"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-clippy" ,rust-clippy "src")
      ("rust-kernel32-sys" ,rust-kernel32-sys "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/kbknapp/term_size-rs.git")
  (synopsis
    "functions for determining terminal sizes and dimensions")
  (description
    "functions for determining terminal sizes and dimensions")
  (license #f))
)

(define-public rust-textwrap
(package
  (name "rust-textwrap")
  (version "0.11.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "textwrap" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lipsum" ,rust-lipsum "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-rand-xorshift" ,rust-rand-xorshift "src")
      ("rust-version-sync" ,rust-version-sync "src")))
  (inputs
    `(("rust-hyphenation" ,rust-hyphenation "src")
      ("rust-term-size" ,rust-term-size "src")
      ("rust-unicode-width" ,rust-unicode-width "src")))
  (home-page
    "https://github.com/mgeisler/textwrap")
  (synopsis
    "Textwrap is a small library for word wrapping, indenting, and
dedenting strings.

You can use it to format strings (such as help and error messages) for
display in commandline applications. It is designed to be efficient
and handle Unicode characters correctly.
")
  (description
    "Textwrap is a small library for word wrapping, indenting, and
dedenting strings.

You can use it to format strings (such as help and error messages) for
display in commandline applications.  It is designed to be efficient
and handle Unicode characters correctly.
")
  (license #f))
)

(define-public rust-unicode-width
(package
  (name "rust-unicode-width")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-width" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09k5lipygardwy0660jhls08fsgknrazzivmn804gps53hiqc8w8"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/unicode-rs/unicode-width")
  (synopsis
    "Determine displayed width of `char` and `str` types
according to Unicode Standard Annex #11 rules.
")
  (description
    "Determine displayed width of `char` and `str` types
according to Unicode Standard Annex #11 rules.
")
  (license #f))
)

(define-public rust-vec-map
(package
  (name "rust-vec-map")
  (version "0.8.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "vec_map" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "06n8hw4hlbcz328a3gbpvmy0ma46vg1lc0r5wf55900szf3qdiq5"))))
  (build-system cargo-build-system)
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/contain-rs/vec-map")
  (synopsis
    "A simple map based on a vector for small integer keys")
  (description
    "This package provides a simple map based on a vector for small integer keys")
  (license #f))
)

(define-public rust-yaml-rust
(package
  (name "rust-yaml-rust")
  (version "0.4.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "yaml-rust" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ka3qhqc5lvk3hz14wmsj32jhmh44blcbfrx5hfxli2gg38kv4k5"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")))
  (inputs
    `(("rust-linked-hash-map"
       ,rust-linked-hash-map
       "src")))
  (home-page
    "http://chyh1990.github.io/yaml-rust/")
  (synopsis "The missing YAML 1.2 parser for rust")
  (description
    "The missing YAML 1.2 parser for rust")
  (license #f))
)

(define-public rust-pulldown-cmark
(package
  (name "rust-pulldown-cmark")
  (version "0.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pulldown-cmark" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1db8vlhm3n72051bkq4am80q28rfrh88796i3y9ajf5hhk3lrdyi"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-html5ever" ,rust-html5ever "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-tendril" ,rust-tendril "src")))
  (inputs
    `(("rust-bitflags" ,rust-bitflags "src")
      ("rust-getopts" ,rust-getopts "src")
      ("rust-memchr" ,rust-memchr "src")
      ("rust-unicase" ,rust-unicase "src")))
  (home-page
    "https://github.com/raphlinus/pulldown-cmark")
  (synopsis "A pull parser for CommonMark")
  (description
    "This package provides a pull parser for CommonMark")
  (license #f))
)

(define-public rust-toml
(package
  (name "rust-toml")
  (version "0.5.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "toml" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "17mk2zdj5mygvsqh8hbi0vrihb58fwxcdd5wqz6px94zk058kic7"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs
    `(("rust-linked-hash-map"
       ,rust-linked-hash-map
       "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/alexcrichton/toml-rs")
  (synopsis
    "A native Rust encoder and decoder of TOML-formatted files and streams. Provides
implementations of the standard Serialize/Deserialize traits for TOML data to
facilitate deserializing and serializing Rust structures.
")
  (description
    "This package provides a native Rust encoder and decoder of TOML-formatted files and streams.  Provides
implementations of the standard Serialize/Deserialize traits for TOML data to
facilitate deserializing and serializing Rust structures.
")
  (license #f))
)

(define-public rust-url
(package
  (name "rust-url")
  (version "1.7.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "url" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0nim1c90mxpi9wgdw2xh8dqd72vlklwlzam436akcrhjac6pqknx"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bencher" ,rust-bencher "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-rustc-test" ,rust-rustc-test "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs
    `(("rust-encoding" ,rust-encoding "src")
      ("rust-heapsize" ,rust-heapsize "src")
      ("rust-idna" ,rust-idna "src")
      ("rust-matches" ,rust-matches "src")
      ("rust-percent-encoding"
       ,rust-percent-encoding
       "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/servo/rust-url")
  (synopsis
    "URL library for Rust, based on the WHATWG URL Standard")
  (description
    "URL library for Rust, based on the WHATWG URL Standard")
  (license #f))
)

(define-public rust-html5ever
(package
  (name "rust-html5ever")
  (version "0.22.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "html5ever" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0vn2hj096dkp0s4lxv4j7j48wpdhfjx5ry2l5xaxmhcdc5mgl4y2"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-rustc-test" ,rust-rustc-test "src")
      ("rust-syn" ,rust-syn "src")
      ("rust-typed-arena" ,rust-typed-arena "src")))
  (inputs
    `(("rust-log" ,rust-log "src")
      ("rust-mac" ,rust-mac "src")
      ("rust-markup5ever" ,rust-markup5ever "src")))
  (home-page "https://github.com/servo/html5ever")
  (synopsis
    "High-performance browser-grade HTML5 parser")
  (description
    "High-performance browser-grade HTML5 parser")
  (license #f))
)

(define-public rust-tendril
(package
  (name "rust-tendril")
  (version "0.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tendril" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0fsx7blrrzgca8aa2yqy8zxyi8s7amskhgkk1ml5sbaqyalyszvh"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-encoding" ,rust-encoding "src")
      ("rust-encoding-rs" ,rust-encoding-rs "src")
      ("rust-futf" ,rust-futf "src")
      ("rust-mac" ,rust-mac "src")
      ("rust-utf-8" ,rust-utf-8 "src")))
  (home-page "https://github.com/servo/tendril")
  (synopsis
    "Compact buffer/string type for zero-copy parsing")
  (description
    "Compact buffer/string type for zero-copy parsing")
  (license #f))
)

(define-public rust-getopts
(package
  (name "rust-getopts")
  (version "0.2.18")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "getopts" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "15xpa5ljczsylavlnm7y2v2y91iaa41drxalncj59yrj079r4wha"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-log" ,rust-log "src")))
  (inputs
    `(("rust-unicode-width" ,rust-unicode-width "src")))
  (home-page
    "https://github.com/rust-lang/getopts")
  (synopsis "getopts-like option parsing.
")
  (description "getopts-like option parsing.
")
  (license #f))
)

(define-public rust-unicase
(package
  (name "rust-unicase")
  (version "2.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "unicase" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1gak4p97z8g41pcdbql1dcbdxibgy8v9anx4f158xnl7z08p5la1"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-version-check" ,rust-version-check "src")))
  (home-page
    "https://github.com/seanmonstar/unicase")
  (synopsis
    "A case-insensitive wrapper around strings.")
  (description
    "This package provides a case-insensitive wrapper around strings.")
  (license #f))
)

(define-public rust-typed-arena
(package
  (name "rust-typed-arena")
  (version "1.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "typed-arena" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1i8yczhwcy0nnrxqck1lql3i7hvg95l0vw0dbgfb92zkms96mh66"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/SimonSapin/rust-typed-arena")
  (synopsis
    "The arena, a fast but limited type of allocator")
  (description
    "The arena, a fast but limited type of allocator")
  (license #f))
)

(define-public rust-mac
(package
  (name "rust-mac")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "mac" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "194vc7vrshqff72rl56f9xgb0cazyl4jda7qsv31m5l6xx7hq7n4"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/reem/rust-mac.git")
  (synopsis
    "A collection of great and ubiqutitous macros.")
  (description
    "This package provides a collection of great and ubiqutitous macros.")
  (license #f))
)

(define-public rust-markup5ever
(package
  (name "rust-markup5ever")
  (version "0.7.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "markup5ever" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1zv2k29zkpf2nb54c20ghs9r7p2kxn1hcm550m4yyghchpwkcxl9"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-phf-codegen" ,rust-phf-codegen "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-string-cache-codegen"
       ,rust-string-cache-codegen
       "src")))
  (inputs
    `(("rust-phf" ,rust-phf "src")
      ("rust-string-cache" ,rust-string-cache "src")
      ("rust-tendril" ,rust-tendril "src")))
  (home-page "https://github.com/servo/html5ever")
  (synopsis
    "Common code for xml5ever and html5ever")
  (description
    "Common code for xml5ever and html5ever")
  (license #f))
)

(define-public rust-phf-codegen
(package
  (name "rust-phf-codegen")
  (version "0.7.24")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "phf_codegen" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0zjiblicfm0nrmr2xxrs6pnf6zz2394wgch6dcbd8jijkq98agmh"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-phf-generator" ,rust-phf-generator "src")
      ("rust-phf-shared" ,rust-phf-shared "src")))
  (home-page
    "https://github.com/sfackler/rust-phf")
  (synopsis "Codegen library for PHF types")
  (description "Codegen library for PHF types")
  (license #f))
)

(define-public rust-string-cache-codegen
(package
  (name "rust-string-cache-codegen")
  (version "0.4.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "string_cache_codegen" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1npl9zq9cd16d7irksblgk7l7g6qknnzsmr12hrhky2fcpp1xshy"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-phf-generator" ,rust-phf-generator "src")
      ("rust-phf-shared" ,rust-phf-shared "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-string-cache-shared"
       ,rust-string-cache-shared
       "src")))
  (home-page
    "https://github.com/servo/string-cache")
  (synopsis
    "A codegen library for string-cache, developed as part of the Servo project.")
  (description
    "This package provides a codegen library for string-cache, developed as part of the Servo project.")
  (license #f))
)

(define-public rust-phf
(package
  (name "rust-phf")
  (version "0.7.24")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "phf" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "066xwv4dr6056a9adlkarwp4n94kbpwngbmd47ngm3cfbyw49nmk"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-phf-macros" ,rust-phf-macros "src")
      ("rust-phf-shared" ,rust-phf-shared "src")))
  (home-page
    "https://github.com/sfackler/rust-phf")
  (synopsis
    "Runtime support for perfect hash function data structures")
  (description
    "Runtime support for perfect hash function data structures")
  (license #f))
)

(define-public rust-string-cache
(package
  (name "rust-string-cache")
  (version "0.7.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "string_cache" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "08sly9s92l0g0ai1iyj9pawl05xbwm4m8kl3zqkv2wkijw4h3mr5"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rand" ,rust-rand "src")
      ("rust-string-cache-codegen"
       ,rust-string-cache-codegen
       "src")))
  (inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-new-debug-unreachable"
       ,rust-new-debug-unreachable
       "src")
      ("rust-phf-shared" ,rust-phf-shared "src")
      ("rust-precomputed-hash"
       ,rust-precomputed-hash
       "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-string-cache-shared"
       ,rust-string-cache-shared
       "src")))
  (home-page
    "https://github.com/servo/string-cache")
  (synopsis
    "A string interning library for Rust, developed as part of the Servo project.")
  (description
    "This package provides a string interning library for Rust, developed as part of the Servo project.")
  (license #f))
)

(define-public rust-phf-generator
(package
  (name "rust-phf-generator")
  (version "0.7.24")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "phf_generator" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0qi62gxk3x3whrmw5c4i71406icqk11qmpgln438p6qm7k4lqdh9"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-phf-shared" ,rust-phf-shared "src")
      ("rust-rand" ,rust-rand "src")))
  (home-page
    "https://github.com/sfackler/rust-phf")
  (synopsis "PHF generation logic")
  (description "PHF generation logic")
  (license #f))
)

(define-public rust-phf-shared
(package
  (name "rust-phf-shared")
  (version "0.7.24")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "phf_shared" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "18371fla0vsj7d6d5rlfb747xbr2in11ar9vgv5qna72bnhp2kr3"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-siphasher" ,rust-siphasher "src")
      ("rust-unicase" ,rust-unicase "src")))
  (home-page
    "https://github.com/sfackler/rust-phf")
  (synopsis "Support code shared by PHF libraries")
  (description
    "Support code shared by PHF libraries")
  (license #f))
)

(define-public rust-siphasher
(package
  (name "rust-siphasher")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "siphasher" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1a03vzazh0wpds6nmclpnkq3z1mj1dmh3738z81lmn2pyrfwf4wr"))))
  (build-system cargo-build-system)
  (home-page "https://docs.rs/siphasher")
  (synopsis
    "SipHash functions from rust-core < 1.13")
  (description
    "SipHash functions from rust-core < 1.13")
  (license #f))
)

(define-public rust-version-check
(package
  (name "rust-version-check")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "version_check" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1pf91pvj8n6akh7w6j5ypka6aqz08b3qpzgs0ak2kjf4frkiljwi"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/SergioBenitez/version_check")
  (synopsis
    "Tiny crate to check the version of the installed/running rustc.")
  (description
    "Tiny crate to check the version of the installed/running rustc.")
  (license #f))
)

(define-public rust-automod
(package
  (name "rust-automod")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "automod" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0pld582piq2d55z0j96zcs8izw3ml46f8h9y7sdyxg093yfvxl2h"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://github.com/dtolnay/automod")
  (synopsis
    "Pull in every source file in a directory as a module.")
  (description
    "Pull in every source file in a directory as a module.")
  (license #f))
)

(define-public rust-compiletest-rs
(package
  (name "rust-compiletest-rs")
  (version "0.3.21")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "compiletest_rs" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1cb1pd1ym1zyinbh8aj30ykr72jx7nh5hx5pnx6m7lflny1h99hp"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-diff" ,rust-diff "src")
      ("rust-filetime" ,rust-filetime "src")
      ("rust-getopts" ,rust-getopts "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-log" ,rust-log "src")
      ("rust-miow" ,rust-miow "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-rustfix" ,rust-rustfix "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-tempfile" ,rust-tempfile "src")
      ("rust-tester" ,rust-tester "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/laumann/compiletest-rs")
  (synopsis
    "The compiletest utility from the Rust compiler as a standalone testing harness")
  (description
    "The compiletest utility from the Rust compiler as a standalone testing harness")
  (license #f))
)

(define-public rust-serde-stacker
(package
  (name "rust-serde-stacker")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_stacker" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1jn54i5m1mlc6nm47f96k85fgjs9mhpbbqa4dvd5xjbivkdw55ic"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-json" ,rust-serde-json "src")))
  (inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-stacker" ,rust-stacker "src")))
  (home-page
    "https://github.com/dtolnay/serde-stacker")
  (synopsis
    "Serde adapter that avoids stack overflow by dynamically growing the stack")
  (description
    "Serde adapter that avoids stack overflow by dynamically growing the stack")
  (license #f))
)

(define-public rust-indexmap
(package
  (name "rust-indexmap")
  (version "1.0.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "indexmap" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "13f5k1kl2759y4xfy0vhays35fmrkmhqngbr2ny8smvrbz0ag0by"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-fnv" ,rust-fnv "src")
      ("rust-itertools" ,rust-itertools "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-serde-test" ,rust-serde-test "src")))
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/bluss/indexmap")
  (synopsis
    "A hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the key-value
pairs is independent of the hash values of the keys. It has the usual
hash table functionality, it preserves insertion order except after
removals, and it allows lookup of its elements by either hash table key
or numerical index. A corresponding hash set type is also provided.

This crate was initially published under the name ordermap, but it was renamed to
indexmap.
")
  (description
    "This package provides a hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the key-value
pairs is independent of the hash values of the keys.  It has the usual
hash table functionality, it preserves insertion order except after
removals, and it allows lookup of its elements by either hash table key
or numerical index.  A corresponding hash set type is also provided.

This crate was initially published under the name ordermap, but it was renamed to
indexmap.
")
  (license #f))
)

(define-public rust-itoa
(package
  (name "rust-itoa")
  (version "0.4.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "itoa" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "02z6pfmppv4n5zcnz2aydqijvmgvg4fd6wr3s4q0xwsi953g61hk"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/dtolnay/itoa")
  (synopsis
    "Fast functions for printing integer primitives to an io::Write")
  (description
    "Fast functions for printing integer primitives to an io::Write")
  (license #f))
)

(define-public rust-ryu
(package
  (name "rust-ryu")
  (version "0.2.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ryu" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1xs6ibjhfhbgki1yk3la3c93zdmz349nim1dlkk9yai8vs69p7pb"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-no-panic" ,rust-no-panic "src")))
  (home-page "https://github.com/dtolnay/ryu")
  (synopsis
    "Fast floating point to string conversion")
  (description
    "Fast floating point to string conversion")
  (license #f))
)

(define-public rust-diff
(package
  (name "rust-diff")
  (version "0.1.11")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "diff" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0fhavni46a2rib93ig5fgbqmm48ysms5sxzb3h9bp7vp2bwnjarw"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-speculate" ,rust-speculate "src")))
  (home-page
    "https://github.com/utkarshkukreti/diff.rs")
  (synopsis
    "An LCS based slice and string diffing implementation.")
  (description
    "An LCS based slice and string diffing implementation.")
  (license #f))
)

(define-public rust-filetime
(package
  (name "rust-filetime")
  (version "0.2.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "filetime" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ip66dc93aqr2hyjkzaa745ycxjrwqmc97bq0xvpxqjbihd5rpx2"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-redox-syscall" ,rust-redox-syscall "src")))
  (home-page
    "https://github.com/alexcrichton/filetime")
  (synopsis
    "Platform-agnostic accessors of timestamps in File metadata
")
  (description
    "Platform-agnostic accessors of timestamps in File metadata
")
  (license #f))
)

(define-public rust-miow
(package
  (name "rust-miow")
  (version "0.3.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "miow" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09ljvx6wg30f2xlv7b7hhpkw7k312n3hjgmrbhwzhz9x03ra0sir"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-socket2" ,rust-socket2 "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/alexcrichton/miow")
  (synopsis
    "A zero overhead I/O library for Windows, focusing on IOCP and Async I/O
abstractions.
")
  (description
    "This package provides a zero overhead I/O library for Windows, focusing on IOCP and Async I/O
abstractions.
")
  (license #f))
)

(define-public rust-rustfix
(package
  (name "rust-rustfix")
  (version "0.4.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustfix" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1qk1s622p1d6x9ivqwjrqwihnwlxl46ir7ncghwqix20mvpacvmr"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-difference" ,rust-difference "src")
      ("rust-duct" ,rust-duct "src")
      ("rust-env-logger" ,rust-env-logger "src")
      ("rust-log" ,rust-log "src")
      ("rust-proptest" ,rust-proptest "src")
      ("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-failure" ,rust-failure "src")
      ("rust-log" ,rust-log "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (home-page
    "https://github.com/rust-lang-nursery/rustfix")
  (synopsis
    "Automatically apply the suggestions made by rustc")
  (description
    "Automatically apply the suggestions made by rustc")
  (license #f))
)

(define-public rust-tempfile
(package
  (name "rust-tempfile")
  (version "3.0.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tempfile" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "12pdg3hz7kamx2440v430239vllyh79ssvc0688q136ri167hv5q"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-redox-syscall" ,rust-redox-syscall "src")
      ("rust-remove-dir-all"
       ,rust-remove-dir-all
       "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "http://stebalien.com/projects/tempfile-rs")
  (synopsis
    "A library for managing temporary files and directories.
")
  (description
    "This package provides a library for managing temporary files and directories.
")
  (license #f))
)

(define-public rust-tester
(package
  (name "rust-tester")
  (version "0.5.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tester" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1xkgapz2i4j977f6kh1zp6sa5llbhy5vbnr6kfj8czsrdjr2r0ay"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-getopts" ,rust-getopts "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-term" ,rust-term "src")))
  (home-page
    "https://github.com/messense/rustc-test")
  (synopsis
    "A fork of Rustâ\x80\x99s `test` crate that doesnâ\x80\x99t require unstable language features.")
  (description
    "This package provides a fork of Rustâ\x80\x99s `test` crate that doesnâ\x80\x99t require unstable language features.")
  (license #f))
)

(define-public rust-speculate
(package
  (name "rust-speculate")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "speculate" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0amnsrza5mc5d2c44mh02fail661sx4d1c0lw35nx0b97hx63lms"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")
      ("rust-unicode-xid" ,rust-unicode-xid "src")))
  (home-page
    "https://github.com/utkarshkukreti/speculate.rs")
  (synopsis
    "An RSpec inspired minimal testing framework for Rust.")
  (description
    "An RSpec inspired minimal testing framework for Rust.")
  (license #f))
)

(define-public rust-socket2
(package
  (name "rust-socket2")
  (version "0.3.8")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "socket2" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1rs8kzzykwlx6pc2qws4mfblnl7l2bc1m8yanmydhmr01191mlf4"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-redox-syscall" ,rust-redox-syscall "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/alexcrichton/socket2-rs")
  (synopsis
    "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.
")
  (description
    "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.
")
  (license #f))
)

(define-public rust-difference
(package
  (name "rust-difference")
  (version "2.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "difference" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1621wx4k8h452p6xzmzzvm7mz87kxh4yqz0kzxfjj9xmjxlbyk2j"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-term" ,rust-term "src")))
  (inputs `(("rust-getopts" ,rust-getopts "src")))
  (home-page
    "https://github.com/johannhof/difference.rs")
  (synopsis
    "A Rust text diffing and assertion library.")
  (description
    "This package provides a Rust text diffing and assertion library.")
  (license #f))
)

(define-public rust-duct
(package
  (name "rust-duct")
  (version "0.12.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "duct" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1vm1nzyi434h2zwix7c925qfa886ri1qx4nkq4hdrgkq7h9ayh1n"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-lazycell" ,rust-lazycell "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-os-pipe" ,rust-os-pipe "src")
      ("rust-shared-child" ,rust-shared-child "src")))
  (home-page
    "https://github.com/oconnor663/duct.rs")
  (synopsis
    "a library for creating shell pipelines")
  (description
    "a library for creating shell pipelines")
  (license #f))
)

(define-public rust-proptest
(package
  (name "rust-proptest")
  (version "0.9.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "proptest" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0b5mxp1qvczysdzacb4vkcfnky7byrshk61161zfjfgqn96q9x94"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-regex" ,rust-regex "src")))
  (inputs
    `(("rust-bit-set" ,rust-bit-set "src")
      ("rust-bitflags" ,rust-bitflags "src")
      ("rust-byteorder" ,rust-byteorder "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-num-traits" ,rust-num-traits "src")
      ("rust-quick-error" ,rust-quick-error "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-rand-chacha" ,rust-rand-chacha "src")
      ("rust-rand-xorshift" ,rust-rand-xorshift "src")
      ("rust-regex-syntax" ,rust-regex-syntax "src")
      ("rust-rusty-fork" ,rust-rusty-fork "src")
      ("rust-tempfile" ,rust-tempfile "src")))
  (home-page
    "https://altsysrq.github.io/proptest-book/proptest/index.html")
  (synopsis
    "Hypothesis-like property-based testing and shrinking.
")
  (description
    "Hypothesis-like property-based testing and shrinking.
")
  (license #f))
)

(define-public rust-os-pipe
(package
  (name "rust-os-pipe")
  (version "0.8.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "os_pipe" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0yqmjndfhas4i82g76rmy2s9iz7ddwah5bhfaghal5a576fq27ff"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-nix" ,rust-nix "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/oconnor663/os_pipe.rs")
  (synopsis
    "a cross-platform library for opening OS pipes")
  (description
    "a cross-platform library for opening OS pipes")
  (license #f))
)

(define-public rust-shared-child
(package
  (name "rust-shared-child")
  (version "0.3.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "shared_child" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0j2grl1qb0pwb75zm08agpsllrli1h4gqya0dfw1qfjvcmwls9by"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/oconnor663/shared_child.rs")
  (synopsis
    "a library for using child processes from multiple threads")
  (description
    "a library for using child processes from multiple threads")
  (license #f))
)

(define-public rust-nix
(package
  (name "rust-nix")
  (version "0.13.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "nix" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0arhg1vs8h2jprlmwzqlwz5xlwpidbp71v3rzbl9dmv80whz7w26"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-cc" ,rust-cc "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-sysctl" ,rust-sysctl "src")
      ("rust-tempfile" ,rust-tempfile "src")))
  (inputs
    `(("rust-bitflags" ,rust-bitflags "src")
      ("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-void" ,rust-void "src")))
  (home-page "https://github.com/nix-rust/nix")
  (synopsis "Rust friendly bindings to *nix APIs")
  (description
    "Rust friendly bindings to *nix APIs")
  (license #f))
)

(define-public rust-bytes
(package
  (name "rust-bytes")
  (version "0.4.12")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bytes" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0768a55q2fsqdjsvcv98ndg9dq7w2g44dvq1avhwpxrdzbydyvr0"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-test" ,rust-serde-test "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-either" ,rust-either "src")
      ("rust-iovec" ,rust-iovec "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/carllerche/bytes")
  (synopsis
    "Types and traits for working with bytes")
  (description
    "Types and traits for working with bytes")
  (license #f))
)

(define-public rust-sysctl
(package
  (name "rust-sysctl")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "sysctl" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0s4rb91iw44mzkx71ccr7jx9whvqss54g1py2bfq0m83lf3q97pv"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-bitflags" ,rust-bitflags "src")
      ("rust-byteorder" ,rust-byteorder "src")
      ("rust-failure" ,rust-failure "src")
      ("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/johalun/sysctl-rs")
  (synopsis "Simplified interface to libc::sysctl")
  (description
    "Simplified interface to libc::sysctl")
  (license #f))
)

(define-public rust-void
(package
  (name "rust-void")
  (version "1.0.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "void" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0zc8f0ksxvmhvgx4fdg0zyn6vdnbxd2xv9hfx4nhzg6kbs4f80ka"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/reem/rust-void.git")
  (synopsis
    "The uninhabited void type for use in statically impossible cases.")
  (description
    "The uninhabited void type for use in statically impossible cases.")
  (license #f))
)

(define-public rust-iovec
(package
  (name "rust-iovec")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "iovec" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "025vi072m22299z3fg73qid188z2iip7k41ba6v5v5yhwwby9rnv"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/carllerche/iovec")
  (synopsis
    "Portable buffer type for scatter/gather I/O operations
")
  (description
    "Portable buffer type for scatter/gather I/O operations
")
  (license #f))
)

(define-public rust-failure-derive
(package
  (name "rust-failure-derive")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "failure_derive" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1q97n7dp51j5hndzic9ng2fgn6f3z5ya1992w84l7vypby8n647a"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-failure" ,rust-failure "src")))
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")
      ("rust-synstructure" ,rust-synstructure "src")))
  (home-page
    "https://rust-lang-nursery.github.io/failure/")
  (synopsis "derives for the failure crate")
  (description "derives for the failure crate")
  (license #f))
)

(define-public rust-synstructure
(package
  (name "rust-synstructure")
  (version "0.10.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "synstructure" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "059078h1zsgn8ka9f7dcpql6axy3hbaavh3ar61m8a4rpwwp2s3k"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-synstructure-test-traits"
       ,rust-synstructure-test-traits
       "src")))
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")
      ("rust-unicode-xid" ,rust-unicode-xid "src")))
  (home-page
    "https://github.com/mystor/synstructure")
  (synopsis
    "Helper methods and macros for custom derives")
  (description
    "Helper methods and macros for custom derives")
  (license #f))
)

(define-public rust-synstructure-test-traits
(package
  (name "rust-synstructure-test-traits")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "synstructure_test_traits" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1b3fs2b9kc1gy9dilaxqjbdl4z0mlrbbxjzkprdx953rif1c3q66"))))
  (build-system cargo-build-system)
  (home-page "")
  (synopsis
    "Helper test traits for synstructure doctests")
  (description
    "Helper test traits for synstructure doctests")
  (license #f))
)

(define-public rust-bit-set
(package
  (name "rust-bit-set")
  (version "0.5.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bit-set" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "100ac8867bvbx9kv634w4xjk98b71i8nq4wdcvpf3cf4ha4j6k78"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs `(("rust-bit-vec" ,rust-bit-vec "src")))
  (home-page
    "https://github.com/contain-rs/bit-set")
  (synopsis "A set of bits")
  (description
    "This package provides a set of bits")
  (license #f))
)

(define-public rust-rusty-fork
(package
  (name "rust-rusty-fork")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rusty-fork" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1bjg8adk0i921088j52rn0hmvsry34q19g96x41pamqcw5j35n9x"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-fnv" ,rust-fnv "src")
      ("rust-quick-error" ,rust-quick-error "src")
      ("rust-tempfile" ,rust-tempfile "src")
      ("rust-wait-timeout" ,rust-wait-timeout "src")))
  (home-page
    "https://github.com/altsysrq/rusty-fork")
  (synopsis
    "Cross-platform library for running Rust tests in sub-processes using a
fork-like interface.
")
  (description
    "Cross-platform library for running Rust tests in sub-processes using a
fork-like interface.
")
  (license #f))
)

(define-public rust-bit-vec
(package
  (name "rust-bit-vec")
  (version "0.5.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bit-vec" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1fyh8221s6cxlmng01v8v2ljhavzawqqs8r1xjc66ap5sjavx6zm"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (home-page
    "https://github.com/contain-rs/bit-vec")
  (synopsis "A vector of bits")
  (description
    "This package provides a vector of bits")
  (license #f))
)

(define-public rust-fnv
(package
  (name "rust-fnv")
  (version "1.0.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fnv" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1ww56bi1r5b8id3ns9j3qxbi7w5h005rzhiryy0zi9h97raqbb9g"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/servo/rust-fnv")
  (synopsis
    "Fowlerâ\x80\x93Nollâ\x80\x93Vo hash function")
  (description
    "Fowlerâ\x80\x93Nollâ\x80\x93Vo hash function")
  (license #f))
)

(define-public rust-wait-timeout
(package
  (name "rust-wait-timeout")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wait-timeout" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1xpkk0j5l9pfmjfh1pi0i89invlavfrd9av5xp0zhxgb29dhy84z"))))
  (build-system cargo-build-system)
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/alexcrichton/wait-timeout")
  (synopsis
    "A crate to wait on a child process with a timeout specified across Unix and
Windows platforms.
")
  (description
    "This package provides a crate to wait on a child process with a timeout specified across Unix and
Windows platforms.
")
  (license #f))
)

(define-public rust-stacker
(package
  (name "rust-stacker")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "stacker" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0js0axz5nla1mkr2dm2vrv9rj964ng1lrv4l43sqlnfgawplhygv"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/alexcrichton/stacker")
  (synopsis
    "A stack growth library useful when implementing deeply recursive algorithms that
may accidentally blow the stack.
")
  (description
    "This package provides a stack growth library useful when implementing deeply recursive algorithms that
may accidentally blow the stack.
")
  (license #f))
)

(define-public rust-num-cpus
(package
  (name "rust-num-cpus")
  (version "1.10.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "num_cpus" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1fpvhmikngmg2mwdilbq631c79xdhwnisfz8qyhbljm563nz08qs"))))
  (build-system cargo-build-system)
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/seanmonstar/num_cpus")
  (synopsis "Get the number of CPUs on a machine.")
  (description
    "Get the number of CPUs on a machine.")
  (license #f))
)

(define-public rust-no-panic
(package
  (name "rust-no-panic")
  (version "0.1.8")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "no-panic" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0fhaa0c0v5v1pcikjijqdnqnx77r2bnlg56pb5cv2wvj41b6qkx4"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempfile" ,rust-tempfile "src")))
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://github.com/dtolnay/no-panic")
  (synopsis
    "Attribute macro to require that the compiler prove a function can't ever panic.")
  (description
    "Attribute macro to require that the compiler prove a function can't ever panic.")
  (license #f))
)

(define-public rust-string-cache-shared
(package
  (name "rust-string-cache-shared")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "string_cache_shared" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1z7dpdix1m42x6ddshdcpjf91ml9mhvnskmiv5kd8hcpq0dlv25i"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/servo/string-cache")
  (synopsis
    "Code share between string_cache and string_cache_codegen.")
  (description
    "Code share between string_cache and string_cache_codegen.")
  (license #f))
)

(define-public rust-phf-macros
(package
  (name "rust-phf-macros")
  (version "0.7.24")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "phf_macros" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0dzylcy14ksy60h265l433j9ra8xhg8xlq3pd5qk658m6f1mxd5x"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-compiletest-rs"
       ,rust-compiletest-rs
       "src")))
  (inputs
    `(("rust-phf-generator" ,rust-phf-generator "src")
      ("rust-phf-shared" ,rust-phf-shared "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/sfackler/rust-phf")
  (synopsis
    "Macros to generate types in the phf crate")
  (description
    "Macros to generate types in the phf crate")
  (license #f))
)

(define-public rust-new-debug-unreachable
(package
  (name "rust-new-debug-unreachable")
  (version "1.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "new_debug_unreachable" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0c1br326qa0rrzxrn2rd5ah7xaprig2i9r4rwsx06vnvc1f003zl"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/mbrubeck/rust-debug-unreachable")
  (synopsis
    "panic in debug, intrinsics::unreachable() in release (fork of debug_unreachable)")
  (description
    "panic in debug, intrinsics::unreachable() in release (fork of debug_unreachable)")
  (license #f))
)

(define-public rust-precomputed-hash
(package
  (name "rust-precomputed-hash")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "precomputed-hash" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "075k9bfy39jhs53cb2fpb9klfakx2glxnf28zdw08ws6lgpq6lwj"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/emilio/precomputed-hash")
  (synopsis
    "A library intending to be a base dependency to expose a precomputed hash")
  (description
    "This package provides a library intending to be a base dependency to expose a precomputed hash")
  (license #f))
)

(define-public rust-encoding
(package
  (name "rust-encoding")
  (version "0.2.33")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1v1ndmkarh9z3n5hk53da4z56hgk9wa5kcsm7cnx345raqw983bb"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-getopts" ,rust-getopts "src")))
  (inputs
    `(("rust-encoding-index-japanese"
       ,rust-encoding-index-japanese
       "src")
      ("rust-encoding-index-korean"
       ,rust-encoding-index-korean
       "src")
      ("rust-encoding-index-simpchinese"
       ,rust-encoding-index-simpchinese
       "src")
      ("rust-encoding-index-singlebyte"
       ,rust-encoding-index-singlebyte
       "src")
      ("rust-encoding-index-tradchinese"
       ,rust-encoding-index-tradchinese
       "src")))
  (home-page
    "https://github.com/lifthrasiir/rust-encoding")
  (synopsis "Character encoding support for Rust")
  (description
    "Character encoding support for Rust")
  (license #f))
)

(define-public rust-encoding-rs
(package
  (name "rust-encoding-rs")
  (version "0.8.17")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding_rs" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1v902qqnbd37vdq4rjvp6k05wmghrasfdcjy30gp1xpjg5f7hma1"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bincode" ,rust-bincode "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-packed-simd" ,rust-packed-simd "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://docs.rs/encoding_rs/")
  (synopsis
    "A Gecko-oriented implementation of the Encoding Standard")
  (description
    "This package provides a Gecko-oriented implementation of the Encoding Standard")
  (license #f))
)

(define-public rust-futf
(package
  (name "rust-futf")
  (version "0.1.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futf" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0fxc18bnabird5jl941nsd6d25vq8cn8barmz4d30dlkzbiir73w"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-mac" ,rust-mac "src")
      ("rust-new-debug-unreachable"
       ,rust-new-debug-unreachable
       "src")))
  (home-page "https://github.com/servo/futf")
  (synopsis "Handling fragments of UTF-8")
  (description "Handling fragments of UTF-8")
  (license #f))
)

(define-public rust-utf-8
(package
  (name "rust-utf-8")
  (version "0.7.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "utf-8" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1iw5rp4i3mfi9k51picbr5bgjqhjcmnxx7001clh5ydq31y2zr05"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/SimonSapin/rust-utf8")
  (synopsis
    "Incremental, zero-copy UTF-8 decoding with error handling")
  (description
    "Incremental, zero-copy UTF-8 decoding with error handling")
  (license #f))
)

(define-public rust-encoding-index-japanese
(package
  (name "rust-encoding-index-japanese")
  (version "1.20141219.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding-index-japanese" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "148c1lmd640p1d7fzk0nv7892mbyavvwddgqvcsm78798bzv5s04"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-encoding-index-tests"
       ,rust-encoding-index-tests
       "src")))
  (home-page
    "https://github.com/lifthrasiir/rust-encoding")
  (synopsis
    "Index tables for Japanese character encodings")
  (description
    "Index tables for Japanese character encodings")
  (license #f))
)

(define-public rust-encoding-index-korean
(package
  (name "rust-encoding-index-korean")
  (version "1.20141219.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding-index-korean" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10cxabp5ppygbq4y6y680856zl9zjvq7ahpiw8zj3fmwwsw3zhsd"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-encoding-index-tests"
       ,rust-encoding-index-tests
       "src")))
  (home-page
    "https://github.com/lifthrasiir/rust-encoding")
  (synopsis
    "Index tables for Korean character encodings")
  (description
    "Index tables for Korean character encodings")
  (license #f))
)

(define-public rust-encoding-index-simpchinese
(package
  (name "rust-encoding-index-simpchinese")
  (version "1.20141219.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding-index-simpchinese" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1xria2i7mc5dqdrpqxasdbxv1qx46jjbm53if3y1i4cvj2a72ynq"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-encoding-index-tests"
       ,rust-encoding-index-tests
       "src")))
  (home-page
    "https://github.com/lifthrasiir/rust-encoding")
  (synopsis
    "Index tables for simplified Chinese character encodings")
  (description
    "Index tables for simplified Chinese character encodings")
  (license #f))
)

(define-public rust-encoding-index-singlebyte
(package
  (name "rust-encoding-index-singlebyte")
  (version "1.20141219.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding-index-singlebyte" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0jp85bz2pprzvg9m95w4q0vibh67b6w3bx35lafay95jzyndal9k"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-encoding-index-tests"
       ,rust-encoding-index-tests
       "src")))
  (home-page
    "https://github.com/lifthrasiir/rust-encoding")
  (synopsis
    "Index tables for various single-byte character encodings")
  (description
    "Index tables for various single-byte character encodings")
  (license #f))
)

(define-public rust-encoding-index-tradchinese
(package
  (name "rust-encoding-index-tradchinese")
  (version "1.20141219.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding-index-tradchinese" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "060ci4iz6xfvzk38syfbjvs7pix5hch3mvxkksswmqwcd3aj03px"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-encoding-index-tests"
       ,rust-encoding-index-tests
       "src")))
  (home-page
    "https://github.com/lifthrasiir/rust-encoding")
  (synopsis
    "Index tables for traditional Chinese character encodings")
  (description
    "Index tables for traditional Chinese character encodings")
  (license #f))
)

(define-public rust-encoding-index-tests
(package
  (name "rust-encoding-index-tests")
  (version "0.1.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding_index_tests" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0s85y091gl17ixass49bzaivng7w8p82p6nyvz2r3my9w4mxhim2"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/lifthrasiir/rust-encoding")
  (synopsis
    "Helper macros used to test index tables for character encodings")
  (description
    "Helper macros used to test index tables for character encodings")
  (license #f))
)

(define-public rust-paste
(package
  (name "rust-paste")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "paste" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ygs077hlq8qlx5y46sfgrmhlqqgkmvvhn4x3y10arawalf4ljhz"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-paste-impl" ,rust-paste-impl "src")
      ("rust-proc-macro-hack"
       ,rust-proc-macro-hack
       "src")))
  (home-page "https://github.com/dtolnay/paste")
  (synopsis
    "Macros for all your token pasting needs")
  (description
    "Macros for all your token pasting needs")
  (license #f))
)

(define-public rust-wasm-bindgen
(package
  (name "rust-wasm-bindgen")
  (version "0.2.42")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1m1mbs4mbj83jf7khsg341inbf24sqndc316wcvdjvzswls3bppz"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-wasm-bindgen-macro"
       ,rust-wasm-bindgen-macro
       "src")))
  (home-page "https://rustwasm.github.io/")
  (synopsis
    "Easy support for interacting between JS and Rust.
")
  (description
    "Easy support for interacting between JS and Rust.
")
  (license #f))
)

(define-public rust-wasm-bindgen-test
(package
  (name "rust-wasm-bindgen-test")
  (version "0.2.42")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-test" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "05xxpb4sflcibi0zgcq3qw8qgr53wrnm2g2rzx5fzwjnmbqlxvca"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-console-error-panic-hook"
       ,rust-console-error-panic-hook
       "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-js-sys" ,rust-js-sys "src")
      ("rust-scoped-tls" ,rust-scoped-tls "src")
      ("rust-wasm-bindgen" ,rust-wasm-bindgen "src")
      ("rust-wasm-bindgen-futures"
       ,rust-wasm-bindgen-futures
       "src")
      ("rust-wasm-bindgen-test-macro"
       ,rust-wasm-bindgen-test-macro
       "src")))
  (home-page
    "https://github.com/rustwasm/wasm-bindgen")
  (synopsis
    "Internal testing crate for wasm-bindgen")
  (description
    "Internal testing crate for wasm-bindgen")
  (license #f))
)

(define-public rust-core-arch
(package
  (name "rust-core-arch")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "core_arch" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "04vdvr9vj0f1cv2p54nsszmrrk9w1js4c0z4i0bdlajl1lydslim"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-wasm-bindgen-test"
       ,rust-wasm-bindgen-test
       "src")))
  (home-page
    "https://github.com/rust-lang-nursery/stdsimd")
  (synopsis
    "`core::arch` - Rust's core library architecture-specific intrinsics.")
  (description
    "`core::arch` - Rust's core library architecture-specific intrinsics.")
  (license #f))
)

(define-public rust-sleef-sys
(package
  (name "rust-sleef-sys")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "sleef-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1881q2yc17j2m1yvh01447c93ws1mspnrj3k2nbvwbvcm8z81kkv"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bindgen" ,rust-bindgen "src")
      ("rust-cmake" ,rust-cmake "src")
      ("rust-env-logger" ,rust-env-logger "src")))
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-libc" ,rust-libc "src")))
  (home-page "https://github.com/gnzlbg/sleef-sys")
  (synopsis
    "Rust FFI bindings to the SLEEF Vectorized Math Library
")
  (description
    "Rust FFI bindings to the SLEEF Vectorized Math Library
")
  (license #f))
)

(define-public rust-paste-impl
(package
  (name "rust-paste-impl")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "paste-impl" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1rkh8nixmb7r1y0mjnsz62p6r1bqah5ciri7bwhmgcmq4gk9drr6"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro-hack"
       ,rust-proc-macro-hack
       "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://github.com/dtolnay/paste")
  (synopsis
    "Implementation detail of the `paste` crate")
  (description
    "Implementation detail of the `paste` crate")
  (license #f))
)

(define-public rust-proc-macro-hack
(package
  (name "rust-proc-macro-hack")
  (version "0.5.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "proc-macro-hack" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1g5q5vxpci0hp4d73cl5cjp2w0059p2qcx3dlzim7ks0pjgfv6va"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-demo-hack" ,rust-demo-hack "src")
      ("rust-demo-hack-impl"
       ,rust-demo-hack-impl
       "src")))
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/dtolnay/proc-macro-hack")
  (synopsis
    "Procedural macros in expression position")
  (description
    "Procedural macros in expression position")
  (license #f))
)

(define-public rust-demo-hack
(package
  (name "rust-demo-hack")
  (version "0.0.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "demo-hack" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0m0114p1g0zzrdph5bg03i8m8p70vrwn3whs191jrbjcrmh5lmnp"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-demo-hack-impl"
       ,rust-demo-hack-impl
       "src")
      ("rust-proc-macro-hack"
       ,rust-proc-macro-hack
       "src")))
  (home-page
    "https://github.com/dtolnay/proc-macro-hack")
  (synopsis "Demo of proc-macro-hack")
  (description "Demo of proc-macro-hack")
  (license #f))
)

(define-public rust-demo-hack-impl
(package
  (name "rust-demo-hack-impl")
  (version "0.0.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "demo-hack-impl" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1f1fdl60xjas9wlmcl9v6f56vgm3mzwr019kcifav5464rx3w3ld"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro-hack"
       ,rust-proc-macro-hack
       "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/dtolnay/proc-macro-hack")
  (synopsis "Demo of proc-macro-hack")
  (description "Demo of proc-macro-hack")
  (license #f))
)

(define-public rust-wasm-bindgen-macro
(package
  (name "rust-wasm-bindgen-macro")
  (version "0.2.42")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-macro" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0w8ak7dq7sa5qay474j2i7n5f8f9b6g458hca5j96k2z5i6cj57r"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-quote" ,rust-quote "src")
      ("rust-wasm-bindgen-macro-support"
       ,rust-wasm-bindgen-macro-support
       "src")))
  (home-page
    "https://rustwasm.github.io/wasm-bindgen/")
  (synopsis
    "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
  (description
    "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
  (license #f))
)

(define-public rust-wasm-bindgen-macro-support
(package
  (name "rust-wasm-bindgen-macro-support")
  (version "0.2.42")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-macro-support" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1193iajdlhjj2vypz50xs4a0rc5y8fdg1138ggdk6hhy949w8s4i"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")
      ("rust-wasm-bindgen-backend"
       ,rust-wasm-bindgen-backend
       "src")
      ("rust-wasm-bindgen-shared"
       ,rust-wasm-bindgen-shared
       "src")))
  (home-page
    "https://rustwasm.github.io/wasm-bindgen/")
  (synopsis
    "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
  (description
    "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
  (license #f))
)

(define-public rust-wasm-bindgen-backend
(package
  (name "rust-wasm-bindgen-backend")
  (version "0.2.42")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-backend" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1wgracnwq4hpwfkzyqr2x4lij7cdwafx2cjxvhf8ibm7fhrm9h20"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-bumpalo" ,rust-bumpalo "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-log" ,rust-log "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")
      ("rust-wasm-bindgen-shared"
       ,rust-wasm-bindgen-shared
       "src")))
  (home-page
    "https://rustwasm.github.io/wasm-bindgen/")
  (synopsis
    "Backend code generation of the wasm-bindgen tool
")
  (description
    "Backend code generation of the wasm-bindgen tool
")
  (license #f))
)

(define-public rust-wasm-bindgen-shared
(package
  (name "rust-wasm-bindgen-shared")
  (version "0.2.42")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-shared" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1zn9kapb6kiiszd6xnic5cc12nxchhd0c03lxmn7n58sdq934v1j"))))
  (build-system cargo-build-system)
  (home-page
    "https://rustwasm.github.io/wasm-bindgen/")
  (synopsis
    "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
  (description
    "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
  (license #f))
)

(define-public rust-bumpalo
(package
  (name "rust-bumpalo")
  (version "2.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bumpalo" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1i3kcqc42mbimrf94mgf9hbbqv78vylfnz3q23wf5c9mm4mvr9pn"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-criterion" ,rust-criterion "src")
      ("rust-quickcheck" ,rust-quickcheck "src")))
  (home-page "https://github.com/fitzgen/bumpalo")
  (synopsis
    "A fast bump allocation arena for Rust.")
  (description
    "This package provides a fast bump allocation arena for Rust.")
  (license #f))
)

(define-public rust-criterion
(package
  (name "rust-criterion")
  (version "0.2.11")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "criterion" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1543wlpc4p1kz7sqqa7ylr8bkdr8l4f34hy4bxj7krpkahwhaqq3"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-approx" ,rust-approx "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-atty" ,rust-atty "src")
      ("rust-cast" ,rust-cast "src")
      ("rust-clap" ,rust-clap "src")
      ("rust-criterion-plot"
       ,rust-criterion-plot
       "src")
      ("rust-csv" ,rust-csv "src")
      ("rust-itertools" ,rust-itertools "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-num-traits" ,rust-num-traits "src")
      ("rust-rand-core" ,rust-rand-core "src")
      ("rust-rand-os" ,rust-rand-os "src")
      ("rust-rand-xoshiro" ,rust-rand-xoshiro "src")
      ("rust-rayon" ,rust-rayon "src")
      ("rust-rayon-core" ,rust-rayon-core "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-tinytemplate" ,rust-tinytemplate "src")
      ("rust-walkdir" ,rust-walkdir "src")))
  (home-page
    "https://bheisler.github.io/criterion.rs/book/index.html")
  (synopsis
    "Statistics-driven micro-benchmarking library")
  (description
    "Statistics-driven micro-benchmarking library")
  (license #f))
)

(define-public rust-approx
(package
  (name "rust-approx")
  (version "0.3.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "approx" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1hx580xjdxl3766js9b49rnbnmr8gw8c060809l43k9f0xshprph"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-num-complex" ,rust-num-complex "src")
      ("rust-num-traits" ,rust-num-traits "src")))
  (home-page
    "https://github.com/brendanzab/approx")
  (synopsis
    "Approximate floating point equality comparisons and assertions.")
  (description
    "Approximate floating point equality comparisons and assertions.")
  (license #f))
)

(define-public rust-cast
(package
  (name "rust-cast")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cast" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09yl2700crxa4n860b080msij25klvs1kfzazhp2aihchvr16q4j"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")))
  (home-page "https://github.com/japaric/cast.rs")
  (synopsis
    "Ergonomic, checked cast functions for primitive types")
  (description
    "Ergonomic, checked cast functions for primitive types")
  (license #f))
)

(define-public rust-criterion-plot
(package
  (name "rust-criterion-plot")
  (version "0.3.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "criterion-plot" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "13pv09z4ryp70qyzablkibwa2mh6c2852qq1sjr9wjigvwnj3ybn"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-itertools-num" ,rust-itertools-num "src")
      ("rust-num-complex" ,rust-num-complex "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-cast" ,rust-cast "src")
      ("rust-itertools" ,rust-itertools "src")))
  (home-page
    "https://github.com/bheisler/criterion.rs")
  (synopsis "Criterion's plotting library")
  (description "Criterion's plotting library")
  (license #f))
)

(define-public rust-csv
(package
  (name "rust-csv")
  (version "1.0.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "csv" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0fqvxa2ixgvzx3jr39cfss2rwqhq16dnh4amzjjva909zddf4i4h"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-bytes" ,rust-serde-bytes "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (inputs
    `(("rust-csv-core" ,rust-csv-core "src")
      ("rust-itoa" ,rust-itoa "src")
      ("rust-ryu" ,rust-ryu "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/BurntSushi/rust-csv")
  (synopsis
    "Fast CSV parsing with support for serde.")
  (description
    "Fast CSV parsing with support for serde.")
  (license #f))
)

(define-public rust-tinytemplate
(package
  (name "rust-tinytemplate")
  (version "1.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tinytemplate" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "05b1jllddgya0n3qn1jms7nizaid0y3krg87p19gnji7jj40hmbn"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-criterion" ,rust-criterion "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (home-page
    "https://github.com/bheisler/TinyTemplate")
  (synopsis "Simple, lightweight template engine")
  (description
    "Simple, lightweight template engine")
  (license #f))
)

(define-public rust-itertools-num
(package
  (name "rust-itertools-num")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "itertools-num" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1rr7ig9nkpampcas23s91x7yac6qdnwssq3nap522xbgkqps4wm8"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-itertools" ,rust-itertools "src")
      ("rust-quickcheck" ,rust-quickcheck "src")))
  (inputs
    `(("rust-num-traits" ,rust-num-traits "src")))
  (home-page
    "https://github.com/bluss/itertools-num")
  (synopsis
    "Numerical iterator tools. Extra iterators and iterator methods and functions.
")
  (description
    "Numerical iterator tools.  Extra iterators and iterator methods and functions.
")
  (license #f))
)

(define-public rust-csv-core
(package
  (name "rust-csv-core")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "csv-core" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0r9xymqixn5hdkjhvb0wzyivfcnvq0dkhyphs7kzzrip5zvdwp7s"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-arrayvec" ,rust-arrayvec "src")))
  (inputs `(("rust-memchr" ,rust-memchr "src")))
  (home-page
    "https://github.com/BurntSushi/rust-csv")
  (synopsis
    "Bare bones CSV parsing with no_std support.")
  (description
    "Bare bones CSV parsing with no_std support.")
  (license #f))
)

(define-public rust-cloudabi
(package
  (name "rust-cloudabi")
  (version "0.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cloudabi" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0kxcg83jlihy0phnd2g8c2c303px3l2p3pkjz357ll6llnd5pz6x"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-bitflags" ,rust-bitflags "src")))
  (home-page "https://nuxi.nl/cloudabi/")
  (synopsis
    "Low level interface to CloudABI. Contains all syscalls and related types.")
  (description
    "Low level interface to CloudABI.  Contains all syscalls and related types.")
  (license #f))
)

(define-public rust-fuchsia-cprng
(package
  (name "rust-fuchsia-cprng")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fuchsia-cprng" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1fnkqrbz7ixxzsb04bsz9p0zzazanma8znfdqjvh39n14vapfvx0"))))
  (build-system cargo-build-system)
  (home-page
    "https://fuchsia.googlesource.com/fuchsia/+/master/garnet/public/rust/fuchsia-cprng")
  (synopsis
    "Rust crate for the Fuchsia cryptographically secure pseudorandom number generator")
  (description
    "Rust crate for the Fuchsia cryptographically secure pseudorandom number generator")
  (license #f))
)

(define-public rust-rdrand
(package
  (name "rust-rdrand")
  (version "0.5.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rdrand" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0biqbnpgkg3ji9sad04nzhgzwihzr2lfjp1wmwar6117p3s4aac3"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-rand-core" ,rust-rand-core "src")))
  (home-page
    "https://github.com/nagisa/rust_rdrand/")
  (synopsis
    "An implementation of random number generator based on rdrand and rdseed instructions")
  (description
    "An implementation of random number generator based on rdrand and rdseed instructions")
  (license #f))
)

(define-public rust-stdweb
(package
  (name "rust-stdweb")
  (version "0.4.16")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "stdweb" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "17dn024wlxi6lzhhpcb2d2smgfryiqdsaq5ylrvji2w25yndbhdj"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rustc-version" ,rust-rustc-version "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-stdweb-internal-test-macro"
       ,rust-stdweb-internal-test-macro
       "src")
      ("rust-wasm-bindgen-test"
       ,rust-wasm-bindgen-test
       "src")))
  (inputs
    `(("rust-discard" ,rust-discard "src")
      ("rust-futures-channel-preview"
       ,rust-futures-channel-preview
       "src")
      ("rust-futures-core-preview"
       ,rust-futures-core-preview
       "src")
      ("rust-futures-executor-preview"
       ,rust-futures-executor-preview
       "src")
      ("rust-futures-util-preview"
       ,rust-futures-util-preview
       "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-stdweb-derive" ,rust-stdweb-derive "src")
      ("rust-stdweb-internal-macros"
       ,rust-stdweb-internal-macros
       "src")
      ("rust-stdweb-internal-runtime"
       ,rust-stdweb-internal-runtime
       "src")
      ("rust-wasm-bindgen" ,rust-wasm-bindgen "src")))
  (home-page "https://github.com/koute/stdweb")
  (synopsis
    "A standard library for the client-side Web")
  (description
    "This package provides a standard library for the client-side Web")
  (license #f))
)

(define-public rust-stdweb-internal-test-macro
(package
  (name "rust-stdweb-internal-test-macro")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "stdweb-internal-test-macro" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "12rrm7p77xnm3xacgn3rgniiyyjb4gq7902wpbljsvbx045z69l2"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")))
  (home-page "https://github.com/koute/stdweb")
  (synopsis "Internal crate of the `stdweb` crate")
  (description
    "Internal crate of the `stdweb` crate")
  (license #f))
)

(define-public rust-discard
(package
  (name "rust-discard")
  (version "1.0.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "discard" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1h67ni5bxvg95s91wgicily4ix7lcw7cq0a5gy9njrybaibhyb91"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/Pauan/rust-discard")
  (synopsis
    "Discard trait which allows for intentionally leaking memory")
  (description
    "Discard trait which allows for intentionally leaking memory")
  (license #f))
)

(define-public rust-futures-channel-preview
(package
  (name "rust-futures-channel-preview")
  (version "0.3.0-alpha.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-channel-preview" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1g4yfy5zsi6vd2mmhkyc62z9prijs94rycqyjsr6adznlqmf7h3m"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-futures-core-preview"
       ,rust-futures-core-preview
       "src")))
  (home-page
    "https://rust-lang-nursery.github.io/futures-rs")
  (synopsis
    "Channels for asynchronous communication using futures-rs.
")
  (description
    "Channels for asynchronous communication using futures-rs.
")
  (license #f))
)

(define-public rust-futures-core-preview
(package
  (name "rust-futures-core-preview")
  (version "0.3.0-alpha.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-core-preview" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1cl0wxwvhacn1741r51ry75ik5bmq4dwfisl1qwqkxcik4cpn7va"))))
  (build-system cargo-build-system)
  (inputs `(("rust-either" ,rust-either "src")))
  (home-page
    "https://rust-lang-nursery.github.io/futures-rs")
  (synopsis
    "The core traits and types in for the `futures` library.
")
  (description
    "The core traits and types in for the `futures` library.
")
  (license #f))
)

(define-public rust-futures-executor-preview
(package
  (name "rust-futures-executor-preview")
  (version "0.3.0-alpha.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-executor-preview" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "00psi82h4l4hkvjlmr4qg1an8baq0ap5m8my90898l766i1z9sqv"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-futures-channel-preview"
       ,rust-futures-channel-preview
       "src")
      ("rust-futures-core-preview"
       ,rust-futures-core-preview
       "src")
      ("rust-futures-util-preview"
       ,rust-futures-util-preview
       "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-pin-utils" ,rust-pin-utils "src")))
  (home-page
    "https://rust-lang-nursery.github.io/futures-rs")
  (synopsis
    "Executors for asynchronous tasks based on the futures-rs library.
")
  (description
    "Executors for asynchronous tasks based on the futures-rs library.
")
  (license #f))
)

(define-public rust-futures-util-preview
(package
  (name "rust-futures-util-preview")
  (version "0.3.0-alpha.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-util-preview" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1b6rqydqx9fkha1hyz916cial62j3ljnas26wk858x7sbjlqv43i"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-either" ,rust-either "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-futures-channel-preview"
       ,rust-futures-channel-preview
       "src")
      ("rust-futures-core-preview"
       ,rust-futures-core-preview
       "src")
      ("rust-futures-io-preview"
       ,rust-futures-io-preview
       "src")
      ("rust-futures-select-macro-preview"
       ,rust-futures-select-macro-preview
       "src")
      ("rust-futures-sink-preview"
       ,rust-futures-sink-preview
       "src")
      ("rust-pin-utils" ,rust-pin-utils "src")
      ("rust-proc-macro-hack"
       ,rust-proc-macro-hack
       "src")
      ("rust-proc-macro-nested"
       ,rust-proc-macro-nested
       "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-rand-core" ,rust-rand-core "src")
      ("rust-slab" ,rust-slab "src")
      ("rust-tokio-io" ,rust-tokio-io "src")))
  (home-page
    "https://rust-lang-nursery.github.io/futures-rs")
  (synopsis
    "Common utilities and extension traits for the futures-rs library.
")
  (description
    "Common utilities and extension traits for the futures-rs library.
")
  (license #f))
)

(define-public rust-stdweb-derive
(package
  (name "rust-stdweb-derive")
  (version "0.5.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "stdweb-derive" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0c1rxx6rqcc4iic5hx320ki3vshpi8k58m5600iqzq4x2zcyn88f"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://github.com/koute/stdweb")
  (synopsis "Derive macros for the `stdweb` crate")
  (description
    "Derive macros for the `stdweb` crate")
  (license #f))
)

(define-public rust-stdweb-internal-macros
(package
  (name "rust-stdweb-internal-macros")
  (version "0.2.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "stdweb-internal-macros" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1yjrmkc6sb1035avic383pa3avk2s9k3n17yjcza8yb9nw47v3z6"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-base-x" ,rust-base-x "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-sha1" ,rust-sha1 "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://github.com/koute/stdweb")
  (synopsis
    "Internal procedural macros for the `stdweb` crate")
  (description
    "Internal procedural macros for the `stdweb` crate")
  (license #f))
)

(define-public rust-stdweb-internal-runtime
(package
  (name "rust-stdweb-internal-runtime")
  (version "0.1.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "stdweb-internal-runtime" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nhpyra7glbwcpakhpj5a3d7h7kx1ynif473nzshmk226m91f8ym"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/koute/stdweb")
  (synopsis
    "Internal runtime for the `stdweb` crate")
  (description
    "Internal runtime for the `stdweb` crate")
  (license #f))
)

(define-public rust-console-error-panic-hook
(package
  (name "rust-console-error-panic-hook")
  (version "0.1.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "console_error_panic_hook" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "04d2narcrzk9bnddz17rr2l819l82pr0h6d98s2w9q236n87dndq"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-wasm-bindgen" ,rust-wasm-bindgen "src")))
  (home-page
    "https://github.com/rustwasm/console_error_panic_hook")
  (synopsis
    "A panic hook for `wasm32-unknown-unknown` that logs panics to `console.error`")
  (description
    "This package provides a panic hook for `wasm32-unknown-unknown` that logs panics to `console.error`")
  (license #f))
)

(define-public rust-futures
(package
  (name "rust-futures")
  (version "0.1.26")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10a9s8yz1acvsy61s244w5b0hxmrv523mn4b8iyigj07jpzix532"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/rust-lang-nursery/futures-rs")
  (synopsis
    "An implementation of futures and streams featuring zero allocations,
composability, and iterator-like interfaces.
")
  (description
    "An implementation of futures and streams featuring zero allocations,
composability, and iterator-like interfaces.
")
  (license #f))
)

(define-public rust-js-sys
(package
  (name "rust-js-sys")
  (version "0.3.19")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "js-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0v8b3kzm0x3na0hgvv526mdvjifndryj5kbbgzbl25xq8pa4z69w"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-wasm-bindgen-futures"
       ,rust-wasm-bindgen-futures
       "src")
      ("rust-wasm-bindgen-test"
       ,rust-wasm-bindgen-test
       "src")))
  (inputs
    `(("rust-wasm-bindgen" ,rust-wasm-bindgen "src")))
  (home-page
    "https://rustwasm.github.io/wasm-bindgen/")
  (synopsis
    "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
  (description
    "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
  (license #f))
)

(define-public rust-scoped-tls
(package
  (name "rust-scoped-tls")
  (version "1.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "scoped-tls" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1hj8lifzvivdb1z02lfnzkshpvk85nkgzxsy2hc0zky9wf894spa"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/alexcrichton/scoped-tls")
  (synopsis
    "Library implementation of the standard library's old `scoped_thread_local!`
macro for providing scoped access to thread local storage (TLS) so any type can
be stored into TLS.
")
  (description
    "Library implementation of the standard library's old `scoped_thread_local!`
macro for providing scoped access to thread local storage (TLS) so any type can
be stored into TLS.
")
  (license #f))
)

(define-public rust-wasm-bindgen-futures
(package
  (name "rust-wasm-bindgen-futures")
  (version "0.3.19")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-futures" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0bcbc525n30a7bprgi77rjjqmgapavpg89sx2myzjhvf3zy73l8a"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-wasm-bindgen-test"
       ,rust-wasm-bindgen-test
       "src")))
  (inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-js-sys" ,rust-js-sys "src")
      ("rust-wasm-bindgen" ,rust-wasm-bindgen "src")))
  (home-page
    "https://rustwasm.github.io/wasm-bindgen/")
  (synopsis
    "Bridging the gap between Rust Futures and JavaScript Promises")
  (description
    "Bridging the gap between Rust Futures and JavaScript Promises")
  (license #f))
)

(define-public rust-wasm-bindgen-test-macro
(package
  (name "rust-wasm-bindgen-test-macro")
  (version "0.2.42")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-test-macro" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1q8s04w3gi8jbcb74pwdj55l37arnv9rpyvqkkcyraf9yd87qizb"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")))
  (home-page
    "https://github.com/rustwasm/wasm-bindgen")
  (synopsis
    "Internal testing macro for wasm-bindgen")
  (description
    "Internal testing macro for wasm-bindgen")
  (license #f))
)

(define-public rust-pin-utils
(package
  (name "rust-pin-utils")
  (version "0.1.0-alpha.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pin-utils" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "11xmyx00n4m37d546by2rxb8ryxs12v55cc172i3yak1rqccd52q"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/rust-lang-nursery/pin-utils")
  (synopsis "Utilities for pinning
")
  (description "Utilities for pinning
")
  (license #f))
)

(define-public rust-futures-io-preview
(package
  (name "rust-futures-io-preview")
  (version "0.3.0-alpha.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-io-preview" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0p8bpz0k46hgszirsqndkwszywgqhkiqjyxjv5c4361xj3casxfx"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-futures-core-preview"
       ,rust-futures-core-preview
       "src")
      ("rust-iovec" ,rust-iovec "src")))
  (home-page
    "https://rust-lang-nursery.github.io/futures-rs")
  (synopsis
    "The `AsyncRead` and `AsyncWrite` traits for the futures-rs library.
")
  (description
    "The `AsyncRead` and `AsyncWrite` traits for the futures-rs library.
")
  (license #f))
)

(define-public rust-futures-select-macro-preview
(package
  (name "rust-futures-select-macro-preview")
  (version "0.3.0-alpha.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri
             "futures-select-macro-preview"
             version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "11b7by01hr7jkj9xj25ks28bg8qpkap0agm0fiqgmr63axdx3h69"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro-hack"
       ,rust-proc-macro-hack
       "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://rust-lang-nursery.github.io/futures-rs")
  (synopsis
    "The `select!` macro for waiting on multiple different `Future`s at once and handling the first one to complete.
")
  (description
    "The `select!` macro for waiting on multiple different `Future`s at once and handling the first one to complete.
")
  (license #f))
)

(define-public rust-futures-sink-preview
(package
  (name "rust-futures-sink-preview")
  (version "0.3.0-alpha.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-sink-preview" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1d3c5ngci04r4dwmg8bj6g4ab6g70klzi8s5nzjx08gpmz42pg6v"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-either" ,rust-either "src")
      ("rust-futures-channel-preview"
       ,rust-futures-channel-preview
       "src")
      ("rust-futures-core-preview"
       ,rust-futures-core-preview
       "src")))
  (home-page
    "https://rust-lang-nursery.github.io/futures-rs")
  (synopsis
    "The asynchronous `Sink` trait for the futures-rs library.
")
  (description
    "The asynchronous `Sink` trait for the futures-rs library.
")
  (license #f))
)

(define-public rust-proc-macro-nested
(package
  (name "rust-proc-macro-nested")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "proc-macro-nested" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0bmlksm8vl44wkwihmwr7jsjznhbg0n7aibcw1cs2jgjcp86x6in"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/dtolnay/proc-macro-hack")
  (synopsis
    "Support for nested proc-macro-hack invocations")
  (description
    "Support for nested proc-macro-hack invocations")
  (license #f))
)

(define-public rust-slab
(package
  (name "rust-slab")
  (version "0.4.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "slab" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1y59xsa27jk84sxzswjk60xcjf8b4fm5960jwpznrrcmasyva4f1"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/carllerche/slab")
  (synopsis
    "Pre-allocated storage for a uniform data type")
  (description
    "Pre-allocated storage for a uniform data type")
  (license #f))
)

(define-public rust-tokio-io
(package
  (name "rust-tokio-io")
  (version "0.1.12")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-io" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09jrz1hh4h1vj45qy09y7m7m8jsy1hl6g32clnky25mdim3dp42h"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tokio-current-thread"
       ,rust-tokio-current-thread
       "src")))
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-log" ,rust-log "src")))
  (home-page "https://tokio.rs")
  (synopsis
    "Core I/O primitives for asynchronous I/O in Rust.
")
  (description
    "Core I/O primitives for asynchronous I/O in Rust.
")
  (license #f))
)

(define-public rust-tokio-current-thread
(package
  (name "rust-tokio-current-thread")
  (version "0.1.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-current-thread" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0hx4c8v88kk0ih8x5s564gsgwwf8n11kryvxm72l1f7isz51fqni"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-tokio-executor"
       ,rust-tokio-executor
       "src")))
  (home-page "https://github.com/tokio-rs/tokio")
  (synopsis
    "Single threaded executor which manage many tasks concurrently on the current thread.
")
  (description
    "Single threaded executor which manage many tasks concurrently on the current thread.
")
  (license #f))
)

(define-public rust-tokio-executor
(package
  (name "rust-tokio-executor")
  (version "0.1.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-executor" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0pjmgpg58k3hf5q9w6xjljsv8xy66lf734qnfwsc0g3pq3349sl3"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tokio" ,rust-tokio "src")))
  (inputs
    `(("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")
      ("rust-futures" ,rust-futures "src")))
  (home-page "https://github.com/tokio-rs/tokio")
  (synopsis "Future execution primitives
")
  (description "Future execution primitives
")
  (license #f))
)

(define-public rust-tokio
(package
  (name "rust-tokio")
  (version "0.1.18")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nbz85f1j97bwjdmlv2xxxdpky9g0kijra1in6530ys3b98iwr35"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-env-logger" ,rust-env-logger "src")
      ("rust-flate2" ,rust-flate2 "src")
      ("rust-futures-cpupool"
       ,rust-futures-cpupool
       "src")
      ("rust-http" ,rust-http "src")
      ("rust-httparse" ,rust-httparse "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-time" ,rust-time "src")))
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-mio" ,rust-mio "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-tokio-async-await"
       ,rust-tokio-async-await
       "src")
      ("rust-tokio-codec" ,rust-tokio-codec "src")
      ("rust-tokio-current-thread"
       ,rust-tokio-current-thread
       "src")
      ("rust-tokio-executor"
       ,rust-tokio-executor
       "src")
      ("rust-tokio-fs" ,rust-tokio-fs "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-tokio-reactor" ,rust-tokio-reactor "src")
      ("rust-tokio-sync" ,rust-tokio-sync "src")
      ("rust-tokio-tcp" ,rust-tokio-tcp "src")
      ("rust-tokio-threadpool"
       ,rust-tokio-threadpool
       "src")
      ("rust-tokio-timer" ,rust-tokio-timer "src")
      ("rust-tokio-trace-core"
       ,rust-tokio-trace-core
       "src")
      ("rust-tokio-udp" ,rust-tokio-udp "src")
      ("rust-tokio-uds" ,rust-tokio-uds "src")))
  (home-page "https://tokio.rs")
  (synopsis
    "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
  (description
    "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
  (license #f))
)

(define-public rust-flate2
(package
  (name "rust-flate2")
  (version "1.0.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "flate2" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1ahqpwfik5s2bc05xpx88pjgd7bmama3iw9pw2k0ipmjham6hzpq"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-tokio-tcp" ,rust-tokio-tcp "src")
      ("rust-tokio-threadpool"
       ,rust-tokio-threadpool
       "src")))
  (inputs
    `(("rust-crc32fast" ,rust-crc32fast "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-libz-sys" ,rust-libz-sys "src")
      ("rust-miniz-sys" ,rust-miniz-sys "src")
      ("rust-miniz-oxide-c-api"
       ,rust-miniz-oxide-c-api
       "src")
      ("rust-miniz-oxide-c-api"
       ,rust-miniz-oxide-c-api
       "src")
      ("rust-tokio-io" ,rust-tokio-io "src")))
  (home-page
    "https://github.com/alexcrichton/flate2-rs")
  (synopsis
    "Bindings to miniz.c for DEFLATE compression and decompression exposed as
Reader/Writer streams. Contains bindings for zlib, deflate, and gzip-based
streams.
")
  (description
    "Bindings to miniz.c for DEFLATE compression and decompression exposed as
Reader/Writer streams.  Contains bindings for zlib, deflate, and gzip-based
streams.
")
  (license #f))
)

(define-public rust-futures-cpupool
(package
  (name "rust-futures-cpupool")
  (version "0.1.8")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-cpupool" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1r32456gpblzfvnkf60545v8acqk7gh5zhyhi1jn669k9gicv45b"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-num-cpus" ,rust-num-cpus "src")))
  (home-page
    "https://github.com/alexcrichton/futures-rs")
  (synopsis
    "An implementation of thread pools which hand out futures to the results of the
computation on the threads themselves.
")
  (description
    "An implementation of thread pools which hand out futures to the results of the
computation on the threads themselves.
")
  (license #f))
)

(define-public rust-http
(package
  (name "rust-http")
  (version "0.1.17")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "http" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "06icxvrd26r6s7dzjavja7r47hgjb9851wblqh8frxnsy3q29lzf"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-indexmap" ,rust-indexmap "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-seahash" ,rust-seahash "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-fnv" ,rust-fnv "src")
      ("rust-itoa" ,rust-itoa "src")))
  (home-page "https://github.com/hyperium/http")
  (synopsis
    "A set of types for representing HTTP requests and responses.
")
  (description
    "This package provides a set of types for representing HTTP requests and responses.
")
  (license #f))
)

(define-public rust-httparse
(package
  (name "rust-httparse")
  (version "1.3.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "httparse" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10vsfx1b8drhif08fbi0ha9d3v1f3h80w42rxh0y3hrvzl64nwz8"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-pico-sys" ,rust-pico-sys "src")))
  (home-page
    "https://github.com/seanmonstar/httparse")
  (synopsis
    "A tiny, safe, speedy, zero-copy HTTP/1.x parser.")
  (description
    "This package provides a tiny, safe, speedy, zero-copy HTTP/1.x parser.")
  (license #f))
)

(define-public rust-mio
(package
  (name "rust-mio")
  (version "0.6.16")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "mio" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0cpl8zry136c2rm88dv9vp3daj5np2i8fbrhrhk116v1y8qn6r3i"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-env-logger" ,rust-env-logger "src")
      ("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-fuchsia-zircon"
       ,rust-fuchsia-zircon
       "src")
      ("rust-fuchsia-zircon-sys"
       ,rust-fuchsia-zircon-sys
       "src")
      ("rust-iovec" ,rust-iovec "src")
      ("rust-kernel32-sys" ,rust-kernel32-sys "src")
      ("rust-lazycell" ,rust-lazycell "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-log" ,rust-log "src")
      ("rust-miow" ,rust-miow "src")
      ("rust-net2" ,rust-net2 "src")
      ("rust-slab" ,rust-slab "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/carllerche/mio")
  (synopsis "Lightweight non-blocking IO")
  (description "Lightweight non-blocking IO")
  (license #f))
)

(define-public rust-tokio-async-await
(package
  (name "rust-tokio-async-await")
  (version "0.1.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-async-await" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1g4167bxkmxnq73i27ffazxriwg9gc0qwpfg9vylbqbmfvdyj8c2"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-hyper" ,rust-hyper "src")
      ("rust-tokio" ,rust-tokio "src")))
  (inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-tokio-io" ,rust-tokio-io "src")))
  (home-page "https://tokio.rs")
  (synopsis
    "Experimental async/await support for Tokio
")
  (description
    "Experimental async/await support for Tokio
")
  (license #f))
)

(define-public rust-tokio-codec
(package
  (name "rust-tokio-codec")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-codec" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "17y3hi3dd0bdfkrzshx9qhwcf49xv9iynszj7iwy3w4nmz71wl2w"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-tokio-io" ,rust-tokio-io "src")))
  (home-page "https://tokio.rs")
  (synopsis
    "Utilities for encoding and decoding frames.
")
  (description
    "Utilities for encoding and decoding frames.
")
  (license #f))
)

(define-public rust-tokio-fs
(package
  (name "rust-tokio-fs")
  (version "0.1.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-fs" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1bxp8585pi4j5g39ci2gkk99qnyilyhhila7cs8r6scdn0idrriz"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rand" ,rust-rand "src")
      ("rust-tempdir" ,rust-tempdir "src")
      ("rust-tempfile" ,rust-tempfile "src")
      ("rust-tokio" ,rust-tokio "src")
      ("rust-tokio-codec" ,rust-tokio-codec "src")
      ("rust-tokio-io" ,rust-tokio-io "src")))
  (inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-tokio-threadpool"
       ,rust-tokio-threadpool
       "src")))
  (home-page "https://tokio.rs")
  (synopsis "Filesystem API for Tokio.
")
  (description "Filesystem API for Tokio.
")
  (license #f))
)

(define-public rust-tokio-reactor
(package
  (name "rust-tokio-reactor")
  (version "0.1.9")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-reactor" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1khip64cn63xvayq1db68kxcnhgw3cb449a4n2lbw4p1qzx6pwba"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-tokio" ,rust-tokio "src")
      ("rust-tokio-io-pool" ,rust-tokio-io-pool "src")))
  (inputs
    `(("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-log" ,rust-log "src")
      ("rust-mio" ,rust-mio "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-parking-lot" ,rust-parking-lot "src")
      ("rust-slab" ,rust-slab "src")
      ("rust-tokio-executor"
       ,rust-tokio-executor
       "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-tokio-sync" ,rust-tokio-sync "src")))
  (home-page "https://tokio.rs")
  (synopsis
    "Event loop that drives Tokio I/O resources.
")
  (description
    "Event loop that drives Tokio I/O resources.
")
  (license #f))
)

(define-public rust-tokio-sync
(package
  (name "rust-tokio-sync")
  (version "0.1.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-sync" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "08p0bznfm841xgc2zpqv4lghyr413vvnf9w7f1p59xvba3gqb8zx"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-env-logger" ,rust-env-logger "src")
      ("rust-loom" ,rust-loom "src")
      ("rust-tokio" ,rust-tokio "src")
      ("rust-tokio-mock-task"
       ,rust-tokio-mock-task
       "src")))
  (inputs
    `(("rust-fnv" ,rust-fnv "src")
      ("rust-futures" ,rust-futures "src")))
  (home-page "https://tokio.rs")
  (synopsis "Synchronization utilities.
")
  (description "Synchronization utilities.
")
  (license #f))
)

(define-public rust-tokio-tcp
(package
  (name "rust-tokio-tcp")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-tcp" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "06a15vg8bcd33ng3h9ldzlq7wl4jsw0p9qpy7v22ls5yah3b250x"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-env-logger" ,rust-env-logger "src")
      ("rust-tokio" ,rust-tokio "src")))
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-iovec" ,rust-iovec "src")
      ("rust-mio" ,rust-mio "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-tokio-reactor" ,rust-tokio-reactor "src")))
  (home-page "https://tokio.rs")
  (synopsis "TCP bindings for tokio.
")
  (description "TCP bindings for tokio.
")
  (license #f))
)

(define-public rust-tokio-threadpool
(package
  (name "rust-tokio-threadpool")
  (version "0.1.13")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-threadpool" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0g1jy6fgnpax9ddwhh8phdk5zl732nsk3i1nbxamk5ng4v7mjmzc"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-env-logger" ,rust-env-logger "src")
      ("rust-futures-cpupool"
       ,rust-futures-cpupool
       "src")
      ("rust-threadpool" ,rust-threadpool "src")))
  (inputs
    `(("rust-crossbeam-deque"
       ,rust-crossbeam-deque
       "src")
      ("rust-crossbeam-queue"
       ,rust-crossbeam-queue
       "src")
      ("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-log" ,rust-log "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-slab" ,rust-slab "src")
      ("rust-tokio-executor"
       ,rust-tokio-executor
       "src")))
  (home-page "https://github.com/tokio-rs/tokio")
  (synopsis
    "A task scheduler backed by a work-stealing thread pool.
")
  (description
    "This package provides a task scheduler backed by a work-stealing thread pool.
")
  (license #f))
)

(define-public rust-tokio-timer
(package
  (name "rust-tokio-timer")
  (version "0.2.10")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-timer" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1ii0l42i9x4myr3y8h9hf7in4p84wad6l4iran6afvxs0h29f419"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rand" ,rust-rand "src")
      ("rust-tokio" ,rust-tokio "src")
      ("rust-tokio-mock-task"
       ,rust-tokio-mock-task
       "src")))
  (inputs
    `(("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-slab" ,rust-slab "src")
      ("rust-tokio-executor"
       ,rust-tokio-executor
       "src")))
  (home-page "https://github.com/tokio-rs/tokio")
  (synopsis "Timer facilities for Tokio
")
  (description "Timer facilities for Tokio
")
  (license #f))
)

(define-public rust-tokio-trace-core
(package
  (name "rust-tokio-trace-core")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-trace-core" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1lwx1m1x5jaf4p8gczhnyrcsnid4cxba92z4b8cdqc4qvvd9w31m"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")))
  (home-page "https://tokio.rs")
  (synopsis "Core primitives for tokio-trace.
")
  (description
    "Core primitives for tokio-trace.
")
  (license #f))
)

(define-public rust-tokio-udp
(package
  (name "rust-tokio-udp")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-udp" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "14kfj35s465czcspayacnzlxrazfvxzhhggq1rqlljhgp1sqa9k6"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-env-logger" ,rust-env-logger "src")))
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-log" ,rust-log "src")
      ("rust-mio" ,rust-mio "src")
      ("rust-tokio-codec" ,rust-tokio-codec "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-tokio-reactor" ,rust-tokio-reactor "src")))
  (home-page "https://tokio.rs")
  (synopsis "UDP bindings for tokio.
")
  (description "UDP bindings for tokio.
")
  (license #f))
)

(define-public rust-tokio-uds
(package
  (name "rust-tokio-uds")
  (version "0.2.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-uds" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0i94kxma6l7iy5hd5k7nvn7v9pnyw0s54bm9mjs0lap1l0xzqzq3"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempfile" ,rust-tempfile "src")
      ("rust-tokio" ,rust-tokio "src")))
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-iovec" ,rust-iovec "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-log" ,rust-log "src")
      ("rust-mio" ,rust-mio "src")
      ("rust-mio-uds" ,rust-mio-uds "src")
      ("rust-tokio-codec" ,rust-tokio-codec "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-tokio-reactor" ,rust-tokio-reactor "src")))
  (home-page "https://github.com/tokio-rs/tokio")
  (synopsis "Unix Domain sockets for Tokio
")
  (description "Unix Domain sockets for Tokio
")
  (license #f))
)

(define-public rust-crc32fast
(package
  (name "rust-crc32fast")
  (version "1.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crc32fast" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1c9dhkvf3brrzzplcijaywxi2w8wv5578i0ryhcm7x8dmzi5s4ms"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bencher" ,rust-bencher "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs `(("rust-cfg-if" ,rust-cfg-if "src")))
  (home-page
    "https://github.com/srijs/rust-crc32fast")
  (synopsis
    "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
  (description
    "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
  (license #f))
)

(define-public rust-libz-sys
(package
  (name "rust-libz-sys")
  (version "1.0.25")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "libz-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1gjycyl2283525abks98bhxa4r259m617xfm5z52p3p3c8ry9d9f"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cc" ,rust-cc "src")
      ("rust-pkg-config" ,rust-pkg-config "src")
      ("rust-vcpkg" ,rust-vcpkg "src")))
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/alexcrichton/libz-sys")
  (synopsis
    "Bindings to the system libz library (also known as zlib).
")
  (description
    "Bindings to the system libz library (also known as zlib).
")
  (license #f))
)

(define-public rust-miniz-sys
(package
  (name "rust-miniz-sys")
  (version "0.1.11")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "miniz-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0jg681nb6i6l47113x481aqz8d23md1q5dlr2sam569n43xyl003"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/alexcrichton/flate2-rs")
  (synopsis "Bindings to the miniz.c library.
")
  (description
    "Bindings to the miniz.c library.
")
  (license #f))
)

(define-public rust-miniz-oxide-c-api
(package
  (name "rust-miniz-oxide-c-api")
  (version "0.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "miniz_oxide_c_api" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1avv8ixjpbiqzqap1ri1n9wl9qnlhzf93cgg3pvpx07389x95zmp"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (inputs
    `(("rust-crc" ,rust-crc "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-miniz-oxide" ,rust-miniz-oxide "src")))
  (home-page
    "https://github.com/Frommi/miniz_oxide/")
  (synopsis
    "DEFLATE compression and decompression API designed to be Rust drop-in replacement for miniz")
  (description
    "DEFLATE compression and decompression API designed to be Rust drop-in replacement for miniz")
  (license #f))
)

(define-public rust-fuchsia-zircon
(package
  (name "rust-fuchsia-zircon")
  (version "0.3.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fuchsia-zircon" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10jxc5ks1x06gpd0xg51kcjrxr35nj6qhx2zlc5n7bmskv3675rf"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-bitflags" ,rust-bitflags "src")
      ("rust-fuchsia-zircon-sys"
       ,rust-fuchsia-zircon-sys
       "src")))
  (home-page
    "https://fuchsia.googlesource.com/garnet/")
  (synopsis "Rust bindings for the Zircon kernel")
  (description
    "Rust bindings for the Zircon kernel")
  (license #f))
)

(define-public rust-fuchsia-zircon-sys
(package
  (name "rust-fuchsia-zircon-sys")
  (version "0.3.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fuchsia-zircon-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "19zp2085qsyq2bh1gvcxq1lb8w6v6jj9kbdkhpdjrl95fypakjix"))))
  (build-system cargo-build-system)
  (home-page
    "https://fuchsia.googlesource.com/garnet/")
  (synopsis
    "Low-level Rust bindings for the Zircon kernel")
  (description
    "Low-level Rust bindings for the Zircon kernel")
  (license #f))
)

(define-public rust-kernel32-sys
(package
  (name "rust-kernel32-sys")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "kernel32-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1389av0601a9yz8dvx5zha9vmkd6ik7ax0idpb032d28555n41vm"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-winapi-build" ,rust-winapi-build "src")))
  (inputs `(("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/retep998/winapi-rs")
  (synopsis
    "Contains function definitions for the Windows API library kernel32. See winapi for types and constants.")
  (description
    "Contains function definitions for the Windows API library kernel32.  See winapi for types and constants.")
  (license #f))
)

(define-public rust-net2
(package
  (name "rust-net2")
  (version "0.2.33")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "net2" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "126g3fgfxp06zimc1l9iyxnn9cif1hjsg7sd81nlls5nnyghsma2"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/rust-lang-nursery/net2-rs")
  (synopsis
    "Extensions to the standard library's networking types as proposed in RFC 1158.
")
  (description
    "Extensions to the standard library's networking types as proposed in RFC 1158.
")
  (license #f))
)

(define-public rust-winapi-build
(package
  (name "rust-winapi-build")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "winapi-build" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1g4rqsgjky0a7530qajn2bbfcrl2v0zb39idgdws9b1l7gp5wc9d"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/retep998/winapi-rs")
  (synopsis
    "Common code for build.rs in WinAPI -sys crates.")
  (description
    "Common code for build.rs in WinAPI -sys crates.")
  (license #f))
)

(define-public rust-tokio-io-pool
(package
  (name "rust-tokio-io-pool")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-io-pool" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0za1952lq8wf230gpxq73zlnzrnv9xbapybi12q2mld3p1f1rcwx"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tokio-current-thread"
       ,rust-tokio-current-thread
       "src")))
  (inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-tokio" ,rust-tokio "src")
      ("rust-tokio-executor"
       ,rust-tokio-executor
       "src")))
  (home-page
    "https://github.com/jonhoo/tokio-io-pool")
  (synopsis
    "Alternative tokio thread pool for I/O-heavy applications")
  (description
    "Alternative tokio thread pool for I/O-heavy applications")
  (license #f))
)

(define-public rust-parking-lot
(package
  (name "rust-parking-lot")
  (version "0.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "parking_lot" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0dz32cqx9200n1lk3kwyb599vabfid3f8sj1aq85sw42s2pb8hdb"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-lock-api" ,rust-lock-api "src")
      ("rust-parking-lot-core"
       ,rust-parking-lot-core
       "src")))
  (home-page
    "https://github.com/Amanieu/parking_lot")
  (synopsis
    "More compact and efficient implementations of the standard synchronization primitives.")
  (description
    "More compact and efficient implementations of the standard synchronization primitives.")
  (license #f))
)

(define-public rust-lock-api
(package
  (name "rust-lock-api")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "lock_api" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0b24q9mh258xa52ap636q1sxz0j5vrnp0hwbbh7ddjka3wwz3sv2"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-owning-ref" ,rust-owning-ref "src")
      ("rust-scopeguard" ,rust-scopeguard "src")))
  (home-page
    "https://github.com/Amanieu/parking_lot")
  (synopsis
    "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
  (description
    "Wrappers to create fully-featured Mutex and RwLock types.  Compatible with no_std.")
  (license #f))
)

(define-public rust-parking-lot-core
(package
  (name "rust-parking-lot-core")
  (version "0.4.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "parking_lot_core" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1jcq8aq4wv9y5fip7jg12jdwjd5g5r3x857xdma8vcin769cgj4l"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rustc-version" ,rust-rustc-version "src")))
  (inputs
    `(("rust-backtrace" ,rust-backtrace "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-petgraph" ,rust-petgraph "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-smallvec" ,rust-smallvec "src")
      ("rust-thread-id" ,rust-thread-id "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/Amanieu/parking_lot")
  (synopsis
    "An advanced API for creating custom synchronization primitives.")
  (description
    "An advanced API for creating custom synchronization primitives.")
  (license #f))
)

(define-public rust-owning-ref
(package
  (name "rust-owning-ref")
  (version "0.4.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "owning_ref" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "04zgwy77lin8qz398s6g44467pd6kjhbrlqifkia5rkr47mbi929"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-stable-deref-trait"
       ,rust-stable-deref-trait
       "src")))
  (home-page
    "https://github.com/Kimundi/owning-ref-rs")
  (synopsis
    "A library for creating references that carry their owner with them.")
  (description
    "This package provides a library for creating references that carry their owner with them.")
  (license #f))
)

(define-public rust-stable-deref-trait
(package
  (name "rust-stable-deref-trait")
  (version "1.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "stable_deref_trait" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1j2lkgakksmz4vc5hfawcch2ipiskrhjs1sih0f3br7s7rys58fv"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/storyyeller/stable_deref_trait")
  (synopsis
    "An unsafe marker trait for types like Box and Rc that dereference to a stable address even when moved, and hence can be used with libraries such as owning_ref and rental.
")
  (description
    "An unsafe marker trait for types like Box and Rc that dereference to a stable address even when moved, and hence can be used with libraries such as owning_ref and rental.
")
  (license #f))
)

(define-public rust-petgraph
(package
  (name "rust-petgraph")
  (version "0.4.13")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "petgraph" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0kyfmca854s54jk26g2x1kjb04c3k7cjilaxyr0if8lhxv8mjdlw"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-defmac" ,rust-defmac "src")
      ("rust-itertools" ,rust-itertools "src")
      ("rust-odds" ,rust-odds "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-fixedbitset" ,rust-fixedbitset "src")
      ("rust-ordermap" ,rust-ordermap "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (home-page "https://github.com/bluss/petgraph")
  (synopsis
    "Graph data structure library. Provides graph types and graph algorithms.")
  (description
    "Graph data structure library.  Provides graph types and graph algorithms.")
  (license #f))
)

(define-public rust-thread-id
(package
  (name "rust-thread-id")
  (version "3.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "thread-id" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1h90v19fjz3x9b25ywh68z5yf2zsmm6h5zb4rl302ckbsp4z9yy7"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-redox-syscall" ,rust-redox-syscall "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/ruuda/thread-id")
  (synopsis "Get a unique thread ID")
  (description "Get a unique thread ID")
  (license #f))
)

(define-public rust-odds
(package
  (name "rust-odds")
  (version "0.3.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "odds" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0rdnxa0na4897yb0svb3figz35g4imxjv61yfm2j21gbh5q8v8d9"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-itertools" ,rust-itertools "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-memchr" ,rust-memchr "src")
      ("rust-quickcheck" ,rust-quickcheck "src")))
  (inputs
    `(("rust-rawpointer" ,rust-rawpointer "src")
      ("rust-rawslice" ,rust-rawslice "src")
      ("rust-unchecked-index"
       ,rust-unchecked-index
       "src")))
  (home-page "https://github.com/bluss/odds")
  (synopsis
    "Odds and ends â\x80\x94 collection miscellania. Extra functionality for slices (`.find()`, `RevSlice`), strings and other things. Things in odds may move to more appropriate crates if we find them.
")
  (description
    "Odds and ends â\x80\x94 collection miscellania.  Extra functionality for slices (`.find()`, `RevSlice`), strings and other things.  Things in odds may move to more appropriate crates if we find them.
")
  (license #f))
)

(define-public rust-fixedbitset
(package
  (name "rust-fixedbitset")
  (version "0.1.9")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fixedbitset" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0czam11mi80dbyhf4rd4lz0ihcf7vkfchrdcrn45wbs0h40dxm46"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/bluss/fixedbitset")
  (synopsis
    "FixedBitSet is a simple bitset collection")
  (description
    "FixedBitSet is a simple bitset collection")
  (license #f))
)

(define-public rust-ordermap
(package
  (name "rust-ordermap")
  (version "0.4.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ordermap" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1m0vxmlm1x92m1ydgpddzg5mrfk3ddy8gk3r9dmpml18qrs9ch4i"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-fnv" ,rust-fnv "src")
      ("rust-itertools" ,rust-itertools "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-serde-test" ,rust-serde-test "src")))
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/bluss/ordermap")
  (synopsis
    "A hash table with consistent order and fast iteration. NOTE: This crate was renamed to indexmap. Please use it under its new name.")
  (description
    "This package provides a hash table with consistent order and fast iteration.  NOTE: This crate was renamed to indexmap.  Please use it under its new name.")
  (license #f))
)

(define-public rust-rawslice
(package
  (name "rust-rawslice")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rawslice" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09bympww1rpsd422da3w444q5w1znjbjh7mjninhq9gaaygkpci2"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")))
  (inputs
    `(("rust-rawpointer" ,rust-rawpointer "src")))
  (home-page "https://github.com/bluss/rawslice/")
  (synopsis
    "Reimplementation of the slice iterators, with extra features. For example
creation from raw pointers and start, end pointer accessors.
")
  (description
    "Reimplementation of the slice iterators, with extra features.  For example
creation from raw pointers and start, end pointer accessors.
")
  (license #f))
)

(define-public rust-unchecked-index
(package
  (name "rust-unchecked-index")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "unchecked-index" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0p6qcai1mjayx59cpgk27d0zgw9hz9r1ira5jiqil66f4ba8dfpf"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/bluss/unchecked-index")
  (synopsis
    "Unchecked indexing wrapper using regular index syntax.")
  (description
    "Unchecked indexing wrapper using regular index syntax.")
  (license #f))
)

(define-public rust-loom
(package
  (name "rust-loom")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "loom" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1jmp5mffwwyqgp914cwz92ij2s6vk1hsnkvgndvzw74xrcfraibj"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-generator" ,rust-generator "src")
      ("rust-scoped-tls" ,rust-scoped-tls "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (home-page "https://github.com/carllerche/loom")
  (synopsis "Model checker for concurrent code")
  (description "Model checker for concurrent code")
  (license #f))
)

(define-public rust-tokio-mock-task
(package
  (name "rust-tokio-mock-task")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-mock-task" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1y7q83qfk9ljjfvs82b453pmz9x1v3d6kr4x55j8mal01s6790dw"))))
  (build-system cargo-build-system)
  (inputs `(("rust-futures" ,rust-futures "src")))
  (home-page
    "https://github.com/carllerche/tokio-mock-task")
  (synopsis "Mock a Tokio task
")
  (description "Mock a Tokio task
")
  (license #f))
)

(define-public rust-generator
(package
  (name "rust-generator")
  (version "0.6.13")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "generator" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0hwjah7165s2209kpi7mac69jzk3ay1h8n7cdnxmf1a5ipsih2h3"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cc" ,rust-cc "src")
      ("rust-rustc-version" ,rust-rustc-version "src")))
  (inputs `(("rust-log" ,rust-log "src")))
  (home-page
    "https://github.com/Xudong-Huang/generator-rs.git")
  (synopsis "Stackfull Generator Library in Rust")
  (description
    "Stackfull Generator Library in Rust")
  (license #f))
)

(define-public rust-threadpool
(package
  (name "rust-threadpool")
  (version "1.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "threadpool" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0rd89n1q7vy47w4c32cnynibffv9kj3jy3dwr0536n9lbw5ckw72"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-num-cpus" ,rust-num-cpus "src")))
  (home-page
    "https://github.com/rust-threadpool/rust-threadpool")
  (synopsis
    "A thread pool for running a number of jobs on a fixed set of worker threads.
")
  (description
    "This package provides a thread pool for running a number of jobs on a fixed set of worker threads.
")
  (license #f))
)

(define-public rust-crossbeam-queue
(package
  (name "rust-crossbeam-queue")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-queue" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0jsa9dbxnwqcxfws09vaschf92d4imlbbikmcn4ka8z7rzb9r5vw"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")))
  (home-page
    "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
  (synopsis "Concurrent queues")
  (description "Concurrent queues")
  (license #f))
)

(define-public rust-pkg-config
(package
  (name "rust-pkg-config")
  (version "0.3.14")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pkg-config" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "135ia995lqzr0gxpk85h0bjxf82kj6hbxdx924sh9jdln6r8wvk7"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")))
  (home-page
    "https://github.com/alexcrichton/pkg-config-rs")
  (synopsis
    "A library to run the pkg-config system tool at build time in order to be used in
Cargo build scripts.
")
  (description
    "This package provides a library to run the pkg-config system tool at build time in order to be used in
Cargo build scripts.
")
  (license #f))
)

(define-public rust-vcpkg
(package
  (name "rust-vcpkg")
  (version "0.2.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "vcpkg" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0g8l2gpc889bl4ja7fx0syr4sgj0w6zq63kx5hdkf4ivxg9rdwny"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-tempdir" ,rust-tempdir "src")))
  (home-page "https://github.com/mcgoo/vcpkg-rs")
  (synopsis
    "A library to find native dependencies in a vcpkg tree at build
time in order to be used in Cargo build scripts.
")
  (description
    "This package provides a library to find native dependencies in a vcpkg tree at build
time in order to be used in Cargo build scripts.
")
  (license #f))
)

(define-public rust-crc
(package
  (name "rust-crc")
  (version "1.8.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crc" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1sqal6gm6lbj7f45iv3rw2s9w3pvvha8v970y51s7k7mwy6m8qyn"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-build-const" ,rust-build-const "src")))
  (home-page
    "https://github.com/mrhooray/crc-rs.git")
  (synopsis
    "Rust implementation of CRC(16, 32, 64) with support of various standards")
  (description
    "Rust implementation of CRC(16, 32, 64) with support of various standards")
  (license #f))
)

(define-public rust-miniz-oxide
(package
  (name "rust-miniz-oxide")
  (version "0.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "miniz_oxide" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "07ppnzf6win6qp845zkf8vand2j8z1wr0b5vs2jm3mh7kwvg4s64"))))
  (build-system cargo-build-system)
  (inputs `(("rust-adler32" ,rust-adler32 "src")))
  (home-page
    "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
  (synopsis
    "DEFLATE compression and decompression library rewritten in Rust based on miniz")
  (description
    "DEFLATE compression and decompression library rewritten in Rust based on miniz")
  (license #f))
)

(define-public rust-build-const
(package
  (name "rust-build-const")
  (version "0.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "build_const" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0faz882spx9474cszay2djmb0lghbwq51qayabcar1s7g4r2l29r"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/vitiral/build_const")
  (synopsis
    "library for creating importable constants from build.rs or a script")
  (description
    "library for creating importable constants from build.rs or a script")
  (license #f))
)

(define-public rust-adler32
(package
  (name "rust-adler32")
  (version "1.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "adler32" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0p7fxlnks9l7p7rwfqi7aqgnk2bps5zc0rjiw00mdw19nnbjjlky"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (home-page
    "https://github.com/remram44/adler32-rs")
  (synopsis
    "Minimal Adler32 implementation for Rust.")
  (description
    "Minimal Adler32 implementation for Rust.")
  (license #f))
)

(define-public rust-seahash
(package
  (name "rust-seahash")
  (version "3.0.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "seahash" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1pr8ijnxnp68ki4m4740yc5mr01zijf86yx07wbsqzwiyhghdmhq"))))
  (build-system cargo-build-system)
  (home-page
    "https://gitlab.redox-os.org/redox-os/seahash")
  (synopsis
    "A blazingly fast, portable hash function with proven statistical guarantees.")
  (description
    "This package provides a blazingly fast, portable hash function with proven statistical guarantees.")
  (license #f))
)

(define-public rust-pico-sys
(package
  (name "rust-pico-sys")
  (version "0.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pico-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1q5pg0ms6szz6b5h26h4k40zb76zbwwjgyigac4wly9qngdj4yl5"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-gcc" ,rust-gcc "src")))
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/reem/rust-pico-sys.git")
  (synopsis "Bindings to the PicoHTTPParser.")
  (description "Bindings to the PicoHTTPParser.")
  (license #f))
)

(define-public rust-hyper
(package
  (name "rust-hyper")
  (version "0.12.27")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "hyper" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0j7qiz5dl7gmq1rbxg473v8n3n6rpv9wrkgxwi66xbr69x1pf9sg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-futures-timer" ,rust-futures-timer "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-pretty-env-logger"
       ,rust-pretty-env-logger
       "src")
      ("rust-rustc-version" ,rust-rustc-version "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-spmc" ,rust-spmc "src")
      ("rust-tokio-fs" ,rust-tokio-fs "src")
      ("rust-tokio-mockstream"
       ,rust-tokio-mockstream
       "src")
      ("rust-url" ,rust-url "src")))
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-futures-cpupool"
       ,rust-futures-cpupool
       "src")
      ("rust-h2" ,rust-h2 "src")
      ("rust-http" ,rust-http "src")
      ("rust-httparse" ,rust-httparse "src")
      ("rust-iovec" ,rust-iovec "src")
      ("rust-itoa" ,rust-itoa "src")
      ("rust-log" ,rust-log "src")
      ("rust-net2" ,rust-net2 "src")
      ("rust-time" ,rust-time "src")
      ("rust-tokio" ,rust-tokio "src")
      ("rust-tokio-executor"
       ,rust-tokio-executor
       "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-tokio-reactor" ,rust-tokio-reactor "src")
      ("rust-tokio-tcp" ,rust-tokio-tcp "src")
      ("rust-tokio-threadpool"
       ,rust-tokio-threadpool
       "src")
      ("rust-tokio-timer" ,rust-tokio-timer "src")
      ("rust-want" ,rust-want "src")))
  (home-page "https://hyper.rs")
  (synopsis "A fast and correct HTTP library.")
  (description
    "This package provides a fast and correct HTTP library.")
  (license #f))
)

(define-public rust-futures-timer
(package
  (name "rust-futures-timer")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-timer" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0hw0nlyrq5an6l6y8md1rg6r380zrddvmh9cg0h64xfwnvlxzkm5"))))
  (build-system cargo-build-system)
  (inputs `(("rust-futures" ,rust-futures "src")))
  (home-page
    "https://github.com/alexcrichton/futures-timer")
  (synopsis
    "Timeouts and intervals for futures.
")
  (description
    "Timeouts and intervals for futures.
")
  (license #f))
)

(define-public rust-pretty-env-logger
(package
  (name "rust-pretty-env-logger")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pretty_env_logger" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0qgxmypqxs831lv7n40yfr4ri5062dd8xpp5qbczgpkm0i73z2yz"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-chrono" ,rust-chrono "src")
      ("rust-env-logger" ,rust-env-logger "src")
      ("rust-log" ,rust-log "src")))
  (home-page
    "https://github.com/seanmonstar/pretty-env-logger")
  (synopsis "a visually pretty env_logger")
  (description "a visually pretty env_logger")
  (license #f))
)

(define-public rust-spmc
(package
  (name "rust-spmc")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "spmc" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "06mdy6zzjwld5jgb1zfrmzrg5yvf31dbiq2wwls1im2zzg8i27yd"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/seanmonstar/spmc")
  (synopsis "Simple SPMC channel")
  (description "Simple SPMC channel")
  (license #f))
)

(define-public rust-tokio-mockstream
(package
  (name "rust-tokio-mockstream")
  (version "1.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-mockstream" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0mg1i39cl8x32wxwbn74hlirks8a6f3g0gfzkb0n0zwbxwvc9gs1"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bytes" ,rust-bytes "src")))
  (inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-tokio-io" ,rust-tokio-io "src")))
  (home-page
    "https://github.com/aatxe/tokio-mockstream")
  (synopsis
    "A fake stream for testing network applications backed by buffers.")
  (description
    "This package provides a fake stream for testing network applications backed by buffers.")
  (license #f))
)

(define-public rust-h2
(package
  (name "rust-h2")
  (version "0.1.18")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "h2" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1g9057dy50rp895i66h02i38f1kw0581nr3izgg0s106vf365aw5"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-env-logger" ,rust-env-logger "src")
      ("rust-hex" ,rust-hex "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-rustls" ,rust-rustls "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-tokio" ,rust-tokio "src")
      ("rust-tokio-rustls" ,rust-tokio-rustls "src")
      ("rust-walkdir" ,rust-walkdir "src")
      ("rust-webpki" ,rust-webpki "src")
      ("rust-webpki-roots" ,rust-webpki-roots "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-bytes" ,rust-bytes "src")
      ("rust-fnv" ,rust-fnv "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-http" ,rust-http "src")
      ("rust-indexmap" ,rust-indexmap "src")
      ("rust-log" ,rust-log "src")
      ("rust-slab" ,rust-slab "src")
      ("rust-string" ,rust-string "src")
      ("rust-tokio-io" ,rust-tokio-io "src")))
  (home-page "https://github.com/carllerche/h2")
  (synopsis "An HTTP/2.0 client and server")
  (description "An HTTP/2.0 client and server")
  (license #f))
)

(define-public rust-want
(package
  (name "rust-want")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "want" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0c52g7b4hhj033jc56sx9z3krivyciz0hlblixq2gc448zx5wfdn"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-futures" ,rust-futures "src")
      ("rust-log" ,rust-log "src")
      ("rust-try-lock" ,rust-try-lock "src")))
  (home-page "https://github.com/seanmonstar/want")
  (synopsis
    "Detect when another Future wants a result.")
  (description
    "Detect when another Future wants a result.")
  (license #f))
)

(define-public rust-heapsize
(package
  (name "rust-heapsize")
  (version "0.4.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "heapsize" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0q94q9ppqjgrw71swiyia4hgby2cz6dldp7ij57nkvhd6zmfcy8n"))))
  (build-system cargo-build-system)
  (inputs `(("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/servo/heapsize")
  (synopsis
    "Infrastructure for measuring the total runtime size of an object on the heap")
  (description
    "Infrastructure for measuring the total runtime size of an object on the heap")
  (license #f))
)

(define-public rust-idna
(package
  (name "rust-idna")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "idna" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0kl4gs5kaydn4v07c6ka33spm9qdh2np0x7iw7g5zd8z1c7rxw1q"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")
      ("rust-rustc-test" ,rust-rustc-test "src")))
  (inputs
    `(("rust-matches" ,rust-matches "src")
      ("rust-unicode-bidi" ,rust-unicode-bidi "src")
      ("rust-unicode-normalization"
       ,rust-unicode-normalization
       "src")))
  (home-page "https://github.com/servo/rust-url/")
  (synopsis
    "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
  (description
    "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
  (license #f))
)

(define-public rust-percent-encoding
(package
  (name "rust-percent-encoding")
  (version "1.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "percent-encoding" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0cgq08v1fvr6bs5fvy390cz830lq4fak8havdasdacxcw790s09i"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/servo/rust-url/")
  (synopsis "Percent encoding and decoding")
  (description "Percent encoding and decoding")
  (license #f))
)

(define-public rust-unicode-bidi
(package
  (name "rust-unicode-bidi")
  (version "0.3.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-bidi" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1malx8ljgm7v1gbaazkn7iicy5wj0bwcyadj3l727a38ch6bvwj9"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-test" ,rust-serde-test "src")))
  (inputs
    `(("rust-flame" ,rust-flame "src")
      ("rust-flamer" ,rust-flamer "src")
      ("rust-matches" ,rust-matches "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/servo/unicode-bidi")
  (synopsis
    "Implementation of the Unicode Bidirectional Algorithm")
  (description
    "Implementation of the Unicode Bidirectional Algorithm")
  (license #f))
)

(define-public rust-unicode-normalization
(package
  (name "rust-unicode-normalization")
  (version "0.1.8")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-normalization" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09i49va90rvia1agvgni4gicnqv50y5zy1naw8mr8bcqifh3j4ql"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-smallvec" ,rust-smallvec "src")))
  (home-page
    "https://github.com/unicode-rs/unicode-normalization")
  (synopsis
    "This crate provides functions for normalization of
Unicode strings, including Canonical and Compatible
Decomposition and Recomposition, as described in
Unicode Standard Annex #15.
")
  (description
    "This crate provides functions for normalization of
Unicode strings, including Canonical and Compatible
Decomposition and Recomposition, as described in
Unicode Standard Annex #15.
")
  (license #f))
)

(define-public rust-flame
(package
  (name "rust-flame")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "flame" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0c5bmhyimzxch3pmh0w3z9n57saasgix4bmbbksr9vp1c5j71hhz"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-thread-id" ,rust-thread-id "src")))
  (home-page "https://github.com/TyOverby/flame")
  (synopsis "a profiling / flamegraph library")
  (description "a profiling / flamegraph library")
  (license #f))
)

(define-public rust-flamer
(package
  (name "rust-flamer")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "flamer" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1b2d7jx80f3p7hqpgdi7wksaiq18k9w23p0cs2sxf7jbx2jx3bgj"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-flame" ,rust-flame "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://github.com/llogiq/flamer")
  (synopsis
    "a procedural macro to insert `flame::start_guard(_)` calls")
  (description
    "a procedural macro to insert `flame::start_guard(_)` calls")
  (license #f))
)

(define-public rust-hex
(package
  (name "rust-hex")
  (version "0.3.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "hex" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0xsdcjiik5j750j67zk42qdnmm4ahirk3gmkmcqgq7qls2jjcl40"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/KokaKiwi/rust-hex")
  (synopsis
    "Encoding and decoding data into/from hexadecimal representation.")
  (description
    "Encoding and decoding data into/from hexadecimal representation.")
  (license #f))
)

(define-public rust-rustls
(package
  (name "rust-rustls")
  (version "0.15.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0vh93fhqfbn4ysw4xzkpkpqdz36xixz4mhs1qllgldfq5iay6wgj"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-env-logger" ,rust-env-logger "src")
      ("rust-log" ,rust-log "src")
      ("rust-tempfile" ,rust-tempfile "src")
      ("rust-webpki-roots" ,rust-webpki-roots "src")))
  (inputs
    `(("rust-base64" ,rust-base64 "src")
      ("rust-log" ,rust-log "src")
      ("rust-ring" ,rust-ring "src")
      ("rust-sct" ,rust-sct "src")
      ("rust-untrusted" ,rust-untrusted "src")
      ("rust-webpki" ,rust-webpki "src")))
  (home-page "https://github.com/ctz/rustls")
  (synopsis
    "Rustls is a modern TLS library written in Rust.")
  (description
    "Rustls is a modern TLS library written in Rust.")
  (license #f))
)

(define-public rust-tokio-rustls
(package
  (name "rust-tokio-rustls")
  (version "0.10.0-alpha.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-rustls" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "170vdvv393fp4ki8ncwb3blnhabwcs5kdda0q2703scn8inxcskz"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-tokio" ,rust-tokio "src")
      ("rust-webpki-roots" ,rust-webpki-roots "src")))
  (inputs
    `(("rust-bytes" ,rust-bytes "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-iovec" ,rust-iovec "src")
      ("rust-rustls" ,rust-rustls "src")
      ("rust-tokio-io" ,rust-tokio-io "src")
      ("rust-webpki" ,rust-webpki "src")))
  (home-page
    "https://github.com/quininer/tokio-rustls")
  (synopsis
    "Asynchronous TLS/SSL streams for Tokio using Rustls.")
  (description
    "Asynchronous TLS/SSL streams for Tokio using Rustls.")
  (license #f))
)

(define-public rust-webpki
(package
  (name "rust-webpki")
  (version "0.19.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "webpki" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10nhyxlqsa4caxlxrijm5h79rdg6ld8hqy78ldjnnfhaj3biqzjg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-base64" ,rust-base64 "src")))
  (inputs
    `(("rust-ring" ,rust-ring "src")
      ("rust-untrusted" ,rust-untrusted "src")))
  (home-page
    "https://github.com/briansmith/webpki")
  (synopsis
    "Web PKI X.509 Certificate Verification.")
  (description
    "Web PKI X.509 Certificate Verification.")
  (license #f))
)

(define-public rust-webpki-roots
(package
  (name "rust-webpki-roots")
  (version "0.16.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "webpki-roots" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "03ny02mwqdgd2ff23k03kbwr2rrcaymxhp7jcjjikfh340hs83y1"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-untrusted" ,rust-untrusted "src")
      ("rust-webpki" ,rust-webpki "src")))
  (home-page "https://github.com/ctz/webpki-roots")
  (synopsis
    "Mozilla's CA root certificates for use with webpki")
  (description
    "Mozilla's CA root certificates for use with webpki")
  (license #f))
)

(define-public rust-string
(package
  (name "rust-string")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "string" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "158r3n6k1c0j9pb5lp7iajaiv0dh53xhmw2g8k1k93p36y4zpfyh"))))
  (build-system cargo-build-system)
  (inputs `(("rust-bytes" ,rust-bytes "src")))
  (home-page
    "https://github.com/carllerche/string")
  (synopsis
    "A UTF-8 encoded string with configurable byte storage.")
  (description
    "This package provides a UTF-8 encoded string with configurable byte storage.")
  (license #f))
)

(define-public rust-base64
(package
  (name "rust-base64")
  (version "0.10.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "base64" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "13k6bvd3n6dm7jqn9x918w65dd9xhx454bqphbnv0bkd6n9dj98b"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-criterion" ,rust-criterion "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")))
  (home-page
    "https://github.com/alicemaz/rust-base64")
  (synopsis
    "encodes and decodes base64 as bytes or utf8")
  (description
    "encodes and decodes base64 as bytes or utf8")
  (license #f))
)

(define-public rust-ring
(package
  (name "rust-ring")
  (version "0.14.6")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ring" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0g091akf4dpg9qj05z3gc4nlrs57mjj2bqab98gaqp79wf3c2ss2"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-spin" ,rust-spin "src")
      ("rust-untrusted" ,rust-untrusted "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/briansmith/ring")
  (synopsis "Safe, fast, small crypto using Rust.")
  (description
    "Safe, fast, small crypto using Rust.")
  (license #f))
)

(define-public rust-sct
(package
  (name "rust-sct")
  (version "0.5.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "sct" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1fb9ym5bwswx01yyggn7v2vfryih4vnqpp4r4ssv3qaqpn7xynig"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-ring" ,rust-ring "src")
      ("rust-untrusted" ,rust-untrusted "src")))
  (home-page "https://github.com/ctz/sct.rs")
  (synopsis
    "Certificate transparency SCT verification library")
  (description
    "Certificate transparency SCT verification library")
  (license #f))
)

(define-public rust-untrusted
(package
  (name "rust-untrusted")
  (version "0.6.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "untrusted" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0byf88b7ca1kb5aap8f6npp6xncvg95dnma8ipmnmd4n9r5izkam"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/briansmith/untrusted")
  (synopsis
    "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of untrusted inputs in Rust.")
  (description
    "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of untrusted inputs in Rust.")
  (license #f))
)

(define-public rust-same-file
(package
  (name "rust-same-file")
  (version "1.0.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "same-file" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0rsjk8zjppgc083jyx89hxi39xnjf12x2aqv3x6gz8d8afzc884g"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-winapi-util" ,rust-winapi-util "src")))
  (home-page
    "https://github.com/BurntSushi/same-file")
  (synopsis
    "A simple crate for determining whether two file paths point to the same file.
")
  (description
    "This package provides a simple crate for determining whether two file paths point to the same file.
")
  (license #f))
)

(define-public rust-winapi-util
(package
  (name "rust-winapi-util")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "winapi-util" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1j839dc6y8vszvrsb7yk0qvs0w6asnahxzbyans37vnsw6vbls3i"))))
  (build-system cargo-build-system)
  (inputs `(("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/BurntSushi/winapi-util")
  (synopsis
    "A dumping ground for high level safe wrappers over winapi.")
  (description
    "This package provides a dumping ground for high level safe wrappers over winapi.")
  (license #f))
)

(define-public rust-try-lock
(package
  (name "rust-try-lock")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "try-lock" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10p36rx6pqi9d0zr876xa8vksx2m66ha45myakl50rn08dxyn176"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/seanmonstar/try-lock")
  (synopsis "A lightweight atomic lock.")
  (description
    "This package provides a lightweight atomic lock.")
  (license #f))
)

(define-public rust-mio-uds
(package
  (name "rust-mio-uds")
  (version "0.6.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "mio-uds" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09gimdbnj7b9yca99pk8lxh9jhl79msj795c8fxi2sqr9slmfqln"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-iovec" ,rust-iovec "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-mio" ,rust-mio "src")))
  (home-page
    "https://github.com/alexcrichton/mio-uds")
  (synopsis
    "Unix domain socket bindings for mio
")
  (description
    "Unix domain socket bindings for mio
")
  (license #f))
)

(define-public rust-base-x
(package
  (name "rust-base-x")
  (version "0.2.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "base-x" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0p8f01yjqf8p7sdmbn52bsjnnz7imxklgmdl5nhsznr2x1ja4nnm"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bencher" ,rust-bencher "src")
      ("rust-json" ,rust-json "src")
      ("rust-rand" ,rust-rand "src")))
  (home-page "https://github.com/OrKoN/base-x-rs")
  (synopsis "Encode/decode any base")
  (description "Encode/decode any base")
  (license #f))
)

(define-public rust-sha1
(package
  (name "rust-sha1")
  (version "0.6.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "sha1" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "03gs2q4m67rn2p8xcdfxhip6mpgahdwm12bnb3vh90ahv9grhy95"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-openssl" ,rust-openssl "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/mitsuhiko/rust-sha1")
  (synopsis
    "Minimal implementation of SHA1 for Rust.")
  (description
    "Minimal implementation of SHA1 for Rust.")
  (license #f))
)

(define-public rust-json
(package
  (name "rust-json")
  (version "0.11.13")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "json" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1glhqf8nzqnrfd9d1bhd60c68ddcssrd8h1swp64apqm0ia4il4s"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/maciejhirsz/json-rust")
  (synopsis "JSON implementation in Rust")
  (description "JSON implementation in Rust")
  (license #f))
)

(define-public rust-openssl
(package
  (name "rust-openssl")
  (version "0.10.20")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "openssl" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1n9qi2gi6l3cl4i4h66m3n7aw5lcmpqa5qmgdjyw2jmc39w6n3as"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-hex" ,rust-hex "src")
      ("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-bitflags" ,rust-bitflags "src")
      ("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-foreign-types" ,rust-foreign-types "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-openssl-sys" ,rust-openssl-sys "src")))
  (home-page
    "https://github.com/sfackler/rust-openssl")
  (synopsis "OpenSSL bindings")
  (description "OpenSSL bindings")
  (license #f))
)

(define-public rust-foreign-types
(package
  (name "rust-foreign-types")
  (version "0.4.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "foreign-types" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ca4i38yrf9iy5k47lr1ylb3rvcbn36d81k5pr5kzf6kmj6p111n"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-foreign-types-macros"
       ,rust-foreign-types-macros
       "src")
      ("rust-foreign-types-shared"
       ,rust-foreign-types-shared
       "src")))
  (home-page
    "https://github.com/sfackler/foreign-types")
  (synopsis
    "A framework for Rust wrappers over C APIs")
  (description
    "This package provides a framework for Rust wrappers over C APIs")
  (license #f))
)

(define-public rust-openssl-sys
(package
  (name "rust-openssl-sys")
  (version "0.9.43")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "openssl-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0pa9plfy4njx2lj9hrvhdf7xsw1cmgsg551yc8avkmbxjls6ij1k"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cc" ,rust-cc "src")
      ("rust-openssl-src" ,rust-openssl-src "src")
      ("rust-pkg-config" ,rust-pkg-config "src")
      ("rust-rustc-version" ,rust-rustc-version "src")
      ("rust-vcpkg" ,rust-vcpkg "src")))
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/sfackler/rust-openssl")
  (synopsis "FFI bindings to OpenSSL")
  (description "FFI bindings to OpenSSL")
  (license #f))
)

(define-public rust-foreign-types-macros
(package
  (name "rust-foreign-types-macros")
  (version "0.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "foreign-types-macros" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "16yjigjcsklcwy2ad32l24k1nwm9n3bsnyhxc3z9whjbsrj60qk6"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/sfackler/foreign-types")
  (synopsis
    "An internal crate used by foreign-types")
  (description
    "An internal crate used by foreign-types")
  (license #f))
)

(define-public rust-foreign-types-shared
(package
  (name "rust-foreign-types-shared")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "foreign-types-shared" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0kanxlif1vp0ffh2r9l610jqbkmb3183yqykxq1z5w1vay2rn7y6"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/sfackler/foreign-types")
  (synopsis
    "An internal crate used by foreign-types")
  (description
    "An internal crate used by foreign-types")
  (license #f))
)

(define-public rust-openssl-src
(package
  (name "rust-openssl-src")
  (version "111.2.1+1.1.1b")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "openssl-src" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0f94spq1p9xzf6lcm4568d61c4mqfpiax58pyl6lpnm3qll4lbm4"))))
  (build-system cargo-build-system)
  (inputs `(("rust-cc" ,rust-cc "src")))
  (home-page
    "https://github.com/alexcrichton/openssl-src-rs")
  (synopsis
    "Source of OpenSSL and logic to build it.
")
  (description
    "Source of OpenSSL and logic to build it.
")
  (license #f))
)

(define-public rust-bindgen
(package
  (name "rust-bindgen")
  (version "0.49.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bindgen" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09lhi3zh7171jb4gr37l2w8k2xg230v2lfv85b8iz8xw4xxbdq9k"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-clap" ,rust-clap "src")
      ("rust-diff" ,rust-diff "src")
      ("rust-shlex" ,rust-shlex "src")))
  (inputs
    `(("rust-bitflags" ,rust-bitflags "src")
      ("rust-cexpr" ,rust-cexpr "src")
      ("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-clang-sys" ,rust-clang-sys "src")
      ("rust-clap" ,rust-clap "src")
      ("rust-env-logger" ,rust-env-logger "src")
      ("rust-hashbrown" ,rust-hashbrown "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-log" ,rust-log "src")
      ("rust-peeking-take-while"
       ,rust-peeking-take-while
       "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-shlex" ,rust-shlex "src")
      ("rust-which" ,rust-which "src")))
  (home-page
    "https://rust-lang.github.io/rust-bindgen/")
  (synopsis
    "Automatically generates Rust FFI bindings to C and C++ libraries.")
  (description
    "Automatically generates Rust FFI bindings to C and C++ libraries.")
  (license #f))
)

(define-public rust-shlex
(package
  (name "rust-shlex")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "shlex" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1lmv6san7g8dv6jdfp14m7bdczq9ss7j7bgsfqyqjc3jnjfippvz"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/comex/rust-shlex")
  (synopsis
    "Split a string into shell words, like Python's shlex.
")
  (description
    "Split a string into shell words, like Python's shlex.
")
  (license #f))
)

(define-public rust-cexpr
(package
  (name "rust-cexpr")
  (version "0.3.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cexpr" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1by64ini3f058pwad3immx5cc12wr0m0kwgaxa8apzym03mj9ym7"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-clang-sys" ,rust-clang-sys "src")))
  (inputs `(("rust-nom" ,rust-nom "src")))
  (home-page
    "https://github.com/jethrogb/rust-cexpr")
  (synopsis "A C expression parser and evaluator")
  (description
    "This package provides a C expression parser and evaluator")
  (license #f))
)

(define-public rust-clang-sys
(package
  (name "rust-clang-sys")
  (version "0.28.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "clang-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0m8h56yjwv19pbah4lrhmb8js9mhx6hi5gk0y4zzix89xjf2c9s2"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-glob" ,rust-glob "src")))
  (inputs
    `(("rust-glob" ,rust-glob "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-libloading" ,rust-libloading "src")))
  (home-page
    "https://github.com/KyleMayes/clang-sys")
  (synopsis "Rust bindings for libclang.")
  (description "Rust bindings for libclang.")
  (license #f))
)

(define-public rust-peeking-take-while
(package
  (name "rust-peeking-take-while")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "peeking_take_while" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "16bhqr6rdyrp12zv381cxaaqqd0pwysvm1q8h2ygihvypvfprc8r"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/fitzgen/peeking_take_while")
  (synopsis
    "Like `Iterator::take_while`, but calls the predicate on a peeked value. This allows you to use `Iterator::by_ref` and `Iterator::take_while` together, and still get the first value for which the `take_while` predicate returned false after dropping the `by_ref`.")
  (description
    "Like `Iterator::take_while`, but calls the predicate on a peeked value.  This allows you to use `Iterator::by_ref` and `Iterator::take_while` together, and still get the first value for which the `take_while` predicate returned false after dropping the `by_ref`.")
  (license #f))
)

(define-public rust-which
(package
  (name "rust-which")
  (version "2.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "which" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0r7i793sc0xqnd2fxnqbksj7j1kx65bwn81b8z49750v4c8cnymm"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tempdir" ,rust-tempdir "src")))
  (inputs
    `(("rust-failure" ,rust-failure "src")
      ("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/harryfei/which-rs.git")
  (synopsis
    "A Rust equivalent of Unix command \"which\". Locate installed execuable in cross platforms.")
  (description
    "This package provides a Rust equivalent of Unix command \"which\".  Locate installed execuable in cross platforms.")
  (license #f))
)

(define-public rust-nom
(package
  (name "rust-nom")
  (version "5.0.0-alpha1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "nom" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0vymwak1893vsl9qx9qdki0inzx1x9nz6rnrzp02n2fsbky3gbdk"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-criterion" ,rust-criterion "src")
      ("rust-jemallocator" ,rust-jemallocator "src")
      ("rust-version-check" ,rust-version-check "src")))
  (inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-memchr" ,rust-memchr "src")
      ("rust-regex" ,rust-regex "src")))
  (home-page "https://github.com/Geal/nom")
  (synopsis
    "A byte-oriented, zero-copy, parser combinators library")
  (description
    "This package provides a byte-oriented, zero-copy, parser combinators library")
  (license #f))
)

(define-public rust-libloading
(package
  (name "rust-libloading")
  (version "0.5.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "libloading" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1hl8bgw54qc9ffg1fykkj9km5wgin2bhilc3rli5i36bsxhdcflw"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (inputs `(("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/nagisa/rust_libloading/")
  (synopsis
    "A safer binding to platformâ\x80\x99s dynamic library loading utilities")
  (description
    "This package provides a safer binding to platformâ\x80\x99s dynamic library loading utilities")
  (license #f))
)

(define-public rust-jemallocator
(package
  (name "rust-jemallocator")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "jemallocator" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0apy9ypvsajn03psxwi7xjbzpn663f95f6sfrpzhybfgldir3dn2"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-paste" ,rust-paste "src")))
  (inputs
    `(("rust-jemalloc-sys" ,rust-jemalloc-sys "src")
      ("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/alexcrichton/jemallocator")
  (synopsis
    "A Rust allocator backed by jemalloc
")
  (description
    "This package provides a Rust allocator backed by jemalloc
")
  (license #f))
)

(define-public rust-jemalloc-sys
(package
  (name "rust-jemalloc-sys")
  (version "0.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "jemalloc-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1p1bnnj2wiqcpg7arwa9wa4fg7dx4j1ksvj61gcdyy3mwd60vvvv"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cc" ,rust-cc "src")
      ("rust-fs-extra" ,rust-fs-extra "src")))
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/alexcrichton/jemallocator")
  (synopsis "Rust FFI bindings to jemalloc
")
  (description "Rust FFI bindings to jemalloc
")
  (license #f))
)

(define-public rust-fs-extra
(package
  (name "rust-fs-extra")
  (version "1.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fs_extra" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0x6675wdhsx277k1k1235jwcv38naf20d8kwrk948ds26hh4lajz"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/webdesus/fs_extra")
  (synopsis
    "Expanding opportunities standard library std::fs and std::io. Recursively copy folders with recept information about process and much more.")
  (description
    "Expanding opportunities standard library std::fs and std::io.  Recursively copy folders with recept information about process and much more.")
  (license #f))
)

(define-public rust-linked-hash-map
(package
  (name "rust-linked-hash-map")
  (version "0.5.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "linked-hash-map" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10qgbvh00q36ql0jh00rxh2jlq6qvl11n6mig0cvkpf4xf5bd4df"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-clippy" ,rust-clippy "src")
      ("rust-heapsize" ,rust-heapsize "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-test" ,rust-serde-test "src")))
  (home-page
    "https://github.com/contain-rs/linked-hash-map")
  (synopsis
    "A HashMap wrapper that holds key-value pairs in insertion order")
  (description
    "This package provides a HashMap wrapper that holds key-value pairs in insertion order")
  (license #f))
)

(define-public rust-lipsum
(package
  (name "rust-lipsum")
  (version "0.6.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "lipsum" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0nlxkz8zjxqmbrxqasr36a5fqn2n33cxy11w0x0a0b6mcx04dr2q"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rand-xorshift" ,rust-rand-xorshift "src")
      ("rust-version-sync" ,rust-version-sync "src")))
  (inputs `(("rust-rand" ,rust-rand "src")))
  (home-page "https://github.com/mgeisler/lipsum/")
  (synopsis
    "Lipsum is a lorem ipsum text generation library. Use this if you need
some filler text for your application.

The text is generated using a simple Markov chain, which you can also
instantiate to generate your own pieces of pseudo-random text.
")
  (description
    "Lipsum is a lorem ipsum text generation library.  Use this if you need
some filler text for your application.

The text is generated using a simple Markov chain, which you can also
instantiate to generate your own pieces of pseudo-random text.
")
  (license #f))
)

(define-public rust-hyphenation
(package
  (name "rust-hyphenation")
  (version "0.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "hyphenation" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0k5msv8calmnfd5kw1rmq4bg5hn1vcd39kbsxl57sdld63xwd4q4"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-atlatl" ,rust-atlatl "src")
      ("rust-bincode" ,rust-bincode "src")
      ("rust-hyphenation-commons"
       ,rust-hyphenation-commons
       "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-pocket-resources"
       ,rust-pocket-resources
       "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-unicode-normalization"
       ,rust-unicode-normalization
       "src")
      ("rust-unicode-segmentation"
       ,rust-unicode-segmentation
       "src")))
  (inputs
    `(("rust-atlatl" ,rust-atlatl "src")
      ("rust-bincode" ,rust-bincode "src")
      ("rust-hyphenation-commons"
       ,rust-hyphenation-commons
       "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/tapeinosyne/hyphenation")
  (synopsis
    "Knuth-Liang hyphenation for a variety of languages")
  (description
    "Knuth-Liang hyphenation for a variety of languages")
  (license #f))
)

(define-public rust-atlatl
(package
  (name "rust-atlatl")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "atlatl" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "18kyvdm56fdb52b1sryi80xgs3nkjdylynsv324aiqnj85l1bfrj"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-fst" ,rust-fst "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-fnv" ,rust-fnv "src")
      ("rust-num-traits" ,rust-num-traits "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/tapeinosyne/atlatl")
  (synopsis "Double-array tries.")
  (description "Double-array tries.")
  (license #f))
)

(define-public rust-hyphenation-commons
(package
  (name "rust-hyphenation-commons")
  (version "0.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "hyphenation_commons" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1pasnbk3rbdgf30jjjh1h24a9pxpdrnn0ihcivmpnzqha6mn2d4y"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-atlatl" ,rust-atlatl "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/tapeinosyne/hyphenation")
  (synopsis
    "Proemial code for the `hyphenation` library")
  (description
    "Proemial code for the `hyphenation` library")
  (license #f))
)

(define-public rust-pocket-resources
(package
  (name "rust-pocket-resources")
  (version "0.3.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pocket-resources" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1n2i5vmi8fdbw89wm5nz1ws1z9f1qax911p6ksg4scmdg23z6df1"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/tomaka/pocket-resources")
  (synopsis
    "Include resources in your applications.")
  (description
    "Include resources in your applications.")
  (license #f))
)

(define-public rust-unicode-segmentation
(package
  (name "rust-unicode-segmentation")
  (version "1.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-segmentation" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1la46g0f9r9pppv7kdv9k226hiad6ai0za0lpinirzfx2by28q5a"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")))
  (home-page
    "https://github.com/unicode-rs/unicode-segmentation")
  (synopsis
    "This crate provides Grapheme Cluster and Word boundaries
according to Unicode Standard Annex #29 rules.
")
  (description
    "This crate provides Grapheme Cluster and Word boundaries
according to Unicode Standard Annex #29 rules.
")
  (license #f))
)

(define-public rust-fst
(package
  (name "rust-fst")
  (version "0.3.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fst" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1pwb4gqpdwa1h4cjp1nkv6blhm4ccr2aym6xpg6ndxfzlxn14wnv"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-fnv" ,rust-fnv "src")
      ("rust-fst-levenshtein"
       ,rust-fst-levenshtein
       "src")
      ("rust-fst-regex" ,rust-fst-regex "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-memmap" ,rust-memmap "src")))
  (home-page "https://github.com/BurntSushi/fst")
  (synopsis
    "Use finite state transducers to compactly represents sets or maps of many
strings (> 1 billion is possible).
")
  (description
    "Use finite state transducers to compactly represents sets or maps of many
strings (> 1 billion is possible).
")
  (license #f))
)

(define-public rust-fst-levenshtein
(package
  (name "rust-fst-levenshtein")
  (version "0.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fst-levenshtein" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1s5ml10442bbnpmilmwjh4pfixsj6837rg68vjzg63i3djd4524y"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-fst" ,rust-fst "src")
      ("rust-utf8-ranges" ,rust-utf8-ranges "src")))
  (home-page "https://github.com/BurntSushi/fst")
  (synopsis
    "Search finite state transducers with fuzzy queries using Levenshtein automata.
")
  (description
    "Search finite state transducers with fuzzy queries using Levenshtein automata.
")
  (license #f))
)

(define-public rust-fst-regex
(package
  (name "rust-fst-regex")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fst-regex" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "126xrv3s8mrq8nqsahmpy0nlks6l3wlivqyf6a0i4g7d3vcs3b47"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-fst" ,rust-fst "src")
      ("rust-regex-syntax" ,rust-regex-syntax "src")
      ("rust-utf8-ranges" ,rust-utf8-ranges "src")))
  (home-page "https://github.com/BurntSushi/fst")
  (synopsis
    "Search finite state transducers with regular expression.
")
  (description
    "Search finite state transducers with regular expression.
")
  (license #f))
)

(define-public rust-afl
(package
  (name "rust-afl")
  (version "0.4.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "afl" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0g2chc18ji7qxi0d03n2ai140qdcww958v5si6rcjnnhmri1vyfb"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rustc-version" ,rust-rustc-version "src")
      ("rust-xdg" ,rust-xdg "src")))
  (inputs
    `(("rust-cc" ,rust-cc "src")
      ("rust-clap" ,rust-clap "src")
      ("rust-rustc-version" ,rust-rustc-version "src")
      ("rust-xdg" ,rust-xdg "src")))
  (home-page "https://github.com/rust-fuzz/afl.rs")
  (synopsis
    "Fuzzing Rust code with american-fuzzy-lop")
  (description
    "Fuzzing Rust code with american-fuzzy-lop")
  (license #f))
)

(define-public rust-xdg
(package
  (name "rust-xdg")
  (version "2.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "xdg" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0mws8a0fr3cqk5nh7aq9lmkmhzghvasqy4mhw6nnza06l4d6i2fh"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/whitequark/rust-xdg")
  (synopsis
    "A library for storing and retrieving files according to XDG Base Directory specification")
  (description
    "This package provides a library for storing and retrieving files according to XDG Base Directory specification")
  (license #f))
)

(define-public rust-crossbeam
(package
  (name "rust-crossbeam")
  (version "0.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "03gy8gzjb42kr4pfbd4kzmmxdm866j1y745z42d9j4513h3r4i5i"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-cfg-if" ,rust-cfg-if "src")
      ("rust-crossbeam-channel"
       ,rust-crossbeam-channel
       "src")
      ("rust-crossbeam-deque"
       ,rust-crossbeam-deque
       "src")
      ("rust-crossbeam-epoch"
       ,rust-crossbeam-epoch
       "src")
      ("rust-crossbeam-queue"
       ,rust-crossbeam-queue
       "src")
      ("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")))
  (home-page
    "https://github.com/crossbeam-rs/crossbeam")
  (synopsis "Tools for concurrent programming")
  (description "Tools for concurrent programming")
  (license #f))
)

(define-public rust-test-assembler
(package
  (name "rust-test-assembler")
  (version "0.1.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "test-assembler" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1sdx9hk0dk3z9crm8834ysyxsi92chls8arpd0gs796kis6lik2w"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")))
  (home-page
    "https://github.com/luser/rust-test-assembler")
  (synopsis
    "A set of types for building complex binary streams.")
  (description
    "This package provides a set of types for building complex binary streams.")
  (license #f))
)

(define-public rust-crossbeam-channel
(package
  (name "rust-crossbeam-channel")
  (version "0.3.8")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-channel" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0azpymyk0ld4akrjfy69ck5pzfgz1f2gb3smm2ywld92vsjd23hg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rand" ,rust-rand "src")
      ("rust-signal-hook" ,rust-signal-hook "src")))
  (inputs
    `(("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")
      ("rust-smallvec" ,rust-smallvec "src")))
  (home-page
    "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-channel")
  (synopsis
    "Multi-producer multi-consumer channels for message passing")
  (description
    "Multi-producer multi-consumer channels for message passing")
  (license #f))
)

(define-public rust-signal-hook
(package
  (name "rust-signal-hook")
  (version "0.1.8")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "signal-hook" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "089h7qii58p36ffpshx4p9m1wni007bg7rll0bmyr2ri4bkpm94p"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-tokio" ,rust-tokio "src")
      ("rust-version-sync" ,rust-version-sync "src")))
  (inputs
    `(("rust-arc-swap" ,rust-arc-swap "src")
      ("rust-futures" ,rust-futures "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-mio" ,rust-mio "src")
      ("rust-mio-uds" ,rust-mio-uds "src")
      ("rust-tokio-reactor" ,rust-tokio-reactor "src")))
  (home-page
    "https://github.com/vorner/signal-hook")
  (synopsis "Unix signal handling")
  (description "Unix signal handling")
  (license #f))
)

(define-public rust-arc-swap
(package
  (name "rust-arc-swap")
  (version "0.3.11")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "arc-swap" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ha8724dpki985v52ifq5sd98xvpa5q51hyma52di75dbqbn4imw"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-crossbeam" ,rust-crossbeam "src")
      ("rust-crossbeam-utils"
       ,rust-crossbeam-utils
       "src")
      ("rust-itertools" ,rust-itertools "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-model" ,rust-model "src")
      ("rust-num-cpus" ,rust-num-cpus "src")
      ("rust-parking-lot" ,rust-parking-lot "src")
      ("rust-proptest" ,rust-proptest "src")
      ("rust-version-sync" ,rust-version-sync "src")))
  (home-page "https://github.com/vorner/arc-swap")
  (synopsis "Atomically swappable Arc")
  (description "Atomically swappable Arc")
  (license #f))
)

(define-public rust-model
(package
  (name "rust-model")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "model" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0kx6hy5i1fn2qs4x6hpng9jixpm68g83vm24z8bqqscr317yinb6"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-permutohedron" ,rust-permutohedron "src")
      ("rust-proptest" ,rust-proptest "src")))
  (home-page "https://github.com/spacejam/model")
  (synopsis
    "model-based testing for data structures, with linearizability checking")
  (description
    "model-based testing for data structures, with linearizability checking")
  (license #f))
)

(define-public rust-goblin
(package
  (name "rust-goblin")
  (version "0.0.22")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "goblin" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1a76i6zz71hjwd11pwmc8iirddj6345mfp02zl5d6bzb04sdambz"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-log" ,rust-log "src")
      ("rust-plain" ,rust-plain "src")
      ("rust-scroll" ,rust-scroll "src")))
  (home-page "https://github.com/m4b/goblin")
  (synopsis
    "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
  (description
    "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
  (license #f))
)

(define-public rust-parity-wasm
(package
  (name "rust-parity-wasm")
  (version "0.38.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "parity-wasm" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0qpfwb9adyi6g98q1w0xiqdzkv4r1p7b2w19wd5cr57rlwifbmr0"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-time" ,rust-time "src")))
  (home-page
    "https://github.com/paritytech/parity-wasm")
  (synopsis
    "WebAssembly binary format serialization/deserialization/interpreter")
  (description
    "WebAssembly binary format serialization/deserialization/interpreter")
  (license #f))
)

(define-public rust-scroll
(package
  (name "rust-scroll")
  (version "0.9.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "scroll" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "10q3w86bn22xrjlfg1c90dfi9c26qjkzn26nad0i9z8pxwad311g"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-rayon" ,rust-rayon "src")
      ("rust-rustc-version" ,rust-rustc-version "src")))
  (inputs
    `(("rust-scroll-derive" ,rust-scroll-derive "src")))
  (home-page "https://github.com/m4b/scroll")
  (synopsis
    "A suite of powerful, extensible, generic, endian-aware Read/Write traits for byte buffers")
  (description
    "This package provides a suite of powerful, extensible, generic, endian-aware Read/Write traits for byte buffers")
  (license #f))
)

(define-public rust-uuid
(package
  (name "rust-uuid")
  (version "0.7.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "uuid" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ank4xk20x3nrz926w8j9mz53bi3v8bykxmhlq2pffa8xc8wdnwh"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bincode" ,rust-bincode "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-serde-test" ,rust-serde-test "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-md5" ,rust-md5 "src")
      ("rust-rand" ,rust-rand "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-sha1" ,rust-sha1 "src")
      ("rust-slog" ,rust-slog "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page "https://github.com/uuid-rs/uuid")
  (synopsis
    "A library to generate and parse UUIDs.")
  (description
    "This package provides a library to generate and parse UUIDs.")
  (license #f))
)

(define-public rust-plain
(package
  (name "rust-plain")
  (version "0.2.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "plain" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/randomites/plain")
  (synopsis
    "A small Rust library that allows users to reinterpret data of certain types safely.")
  (description
    "This package provides a small Rust library that allows users to reinterpret data of certain types safely.")
  (license #f))
)

(define-public rust-scroll-derive
(package
  (name "rust-scroll-derive")
  (version "0.9.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "scroll_derive" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1jqg5mm8nvii6avl1z1rc89agzh2kwkppgpsnwfakxg78mnaj6lg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-scroll" ,rust-scroll "src")))
  (inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/m4b/scroll_derive")
  (synopsis
    "A macros 1.1 derive implementation for Pread and Pwrite traits from the scroll crate")
  (description
    "This package provides a macros 1.1 derive implementation for Pread and Pwrite traits from the scroll crate")
  (license #f))
)

(define-public rust-md5
(package
  (name "rust-md5")
  (version "0.6.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "md5" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "17b2xm4h4cvxsdjsf3kdrzqv2za60kak961xzi5kmw6g6djcssvy"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/stainless-steel/md5")
  (synopsis
    "The package provides the MD5 hash function.")
  (description
    "The package provides the MD5 hash function.")
  (license #f))
)

(define-public rust-slog
(package
  (name "rust-slog")
  (version "2.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "slog" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "13jh74jlckzh5cygkhs0k4r82wnmw8ha2km829xwslhr83n2w6hy"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-erased-serde" ,rust-erased-serde "src")))
  (home-page "https://github.com/slog-rs/slog")
  (synopsis
    "Structured, extensible, composable logging for Rust")
  (description
    "Structured, extensible, composable logging for Rust")
  (license #f))
)

(define-public rust-erased-serde
(package
  (name "rust-erased-serde")
  (version "0.3.9")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "erased-serde" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0q7bnxs5zskfq5iillig55g7891dllcxh2p8y8k1p2j72syf9viv"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-cbor" ,rust-serde-cbor "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/dtolnay/erased-serde")
  (synopsis
    "Type-erased Serialize and Serializer traits")
  (description
    "Type-erased Serialize and Serializer traits")
  (license #f))
)

(define-public rust-serde-cbor
(package
  (name "rust-serde-cbor")
  (version "0.9.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_cbor" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1zdjazfp6grvrk030872m9zkn604sm0vws4bx1bws5hv76anvka5"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-bytes" ,rust-serde-bytes "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-half" ,rust-half "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/pyfisch/cbor")
  (synopsis "CBOR support for serde.")
  (description "CBOR support for serde.")
  (license #f))
)

(define-public rust-half
(package
  (name "rust-half")
  (version "1.3.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "half" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0diqajg3mgar511hxswl4kgqqz9a026yvn3103x5h2smknlc4lwk"))))
  (build-system cargo-build-system)
  (inputs `(("rust-serde" ,rust-serde "src")))
  (home-page
    "https://github.com/starkat99/half-rs")
  (synopsis
    "Half-precision floating point f16 type for Rust implementing the IEEE 754-2008 binary16 type.")
  (description
    "Half-precision floating point f16 type for Rust implementing the IEEE 754-2008 binary16 type.")
  (license #f))
)

(define-public rust-libgit2-sys
(package
  (name "rust-libgit2-sys")
  (version "0.7.11")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "libgit2-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1wcvg2qqra2aviasvqcscl8gb2rnjnd6h998wy5dlmf2bnriqi28"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cc" ,rust-cc "src")
      ("rust-pkg-config" ,rust-pkg-config "src")))
  (inputs
    `(("rust-curl-sys" ,rust-curl-sys "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-libssh2-sys" ,rust-libssh2-sys "src")
      ("rust-libz-sys" ,rust-libz-sys "src")
      ("rust-openssl-sys" ,rust-openssl-sys "src")))
  (home-page
    "https://github.com/alexcrichton/git2-rs")
  (synopsis
    "Native bindings to the libgit2 library")
  (description
    "Native bindings to the libgit2 library")
  (license #f))
)

(define-public rust-openssl-probe
(package
  (name "rust-openssl-probe")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "openssl-probe" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1pijrdifgsdwd45b08c2g0dsmnhz7c3kmagb70839ngrd7d29bvp"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/alexcrichton/openssl-probe")
  (synopsis
    "Tool for helping to find SSL certificate locations on the system for OpenSSL
")
  (description
    "Tool for helping to find SSL certificate locations on the system for OpenSSL
")
  (license #f))
)

(define-public rust-curl-sys
(package
  (name "rust-curl-sys")
  (version "0.4.18")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "curl-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0hls7jlpixn8qbbd7sy6lf723izzzbhbwag8v23ji62v5l2s14cx"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cc" ,rust-cc "src")
      ("rust-pkg-config" ,rust-pkg-config "src")
      ("rust-vcpkg" ,rust-vcpkg "src")))
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-libnghttp2-sys"
       ,rust-libnghttp2-sys
       "src")
      ("rust-libz-sys" ,rust-libz-sys "src")
      ("rust-openssl-sys" ,rust-openssl-sys "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/alexcrichton/curl-rust")
  (synopsis
    "Native bindings to the libcurl library")
  (description
    "Native bindings to the libcurl library")
  (license #f))
)

(define-public rust-libssh2-sys
(package
  (name "rust-libssh2-sys")
  (version "0.2.11")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "libssh2-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "17dz3xxy5bc73sr52maa6wdqmw1a0ymznrgfzlxid2rng101yshj"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-cc" ,rust-cc "src")
      ("rust-pkg-config" ,rust-pkg-config "src")
      ("rust-vcpkg" ,rust-vcpkg "src")))
  (inputs
    `(("rust-libc" ,rust-libc "src")
      ("rust-libz-sys" ,rust-libz-sys "src")
      ("rust-openssl-sys" ,rust-openssl-sys "src")))
  (home-page
    "https://github.com/alexcrichton/ssh2-rs")
  (synopsis
    "Native bindings to the libssh2 library")
  (description
    "Native bindings to the libssh2 library")
  (license #f))
)

(define-public rust-libnghttp2-sys
(package
  (name "rust-libnghttp2-sys")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "libnghttp2-sys" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1x74vfrhmp78q6gswjrr4j53ciglimk8xaqy5mr0nwx4pmk7jpfp"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/alexcrichton/nghttp2-rs")
  (synopsis
    "FFI bindings for libnghttp2 (nghttp2)
")
  (description
    "FFI bindings for libnghttp2 (nghttp2)
")
  (license #f))
)

(define-public rust-data-encoding
(package
  (name "rust-data-encoding")
  (version "2.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "data-encoding" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "15xd6afhsjl08285piwczrafmckpp8i29padj8v12xhahshprx7l"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/ia0/data-encoding")
  (synopsis
    "Efficient and customizable data-encoding functions")
  (description
    "Efficient and customizable data-encoding functions")
  (license #f))
)

(define-public rust-constant-time-eq
(package
  (name "rust-constant-time-eq")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "constant_time_eq" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "17janp8n9dd6kjbbgqiayrh9fw81v4cq9rz04926s5nf4pi15w4g"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/cesarb/constant_time_eq")
  (synopsis
    "Compares two equal-sized byte strings in constant time.")
  (description
    "Compares two equal-sized byte strings in constant time.")
  (license #f))
)

(define-public rust-pretty-assertions
(package
  (name "rust-pretty-assertions")
  (version "0.6.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pretty_assertions" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09yl14gnmpygiqrdlsa64lcl4w6ydjl9m8jri6kgam0v9rjf309z"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-ansi-term" ,rust-ansi-term "src")
      ("rust-ctor" ,rust-ctor "src")
      ("rust-difference" ,rust-difference "src")
      ("rust-output-vt100" ,rust-output-vt100 "src")))
  (home-page
    "https://github.com/colin-kiegel/rust-pretty-assertions")
  (synopsis
    "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs.")
  (description
    "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs.")
  (license #f))
)

(define-public rust-skeptic
(package
  (name "rust-skeptic")
  (version "0.13.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "skeptic" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0rai61hbs65nbvbhqlk1nap5hlav5qx3zmjjjzh9rhgxagc8xyyn"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-unindent" ,rust-unindent "src")))
  (inputs
    `(("rust-bytecount" ,rust-bytecount "src")
      ("rust-cargo-metadata"
       ,rust-cargo-metadata
       "src")
      ("rust-error-chain" ,rust-error-chain "src")
      ("rust-glob" ,rust-glob "src")
      ("rust-pulldown-cmark"
       ,rust-pulldown-cmark
       "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-tempdir" ,rust-tempdir "src")
      ("rust-walkdir" ,rust-walkdir "src")))
  (home-page
    "https://github.com/budziq/rust-skeptic")
  (synopsis
    "Test your Rust markdown documentation via Cargo")
  (description
    "Test your Rust markdown documentation via Cargo")
  (license #f))
)

(define-public rust-darling
(package
  (name "rust-darling")
  (version "0.9.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "darling" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1mnksf0i47pb7sxvi1iqfwmqy9iny0x8w56ilybpb431b46cpyzw"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (inputs
    `(("rust-darling-core" ,rust-darling-core "src")
      ("rust-darling-macro" ,rust-darling-macro "src")))
  (home-page
    "https://github.com/TedDriggs/darling")
  (synopsis
    "A proc-macro library for reading attributes into structs when
implementing custom derives.
")
  (description
    "This package provides a proc-macro library for reading attributes into structs when
implementing custom derives.
")
  (license #f))
)

(define-public rust-derive-builder-core
(package
  (name "rust-derive-builder-core")
  (version "0.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "derive_builder_core" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0a15iqb0d5qn5hq5xcq65gh2ghfzj40p1zwaz215pp51dgh4yj7v"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-pretty-assertions"
       ,rust-pretty-assertions
       "src")))
  (inputs
    `(("rust-darling" ,rust-darling "src")
      ("rust-log" ,rust-log "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/colin-kiegel/rust-derive-builder")
  (synopsis
    "Internal helper library for the derive_builder crate.")
  (description
    "Internal helper library for the derive_builder crate.")
  (license #f))
)

(define-public rust-ctor
(package
  (name "rust-ctor")
  (version "0.1.9")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ctor" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0b096a0i9q00gbkwhmqw2jn7zp9r4sw88039bwmjbha3jrhifk1v"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-libc-print" ,rust-libc-print "src")))
  (inputs
    `(("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/mmastrac/rust-ctor")
  (synopsis
    "__attribute__((constructor)) for Rust")
  (description
    "__attribute__((constructor)) for Rust")
  (license #f))
)

(define-public rust-output-vt100
(package
  (name "rust-output-vt100")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "output_vt100" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1ygqplpxz4gg3i8f3rkan2q69pqll7gv65l2mmd8r9dphnvwbkak"))))
  (build-system cargo-build-system)
  (inputs `(("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/Phundrak/output-vt100-rs")
  (synopsis
    "Utility to activate escape codes in Windows' CMD and PowerShell")
  (description
    "Utility to activate escape codes in Windows' CMD and PowerShell")
  (license #f))
)

(define-public rust-libc-print
(package
  (name "rust-libc-print")
  (version "0.1.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "libc-print" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1fpgdw9wmwpb89b5qhmmbliy02val8g6q2fcx2hc5lvxxlajirgp"))))
  (build-system cargo-build-system)
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/mmastrac/rust-libc-print")
  (synopsis
    "println! and eprintln! macros on libc without stdlib")
  (description
    "println! and eprintln! macros on libc without stdlib")
  (license #f))
)

(define-public rust-unindent
(package
  (name "rust-unindent")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "unindent" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1x21ilf78aqcq9xzb9b7i628wm10rhk0jp0chlv06rkc690l8jw3"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/dtolnay/indoc")
  (synopsis
    "Remove a column of leading whitespace from a string")
  (description
    "Remove a column of leading whitespace from a string")
  (license #f))
)

(define-public rust-bytecount
(package
  (name "rust-bytecount")
  (version "0.5.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "bytecount" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0z6a280kiy4kg5v3qw97pbyvwycr17fsm41804i8zpq7nmads3xy"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-criterion" ,rust-criterion "src")
      ("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-rand" ,rust-rand "src")))
  (inputs
    `(("rust-packed-simd" ,rust-packed-simd "src")))
  (home-page "https://github.com/llogiq/bytecount")
  (synopsis
    "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
  (description
    "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
  (license #f))
)

(define-public rust-cargo-metadata
(package
  (name "rust-cargo-metadata")
  (version "0.7.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "cargo_metadata" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0mrwh07k9vp3yzbwh7s27lbdlqkxwdsy5hd4cpr26hn382r6538p"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-clap" ,rust-clap "src")
      ("rust-docopt" ,rust-docopt "src")
      ("rust-structopt" ,rust-structopt "src")))
  (inputs
    `(("rust-error-chain" ,rust-error-chain "src")
      ("rust-semver" ,rust-semver "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (home-page
    "https://github.com/oli-obk/cargo_metadata")
  (synopsis
    "structured access to the output of `cargo metadata`")
  (description
    "structured access to the output of `cargo metadata`")
  (license #f))
)

(define-public rust-structopt
(package
  (name "rust-structopt")
  (version "0.2.15")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "structopt" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1lf8j6i89m238nz8p058dqbyxxamkcrl4v63cg9qg2ak2b1n01rx"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-clap" ,rust-clap "src")
      ("rust-structopt-derive"
       ,rust-structopt-derive
       "src")))
  (home-page
    "https://github.com/TeXitoi/structopt")
  (synopsis
    "Parse command line argument by defining a struct.")
  (description
    "Parse command line argument by defining a struct.")
  (license #f))
)

(define-public rust-structopt-derive
(package
  (name "rust-structopt-derive")
  (version "0.2.15")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "structopt-derive" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1illph1wp0ksirwk4nf2yzyj5dprhrmbfvrapkzychnha5ryp2jj"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-heck" ,rust-heck "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/TeXitoi/structopt")
  (synopsis
    "Parse command line argument by defining a struct, derive crate.")
  (description
    "Parse command line argument by defining a struct, derive crate.")
  (license #f))
)

(define-public rust-heck
(package
  (name "rust-heck")
  (version "0.3.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "heck" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "01a2v7yvkiqxakdqz4hw3w3g4sm52ivz9cs3qcsv2arxsmw4wmi0"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-unicode-segmentation"
       ,rust-unicode-segmentation
       "src")))
  (home-page
    "https://github.com/withoutboats/heck")
  (synopsis "heck is a case conversion library.")
  (description
    "heck is a case conversion library.")
  (license #f))
)

(define-public rust-darling-core
(package
  (name "rust-darling-core")
  (version "0.9.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "darling_core" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0b201dx4m58l5ac7gmbjvbf4z2xipnk5d4pqa7mz7gy3f21h3z3a"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-fnv" ,rust-fnv "src")
      ("rust-ident-case" ,rust-ident-case "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-strsim" ,rust-strsim "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/TedDriggs/darling")
  (synopsis
    "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.
")
  (description
    "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives.  Use https://crates.io/crates/darling in your code.
")
  (license #f))
)

(define-public rust-darling-macro
(package
  (name "rust-darling-macro")
  (version "0.9.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "darling_macro" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1lcq9418w0vmvncg4a3n9k64zjvqz0048aviqi0rmlpiqv0xmn66"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-darling-core" ,rust-darling-core "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page
    "https://github.com/TedDriggs/darling")
  (synopsis
    "Internal support for a proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.
")
  (description
    "Internal support for a proc-macro library for reading attributes into structs when
implementing custom derives.  Use https://crates.io/crates/darling in your code.
")
  (license #f))
)

(define-public rust-ident-case
(package
  (name "rust-ident-case")
  (version "1.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ident_case" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))))
  (build-system cargo-build-system)
  (home-page
    "https://github.com/TedDriggs/ident_case")
  (synopsis
    "Utility for applying case rules to Rust identifiers.")
  (description
    "Utility for applying case rules to Rust identifiers.")
  (license #f))
)

(define-public rust-ci-info
(package
  (name "rust-ci-info")
  (version "0.4.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ci_info" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0cmhcsxvgq7nh07s2wycz6xp02zrky6fv1cwfxrn1960gqs6ikyz"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-serde-derive" ,rust-serde-derive "src")))
  (home-page
    "http://github.com/sagiegurari/ci_info")
  (synopsis
    "Provides current CI environment information.")
  (description
    "Provides current CI environment information.")
  (license #f))
)

(define-public rust-console
(package
  (name "rust-console")
  (version "0.7.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "console" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0jqqwbhnwsp5imm4gp1rmzs4l9pk7bhm9bpi3rr0phrz7w6p5wrb"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-atty" ,rust-atty "src")
      ("rust-clicolors-control"
       ,rust-clicolors-control
       "src")
      ("rust-encode-unicode"
       ,rust-encode-unicode
       "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-parking-lot" ,rust-parking-lot "src")
      ("rust-regex" ,rust-regex "src")
      ("rust-termios" ,rust-termios "src")
      ("rust-unicode-width" ,rust-unicode-width "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/mitsuhiko/console")
  (synopsis
    "A terminal and console abstraction for Rust")
  (description
    "This package provides a terminal and console abstraction for Rust")
  (license #f))
)

(define-public rust-pest
(package
  (name "rust-pest")
  (version "2.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pest" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "134686mwxm73asbiads53zfchqvvcrsrsyax2cghfcizmvg8ac4k"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-serde-json" ,rust-serde-json "src")
      ("rust-ucd-trie" ,rust-ucd-trie "src")))
  (home-page "https://pest-parser.github.io/")
  (synopsis "The Elegant Parser")
  (description "The Elegant Parser")
  (license #f))
)

(define-public rust-pest-derive
(package
  (name "rust-pest-derive")
  (version "2.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pest_derive" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1l5jfa6ril71cw5nsiw0r45br54dd8cj2r1nc2d1wq6wb3jilgc3"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-pest" ,rust-pest "src")
      ("rust-pest-generator"
       ,rust-pest-generator
       "src")))
  (home-page "https://pest-parser.github.io/")
  (synopsis "pest's derive macro")
  (description "pest's derive macro")
  (license #f))
)

(define-public rust-ron
(package
  (name "rust-ron")
  (version "0.5.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ron" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1mb2bavvp8jg5wx0kx9n45anrsbjwhjzddim987bjaa11hg45kif"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-bytes" ,rust-serde-bytes "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs
    `(("rust-base64" ,rust-base64 "src")
      ("rust-bitflags" ,rust-bitflags "src")
      ("rust-serde" ,rust-serde "src")))
  (home-page "https://github.com/ron-rs/ron")
  (synopsis "Rusty Object Notation")
  (description "Rusty Object Notation")
  (license #f))
)

(define-public rust-serde-yaml
(package
  (name "rust-serde-yaml")
  (version "0.8.8")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_yaml" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "14s5ac0n0smmy20zgpr7ys609hzpmzvnnlm2dasmk5d6jzhai1q8"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-serde-derive" ,rust-serde-derive "src")
      ("rust-unindent" ,rust-unindent "src")
      ("rust-version-sync" ,rust-version-sync "src")))
  (inputs
    `(("rust-dtoa" ,rust-dtoa "src")
      ("rust-linked-hash-map"
       ,rust-linked-hash-map
       "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-yaml-rust" ,rust-yaml-rust "src")))
  (home-page
    "https://github.com/dtolnay/serde-yaml")
  (synopsis "YAML support for Serde")
  (description "YAML support for Serde")
  (license #f))
)

(define-public rust-clicolors-control
(package
  (name "rust-clicolors-control")
  (version "1.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "clicolors-control" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1y80cgarxhrd1bz5yjm81r444v6flvy36aaxrrsac0yhfd6gvavk"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-atty" ,rust-atty "src")
      ("rust-lazy-static" ,rust-lazy-static "src")
      ("rust-libc" ,rust-libc "src")
      ("rust-winapi" ,rust-winapi "src")))
  (home-page
    "https://github.com/mitsuhiko/clicolors-control")
  (synopsis
    "A common utility library to control CLI colorization")
  (description
    "This package provides a common utility library to control CLI colorization")
  (license #f))
)

(define-public rust-encode-unicode
(package
  (name "rust-encode-unicode")
  (version "0.3.5")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "encode_unicode" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1g8a8pixkxz6r927f4sc4r15qyc0szxdxb1732v8q7h0di4wkclh"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")))
  (inputs
    `(("rust-ascii" ,rust-ascii "src")
      ("rust-clippy" ,rust-clippy "src")))
  (home-page
    "https://github.com/tormol/encode_unicode")
  (synopsis
    "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.
")
  (description
    "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.
")
  (license #f))
)

(define-public rust-termios
(package
  (name "rust-termios")
  (version "0.3.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "termios" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "09any1p4jp4bphvb5ikagnvwjc3xn2djchy96nkpa782xb2j1dkj"))))
  (build-system cargo-build-system)
  (inputs `(("rust-libc" ,rust-libc "src")))
  (home-page
    "https://github.com/dcuddeback/termios-rs")
  (synopsis
    "Safe bindings for the termios library.")
  (description
    "Safe bindings for the termios library.")
  (license #f))
)

(define-public rust-ascii
(package
  (name "rust-ascii")
  (version "0.9.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ascii" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0dck6rsjnxlczyjnncn8hf16bxj42m1vi6s2n32c1jg2ijd9dz55"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-quickcheck" ,rust-quickcheck "src")
      ("rust-serde" ,rust-serde "src")
      ("rust-serde-test" ,rust-serde-test "src")))
  (home-page
    "https://github.com/tomprogrammer/rust-ascii")
  (synopsis
    "ASCII-only equivalents to `char`, `str` and `String`.")
  (description
    "ASCII-only equivalents to `char`, `str` and `String`.")
  (license #f))
)

(define-public rust-ucd-trie
(package
  (name "rust-ucd-trie")
  (version "0.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "ucd-trie" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0xwxkg0fyclbz8fl99iidq4gaw2jjngf8c6c8kqnqhkpzsqwbabi"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-lazy-static" ,rust-lazy-static "src")))
  (home-page "https://github.com/BurntSushi/rucd")
  (synopsis
    "A trie for storing Unicode codepoint sets and maps.
")
  (description
    "This package provides a trie for storing Unicode codepoint sets and maps.
")
  (license #f))
)

(define-public rust-pest-generator
(package
  (name "rust-pest-generator")
  (version "2.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pest_generator" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ipnv77lqhj4d4fpfxi8m168lcjp482kszaknlardmpgqiv0a4k3"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-pest" ,rust-pest "src")
      ("rust-pest-meta" ,rust-pest-meta "src")
      ("rust-proc-macro2" ,rust-proc-macro2 "src")
      ("rust-quote" ,rust-quote "src")
      ("rust-syn" ,rust-syn "src")))
  (home-page "https://pest-parser.github.io/")
  (synopsis "pest code generator")
  (description "pest code generator")
  (license #f))
)

(define-public rust-pest-meta
(package
  (name "rust-pest-meta")
  (version "2.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "pest_meta" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0kaprdz3jis9bjfwhri1zncbsvack5m3gx2g5flspdy7wxnyljgj"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-sha-1" ,rust-sha-1 "src")))
  (inputs
    `(("rust-maplit" ,rust-maplit "src")
      ("rust-pest" ,rust-pest "src")))
  (home-page "https://pest-parser.github.io/")
  (synopsis
    "pest meta language parser and validator")
  (description
    "pest meta language parser and validator")
  (license #f))
)

(define-public rust-sha-1
(package
  (name "rust-sha-1")
  (version "0.8.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "sha-1" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0s6fdy5wp3x4h2z4fcl2d9vjvrpzr87v4h49r51xcq8nm4qj35i3"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-digest" ,rust-digest "src")
      ("rust-hex-literal" ,rust-hex-literal "src")))
  (inputs
    `(("rust-block-buffer" ,rust-block-buffer "src")
      ("rust-digest" ,rust-digest "src")
      ("rust-fake-simd" ,rust-fake-simd "src")
      ("rust-opaque-debug" ,rust-opaque-debug "src")
      ("rust-sha1-asm" ,rust-sha1-asm "src")))
  (home-page
    "https://github.com/RustCrypto/hashes")
  (synopsis "SHA-1 hash function")
  (description "SHA-1 hash function")
  (license #f))
)

(define-public rust-maplit
(package
  (name "rust-maplit")
  (version "1.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "maplit" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0hsczmvd6zkqgzqdjp5hfyg7f339n68w83n4pxvnsszrzssbdjq8"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/bluss/maplit")
  (synopsis
    "Collection â\x80\x9cliteralâ\x80\x9d macros for HashMap, HashSet, BTreeMap, and BTreeSet.")
  (description
    "Collection â\x80\x9cliteralâ\x80\x9d macros for HashMap, HashSet, BTreeMap, and BTreeSet.")
  (license #f))
)

(define-public rust-digest
(package
  (name "rust-digest")
  (version "0.8.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "digest" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0p0kxsb6b2qgc8d28dzvppmwmnx7h77cf8hf05idhfjdk1k77x05"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-blobby" ,rust-blobby "src")
      ("rust-generic-array" ,rust-generic-array "src")))
  (home-page
    "https://github.com/RustCrypto/traits")
  (synopsis
    "Traits for cryptographic hash functions")
  (description
    "Traits for cryptographic hash functions")
  (license #f))
)

(define-public rust-hex-literal
(package
  (name "rust-hex-literal")
  (version "0.1.4")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "hex-literal" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0ffnn5g9q5xhdmzj2ic5hk9y18kyqflbmqcssqcya9gixs5r5hnx"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-hex-literal-impl"
       ,rust-hex-literal-impl
       "src")
      ("rust-proc-macro-hack"
       ,rust-proc-macro-hack
       "src")))
  (home-page "https://github.com/RustCrypto/utils")
  (synopsis
    "Procedural macro for converting hexadecimal string to byte array at compile time.")
  (description
    "Procedural macro for converting hexadecimal string to byte array at compile time.")
  (license #f))
)

(define-public rust-block-buffer
(package
  (name "rust-block-buffer")
  (version "0.7.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "block-buffer" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "12v8wizynqin0hqf140kmp9s38q223mp1b0hkqk8j5pk8720v560"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-block-padding" ,rust-block-padding "src")
      ("rust-byte-tools" ,rust-byte-tools "src")
      ("rust-byteorder" ,rust-byteorder "src")
      ("rust-generic-array" ,rust-generic-array "src")))
  (home-page "https://github.com/RustCrypto/utils")
  (synopsis
    "Fixed size buffer for block processing of data")
  (description
    "Fixed size buffer for block processing of data")
  (license #f))
)

(define-public rust-fake-simd
(package
  (name "rust-fake-simd")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "fake-simd" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1vfylvk4va2ivqx85603lyqqp0zk52cgbs4n5nfbbbqx577qm2p8"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/RustCrypto/utils")
  (synopsis
    "Crate for mimicking simd crate on stable Rust")
  (description
    "Crate for mimicking simd crate on stable Rust")
  (license #f))
)

(define-public rust-opaque-debug
(package
  (name "rust-opaque-debug")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "opaque-debug" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "02942l2gc7w5r4js7i9063x99szic5mzzk1055j83v4diqpbpxck"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/RustCrypto/utils")
  (synopsis
    "Macro for opaque Debug trait implementation")
  (description
    "Macro for opaque Debug trait implementation")
  (license #f))
)

(define-public rust-sha1-asm
(package
  (name "rust-sha1-asm")
  (version "0.4.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "sha1-asm" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1i1i8viy6y30mv9v5hwhg9w6b722qkyh9c6n8bn4d27jpv14pg0s"))))
  (build-system cargo-build-system)
  (native-inputs `(("rust-cc" ,rust-cc "src")))
  (home-page
    "https://github.com/RustCrypto/asm-hashes")
  (synopsis
    "Assembly implementation of SHA-1 compression function")
  (description
    "Assembly implementation of SHA-1 compression function")
  (license #f))
)

(define-public rust-blobby
(package
  (name "rust-blobby")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "blobby" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1xicpf3s2mi5xqnx8ps5mdych4ib5nh2nfsbrsg8ar8bjk1girbg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-byteorder" ,rust-byteorder "src")
      ("rust-hex" ,rust-hex "src")))
  (inputs
    `(("rust-byteorder" ,rust-byteorder "src")))
  (home-page "https://github.com/RustCrypto/utils")
  (synopsis
    "Iterator over simple binary blob storage")
  (description
    "Iterator over simple binary blob storage")
  (license #f))
)

(define-public rust-generic-array
(package
  (name "rust-generic-array")
  (version "0.13.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "generic-array" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1xn51659qkdvv78nrsy4sdjngi1d1k0blkcyxh3hsixd1x108f8x"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-bincode" ,rust-bincode "src")
      ("rust-serde-json" ,rust-serde-json "src")))
  (inputs
    `(("rust-serde" ,rust-serde "src")
      ("rust-typenum" ,rust-typenum "src")))
  (home-page
    "https://github.com/fizyk20/generic-array.git")
  (synopsis
    "Generic types implementing functionality of arrays")
  (description
    "Generic types implementing functionality of arrays")
  (license #f))
)

(define-public rust-typenum
(package
  (name "rust-typenum")
  (version "1.10.0")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "typenum" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "0sc1jirllfhdi52z1xv9yqzxzpk6v7vadd13n7wvs1wnjipn6bb1"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/paholg/typenum")
  (synopsis
    "Typenum is a Rust library for type-level numbers evaluated at compile time. It currently supports bits, unsigned integers, and signed integers. It also provides a type-level array of type-level numbers, but its implementation is incomplete.")
  (description
    "Typenum is a Rust library for type-level numbers evaluated at compile time.  It currently supports bits, unsigned integers, and signed integers.  It also provides a type-level array of type-level numbers, but its implementation is incomplete.")
  (license #f))
)

(define-public rust-hex-literal-impl
(package
  (name "rust-hex-literal-impl")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "hex-literal-impl" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1nnxqhyn9l998ma04ip79bmpqv1as6003s03g26ynhrr471p022j"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-proc-macro-hack"
       ,rust-proc-macro-hack
       "src")))
  (home-page "https://github.com/RustCrypto/utils")
  (synopsis
    "Internal implementation of the hex-literal crate")
  (description
    "Internal implementation of the hex-literal crate")
  (license #f))
)

(define-public rust-block-padding
(package
  (name "rust-block-padding")
  (version "0.1.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "block-padding" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "14d5mvrfnc0xxqcmgx1rg1k6c76wdwmkl8bgaqy0sn7b5a4malnp"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-byte-tools" ,rust-byte-tools "src")))
  (home-page "https://github.com/RustCrypto/utils")
  (synopsis
    "Padding and unpadding of messages divided into blocks.")
  (description
    "Padding and unpadding of messages divided into blocks.")
  (license #f))
)

(define-public rust-byte-tools
(package
  (name "rust-byte-tools")
  (version "0.3.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "byte-tools" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1mqi29wsm8njpl51pfwr31wmpzs5ahlcb40wsjyd92l90ixcmdg3"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/RustCrypto/utils")
  (synopsis "Bytes related utility functions")
  (description "Bytes related utility functions")
  (license #f))
)

(define-public rust-dtoa
(package
  (name "rust-dtoa")
  (version "0.4.3")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "dtoa" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1pc9fkpali122caa7pz9mm0vbijwr1iaby8m64yz26j1xd012c3d"))))
  (build-system cargo-build-system)
  (home-page "https://github.com/dtolnay/dtoa")
  (synopsis
    "Fast functions for printing floating-point primitives to an io::Write")
  (description
    "Fast functions for printing floating-point primitives to an io::Write")
  (license #f))
)

(define-public rust-wincolor
(package
  (name "rust-wincolor")
  (version "1.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "wincolor" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1fp9sxq63kw3vjjcjrl3f7px082pplzxcr3qza2n2pa6mq0xj7jn"))))
  (build-system cargo-build-system)
  (inputs
    `(("rust-winapi" ,rust-winapi "src")
      ("rust-winapi-util" ,rust-winapi-util "src")))
  (home-page
    "https://github.com/BurntSushi/termcolor/tree/master/wincolor")
  (synopsis
    "A simple Windows specific API for controlling text color in a Windows console.
")
  (description
    "This package provides a simple Windows specific API for controlling text color in a Windows console.
")
  (license #f))
)

(define-public rust-custom-derive
(package
  (name "rust-custom-derive")
  (version "0.1.7")
  (source
    (origin
      (method url-fetch)
      (uri (crate-uri "custom_derive" version))
      (file-name
        (string-append name "-" version ".tar.gz"))
      (sha256
        (base32
          "1f81bavw1wnykwh21hh4yyzigs6zl6f6pkk9p3car8kq95yfb2pg"))))
  (build-system cargo-build-system)
  (native-inputs
    `(("rust-rustc-serialize"
       ,rust-rustc-serialize
       "src")))
  (home-page
    "https://github.com/DanielKeep/rust-custom-derive/tree/custom_derive-master")
  (synopsis
    "(Note: superseded by `macro-attr`) This crate provides a macro that enables the use of custom derive attributes.")
  (description
    "(Note: superseded by `macro-attr`) This crate provides a macro that enables the use of custom derive attributes.")
  (license #f))
)
