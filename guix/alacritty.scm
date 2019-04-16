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
   (native-inputs
    `(("rust-serde-derive" ,rust-serde-derive "src")))
   (inputs
    `(("rust-serde-derive" ,rust-serde-derive "src")))
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

alacritty
