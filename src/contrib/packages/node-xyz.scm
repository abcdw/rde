;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
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

(define-module (contrib packages node-xyz)
  #:use-module (contrib packages node)
  #:use-module (gnu packages base)
  #:use-module (gnu packages node)
  #:use-module (gnu packages node-xyz)
  #:use-module (guix build-system node)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  ;; #:use-module (ice-9 textual-ports)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public node-typescript-4.7.3
  (package
    (name "node-typescript")
    (version "4.7.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/typescript/-/typescript-4.7.3.tgz")
        (sha256
          (base32
            "1d0wxv9rzgjn11wkiscalw878kj08mmn3q1a5ifcicxml2bg9v0m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://www.typescriptlang.org/")
    (synopsis
      "TypeScript is a language for application scale JavaScript development")
    (description
      "TypeScript is a language for application scale JavaScript development")
    (license license:asl2.0)))

(define-public node-typescript node-typescript-4.7.3)

(define-public node-commander-9.3.0
  (package
    (name "node-commander")
    (version "9.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/commander/-/commander-9.3.0.tgz")
        (sha256
          (base32
            "1kdk6l68zz6p7dslnr5mwgl0gmijqfw3krcwwy3q9r75lr6zavd1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/tj/commander.js#readme")
    (synopsis
      "the complete solution for node.js command-line programs")
    (description
      "the complete solution for node.js command-line programs")
    (license license:expat)))

(define-public node-jsonfile-6.1.0
  (package
    (name "node-jsonfile")
    (version "6.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/jsonfile/-/jsonfile-6.1.0.tgz")
        (sha256
          (base32
            "1csrz2dy4chva2qzjxpx6jxjbxqqm6jr64vb2zc3y4cj7b9yxn0b"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-graceful-fs" ,node-graceful-fs-4.2.10)
        ("node-universalify" ,node-universalify-2.0.0)))
    (home-page
      "https://github.com/jprichardson/node-jsonfile#readme")
    (synopsis "Easily read/write JSON files.")
    (description "Easily read/write JSON files.")
    (license license:expat)))

(define-public node-universalify-2.0.0
  (package
    (name "node-universalify")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/universalify/-/universalify-2.0.0.tgz")
        (sha256
          (base32
            "10a8wqni1k8rgcwxsb7x8ryjz5w33mwg856yb4qnf7ld52jgf527"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/RyanZim/universalify#readme")
    (synopsis
      "Make a callback- or promise-based function support both promises and callbacks.")
    (description
      "Make a callback- or promise-based function support both promises and callbacks.")
    (license license:expat)))

(define-public node-fs-extra-10.1.0
  (package
    (name "node-fs-extra")
    (version "10.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fs-extra/-/fs-extra-10.1.0.tgz")
        (sha256
          (base32
            "1shzjwi0jj6haqwji5mc45nb08gw0zyq6gy26mmv0sw65s2ngajh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-universalify" ,node-universalify-2.0.0)
        ("node-jsonfile" ,node-jsonfile-6.1.0)
        ("node-graceful-fs" ,node-graceful-fs-4.2.10)))
    (home-page
      "https://github.com/jprichardson/node-fs-extra")
    (synopsis
      "fs-extra contains methods that aren't included in the vanilla Node.js fs package. Such as recursive mkdir, copy, and remove.")
    (description
      "fs-extra contains methods that aren't included in the vanilla Node.js fs package. Such as recursive mkdir, copy, and remove.")
    (license license:expat)))

(define-public node-p-debounce-2.1.0
  (package
    (name "node-p-debounce")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/p-debounce/-/p-debounce-2.1.0.tgz")
        (sha256
          (base32
            "0wb1sa0xd7hzp318mymdas72lmk7mk7ff3vxm5k77r41s21nhz8n"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/p-debounce#readme")
    (synopsis
      "Debounce promise-returning & async functions")
    (description
      "Debounce promise-returning & async functions")
    (license license:expat)))

(define-public node-p-try-2.2.0
  (package
    (name "node-p-try")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/p-try/-/p-try-2.2.0.tgz")
        (sha256
          (base32
            "141pf5z1f3xmm5c0fdrfddsf7xfigjxfl103zh59bpwrk2wb5453"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/p-try#readme")
    (synopsis "`Start a promise chain")
    (description "`Start a promise chain")
    (license license:expat)))

(define-public node-p-limit-2.3.0
  (package
    (name "node-p-limit")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/p-limit/-/p-limit-2.3.0.tgz")
        (sha256
          (base32
            "15djin88kfxjdvzd7f2gnwblgclqljzqxiidm1pmrsyg14j4ajrq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-p-try" ,node-p-try-2.2.0)))
    (home-page
      "https://github.com/sindresorhus/p-limit#readme")
    (synopsis
      "Run multiple promise-returning & async functions with limited concurrency")
    (description
      "Run multiple promise-returning & async functions with limited concurrency")
    (license license:expat)))

(define-public node-p-locate-3.0.0
  (package
    (name "node-p-locate")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/p-locate/-/p-locate-3.0.0.tgz")
        (sha256
          (base32
            "1fbvw7ka1lgrhr7kynsjv7iqw1sdqqrh088py2r4kyjhbl8xzq70"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-p-limit" ,node-p-limit-2.3.0)))
    (home-page
      "https://github.com/sindresorhus/p-locate#readme")
    (synopsis
      "Get the first fulfilled promise that satisfies the provided testing function")
    (description
      "Get the first fulfilled promise that satisfies the provided testing function")
    (license license:expat)))

(define-public node-path-exists-3.0.0
  (package
    (name "node-path-exists")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-exists/-/path-exists-3.0.0.tgz")
        (sha256
          (base32
            "0b9j0s6mvbf7js1fsga1jx4k6c4k17yn9c1jlaiziqkmvi98gxyp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/path-exists#readme")
    (synopsis "Check if a path exists")
    (description "Check if a path exists")
    (license license:expat)))

(define-public node-locate-path-3.0.0
  (package
    (name "node-locate-path")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/locate-path/-/locate-path-3.0.0.tgz")
        (sha256
          (base32
            "1ghmaifkp6r47h6ygdgkf7srvxhc5qwhgjwq00ia250kpxww5xdm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-path-exists" ,node-path-exists-3.0.0)
        ("node-p-locate" ,node-p-locate-3.0.0)))
    (home-page
      "https://github.com/sindresorhus/locate-path#readme")
    (synopsis
      "Get the first path that exists on disk of multiple paths")
    (description
      "Get the first path that exists on disk of multiple paths")
    (license license:expat)))

(define-public node-find-up-3.0.0
  (package
    (name "node-find-up")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/find-up/-/find-up-3.0.0.tgz")
        (sha256
          (base32
            "0ln7mwc9b465l646xvjd0692yy6izi27xb4y7g8ffpvjl7s9fx4r"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-locate-path" ,node-locate-path-3.0.0)))
    (home-page
      "https://github.com/sindresorhus/find-up#readme")
    (synopsis
      "Find a file or directory by walking up parent directories")
    (description
      "Find a file or directory by walking up parent directories")
    (license license:expat)))

(define-public node-pkg-up-3.1.0
  (package
    (name "node-pkg-up")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/pkg-up/-/pkg-up-3.1.0.tgz")
        (sha256
          (base32
            "033ncy995cs401ywvrdjdcdi53c9hn2i0065aggyhlcssinbzmxl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-find-up" ,node-find-up-3.0.0)))
    (home-page
      "https://github.com/sindresorhus/pkg-up#readme")
    (synopsis "Find the closest package.json file")
    (description
      "Find the closest package.json file")
    (license license:expat)))

(define-public node-yallist-4.0.0
  (package
    (name "node-yallist")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/yallist/-/yallist-4.0.0.tgz")
        (sha256
          (base32
            "0jadz9mh1lzfk19bvqqlrg40ggfk2yyfyrpgj5c62dk54ym7h358"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/yallist#readme")
    (synopsis "Yet Another Linked List")
    (description "Yet Another Linked List")
    (license license:isc)))

(define-public node-lru-cache-6.0.0
  (package
    (name "node-lru-cache")
    (version "6.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lru-cache/-/lru-cache-6.0.0.tgz")
        (sha256
          (base32
            "0pnziizgv8jpg708ykywcjby0syjz1l2ll1j727rdxhw0gmhvr2w"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-yallist" ,node-yallist-4.0.0)))
    (home-page
      "https://github.com/isaacs/node-lru-cache#readme")
    (synopsis
      "A cache object that deletes the least-recently-used items.")
    (description
      "A cache object that deletes the least-recently-used items.")
    (license license:isc)))

(define-public node-semver-7.3.7
  (package
    (name "node-semver")
    (version "7.3.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/semver/-/semver-7.3.7.tgz")
        (sha256
          (base32
            "0x5pd50bcnim1inm4wgdnc7829ra14ss7qcdn80mdf39a3d85qlg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-lru-cache" ,node-lru-cache-6.0.0)))
    (home-page
      "https://github.com/npm/node-semver#readme")
    (synopsis
      "The semantic version parser used by npm.")
    (description
      "The semantic version parser used by npm.")
    (license license:isc)))

(define-public node-array-union-2.1.0
  (package
    (name "node-array-union")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/array-union/-/array-union-2.1.0.tgz")
        (sha256
          (base32
            "1ih8b5i4b06l71652xm8r89h89vdj8vp648s2a5bgr4kbzh4kmx8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/array-union#readme")
    (synopsis
      "Create an array of unique values, in order, from the input arrays")
    (description
      "Create an array of unique values, in order, from the input arrays")
    (license license:expat)))

(define-public node-path-type-4.0.0
  (package
    (name "node-path-type")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-type/-/path-type-4.0.0.tgz")
        (sha256
          (base32
            "15wvcgwg053hr2h11ja5swvdz3vvxciqq5aad0ara9qmzgwfh9f0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/path-type#readme")
    (synopsis
      "Check if a path is a file, directory, or symlink")
    (description
      "Check if a path is a file, directory, or symlink")
    (license license:expat)))

(define-public node-dir-glob-3.0.1
  (package
    (name "node-dir-glob")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/dir-glob/-/dir-glob-3.0.1.tgz")
        (sha256
          (base32
            "0wj53iqp275dlsg7v36kxv97fid5pan2girgnhbdqw0vj8qplmkp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-path-type" ,node-path-type-4.0.0)))
    (home-page
      "https://github.com/kevva/dir-glob#readme")
    (synopsis
      "Convert directories to glob compatible strings")
    (description
      "Convert directories to glob compatible strings")
    (license license:expat)))

(define-public node-nodelib-fs-stat-2.0.5
  (package
    (name "node-nodelib-fs-stat")
    (version "2.0.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@nodelib/fs.stat/-/fs.stat-2.0.5.tgz")
        (sha256
          (base32
            "0sqkaapvl86zldyw00j920hv4yncwb14nbgwnf3wl6pja4sm7y6q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://www.npmjs.com/package/node-nodelib-fs-stat")
    (synopsis
      "Get the status of a file with some features")
    (description
      "Get the status of a file with some features")
    (license license:expat)))

(define-public node-queue-microtask-1.2.3
  (package
    (name "node-queue-microtask")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/queue-microtask/-/queue-microtask-1.2.3.tgz")
        (sha256
          (base32
            "1kcyybqa9jqb339mr9i9fxa7b6pn7fl5fzm278a5x1h1l8h0mbzz"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/feross/queue-microtask")
    (synopsis
      "fast, tiny `queueMicrotask` shim for modern engines")
    (description
      "fast, tiny `queueMicrotask` shim for modern engines")
    (license license:expat)))

(define-public node-run-parallel-1.2.0
  (package
    (name "node-run-parallel")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/run-parallel/-/run-parallel-1.2.0.tgz")
        (sha256
          (base32
            "1j3syw7nnhr98sr9jngzmgqj33khjnl5rimhgbpih2vy6zsk38kb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-queue-microtask"
         ,node-queue-microtask-1.2.3)))
    (home-page
      "https://github.com/feross/run-parallel")
    (synopsis
      "Run an array of functions in parallel")
    (description
      "Run an array of functions in parallel")
    (license license:expat)))

(define-public node-nodelib-fs-scandir-2.1.5
  (package
    (name "node-nodelib-fs-scandir")
    (version "2.1.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@nodelib/fs.scandir/-/fs.scandir-2.1.5.tgz")
        (sha256
          (base32
            "0k7r1kjscdfbm2ckdgvq13zgycd4mci1admxn3dqp3n72yivy959"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-run-parallel" ,node-run-parallel-1.2.0)
        ("node-nodelib-fs-stat"
         ,node-nodelib-fs-stat-2.0.5)))
    (home-page
      "https://www.npmjs.com/package/node-nodelib-fs-scandir")
    (synopsis
      "List files and directories inside the specified directory")
    (description
      "List files and directories inside the specified directory")
    (license license:expat)))

(define-public node-reusify-1.0.4
  (package
    (name "node-reusify")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/reusify/-/reusify-1.0.4.tgz")
        (sha256
          (base32
            "1i1kl423618nfp3rjalyl810v7sxz2x04smrmfpafbzs2zahql5a"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mcollina/reusify#readme")
    (synopsis
      "Reuse objects and functions with style")
    (description
      "Reuse objects and functions with style")
    (license license:expat)))

(define-public node-fastq-1.13.0
  (package
    (name "node-fastq")
    (version "1.13.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fastq/-/fastq-1.13.0.tgz")
        (sha256
          (base32
            "1qy2wl0x9iakx3fd6sydj6c21lwvamm8v58632rznp2d41fm8bqa"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-reusify" ,node-reusify-1.0.4)))
    (home-page
      "https://github.com/mcollina/fastq#readme")
    (synopsis "Fast, in memory work queue")
    (description "Fast, in memory work queue")
    (license license:isc)))

(define-public node-nodelib-fs-walk-1.2.8
  (package
    (name "node-nodelib-fs-walk")
    (version "1.2.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@nodelib/fs.walk/-/fs.walk-1.2.8.tgz")
        (sha256
          (base32
            "0gbxfa920a6ykrl8a4phhvlwgybvivm2z10yyybww8mqd4gn5yfb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-fastq" ,node-fastq-1.13.0)
        ("node-nodelib-fs-scandir"
         ,node-nodelib-fs-scandir-2.1.5)))
    (home-page
      "https://www.npmjs.com/package/node-nodelib-fs-walk")
    (synopsis
      "A library for efficiently walking a directory recursively")
    (description
      "A library for efficiently walking a directory recursively")
    (license license:expat)))

(define-public node-glob-parent-5.1.2
  (package
    (name "node-glob-parent")
    (version "5.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/glob-parent/-/glob-parent-5.1.2.tgz")
        (sha256
          (base32
            "1mfna9lpp82lapng0qq5x4x5j10nhimcx36lg4m5k4wbs7msy5ln"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-is-glob" ,node-is-glob-4.0.3)))
    (home-page
      "https://github.com/gulpjs/glob-parent#readme")
    (synopsis
      "Extract the non-magic parent path from a glob string.")
    (description
      "Extract the non-magic parent path from a glob string.")
    (license license:isc)))

(define-public node-is-number-7.0.0
  (package
    (name "node-is-number")
    (version "7.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-number/-/is-number-7.0.0.tgz")
        (sha256
          (base32
            "07nmmpplsj1gxzng6fxhnnyfkif9fvhvxa89d5lrgkwqf42w2xbv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-number")
    (synopsis
      "Returns true if a number or string value is a finite number. Useful for regex matches, parsing, user input, etc.")
    (description
      "Returns true if a number or string value is a finite number. Useful for regex matches, parsing, user input, etc.")
    (license license:expat)))

(define-public node-to-regex-range-5.0.1
  (package
    (name "node-to-regex-range")
    (version "5.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/to-regex-range/-/to-regex-range-5.0.1.tgz")
        (sha256
          (base32
            "1ms2bgz2paqfpjv1xpwx67i3dns5j9gn99il6cx5r4qaq9g2afm6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-number" ,node-is-number-7.0.0)))
    (home-page
      "https://github.com/micromatch/to-regex-range")
    (synopsis
      "Pass two numbers, get a regex-compatible source string for matching ranges. Validated against more than 2.78 million test assertions.")
    (description
      "Pass two numbers, get a regex-compatible source string for matching ranges. Validated against more than 2.78 million test assertions.")
    (license license:expat)))

(define-public node-fill-range-7.0.1
  (package
    (name "node-fill-range")
    (version "7.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fill-range/-/fill-range-7.0.1.tgz")
        (sha256
          (base32
            "0wp93mwfgzcddi6ii62qx7gb082jgh0rfq6pgvv2xndjyaygvk98"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-to-regex-range"
         ,node-to-regex-range-5.0.1)))
    (home-page
      "https://github.com/jonschlinkert/fill-range")
    (synopsis
      "Fill in a range of numbers or letters, optionally passing an increment or `step` to use, or create a regex-compatible range with `options.toRegex`")
    (description
      "Fill in a range of numbers or letters, optionally passing an increment or `step` to use, or create a regex-compatible range with `options.toRegex`")
    (license license:expat)))

(define-public node-braces-3.0.2
  (package
    (name "node-braces")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/braces/-/braces-3.0.2.tgz")
        (sha256
          (base32
            "1kpaa113m54qc1n2zvs0p1ika4s9dzvcczlw8q66xkyliy982n3k"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-fill-range" ,node-fill-range-7.0.1)))
    (home-page
      "https://github.com/micromatch/braces")
    (synopsis
      "Bash-like brace expansion, implemented in JavaScript. Safer than other brace expansion libs, with complete support for the Bash 4.3 braces specification, without sacrificing speed.")
    (description
      "Bash-like brace expansion, implemented in JavaScript. Safer than other brace expansion libs, with complete support for the Bash 4.3 braces specification, without sacrificing speed.")
    (license license:expat)))

(define-public node-picomatch-2.3.1
  (package
    (name "node-picomatch")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/picomatch/-/picomatch-2.3.1.tgz")
        (sha256
          (base32
            "07y1h9gbbdyjdpwb461x7dai0yg7hcyijxrcnpbr1h37r2gfw50v"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/micromatch/picomatch")
    (synopsis
      "Blazing fast and accurate glob matcher written in JavaScript, with no dependencies and full support for standard and extended Bash glob features, including braces, extglobs, POSIX brackets, and regular expressions.")
    (description
      "Blazing fast and accurate glob matcher written in JavaScript, with no dependencies and full support for standard and extended Bash glob features, including braces, extglobs, POSIX brackets, and regular expressions.")
    (license license:expat)))

(define-public node-micromatch-4.0.5
  (package
    (name "node-micromatch")
    (version "4.0.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/micromatch/-/micromatch-4.0.5.tgz")
        (sha256
          (base32
            "1axxwnl1i0mibpbir72nvz76dzi43cv8lhyzpxw95056mddnlmi0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-picomatch" ,node-picomatch-2.3.1)
        ("node-braces" ,node-braces-3.0.2)))
    (home-page
      "https://github.com/micromatch/micromatch")
    (synopsis
      "Glob matching for javascript/node.js. A replacement and faster alternative to minimatch and multimatch.")
    (description
      "Glob matching for javascript/node.js. A replacement and faster alternative to minimatch and multimatch.")
    (license license:expat)))

(define-public node-fast-glob-3.2.11
  (package
    (name "node-fast-glob")
    (version "3.2.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fast-glob/-/fast-glob-3.2.11.tgz")
        (sha256
          (base32
            "1p1sxg1vnnqc45m0xd8fliscv5vchd3m56yxkjz4zfxb6cz8k3v0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-micromatch" ,node-micromatch-4.0.5)
        ("node-merge2" ,node-merge2-1.4.1)
        ("node-glob-parent" ,node-glob-parent-5.1.2)
        ("node-nodelib-fs-walk"
         ,node-nodelib-fs-walk-1.2.8)
        ("node-nodelib-fs-stat"
         ,node-nodelib-fs-stat-2.0.5)))
    (home-page
      "https://github.com/mrmlnc/fast-glob#readme")
    (synopsis
      "It's a very fast and efficient glob library for Node.js")
    (description
      "It's a very fast and efficient glob library for Node.js")
    (license license:expat)))

(define-public node-ignore-5.2.0
  (package
    (name "node-ignore")
    (version "5.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ignore/-/ignore-5.2.0.tgz")
        (sha256
          (base32
            "1kaia5s9yhayx63kgqdrnk38l2cyvyp6al31g4qkhhcplmqsfkfn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/kaelzhang/node-ignore#readme")
    (synopsis
      "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
    (description
      "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
    (license license:expat)))

(define-public node-merge2-1.4.1
  (package
    (name "node-merge2")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/merge2/-/merge2-1.4.1.tgz")
        (sha256
          (base32
            "10bq7m23r366fk3r6j058i1l4jz6vn5xlxcfnp2mkj5kr68i4f5q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/teambition/merge2")
    (synopsis
      "Merge multiple streams into one stream in sequence or parallel.")
    (description
      "Merge multiple streams into one stream in sequence or parallel.")
    (license license:expat)))

(define-public node-globby-11.1.0
  (package
    (name "node-globby")
    (version "11.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/globby/-/globby-11.1.0.tgz")
        (sha256
          (base32
            "1x7bjpz2s2j4y7hxc19vfj9rh3ck0693c9xa2z47qxa7b09xhx46"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-slash" ,node-slash-3.0.0)
        ("node-merge2" ,node-merge2-1.4.1)
        ("node-ignore" ,node-ignore-5.2.0)
        ("node-fast-glob" ,node-fast-glob-3.2.11)
        ("node-dir-glob" ,node-dir-glob-3.0.1)
        ("node-array-union" ,node-array-union-2.1.0)))
    (home-page
      "https://github.com/sindresorhus/globby#readme")
    (synopsis "User-friendly glob matching")
    (description "User-friendly glob matching")
    (license license:expat)))

(define-public node-graceful-fs-4.2.10
  (package
    (name "node-graceful-fs")
    (version "4.2.10")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.2.10.tgz")
        (sha256
          (base32
            "0vacs46qbczsrp1zgavd6rvyjm9riyzv45va5saqyrnncji5vl5r"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/node-graceful-fs#readme")
    (synopsis
      "A drop-in replacement for fs, making various improvements.")
    (description
      "A drop-in replacement for fs, making various improvements.")
    (license license:isc)))

(define-public node-is-extglob-2.1.1
  (package
    (name "node-is-extglob")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-extglob/-/is-extglob-2.1.1.tgz")
        (sha256
          (base32
            "06dwa2xzjx6az40wlvwj11vican2w46710b9170jzmka2j344pcc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-extglob")
    (synopsis
      "Returns true if a string has an extglob.")
    (description
      "Returns true if a string has an extglob.")
    (license license:expat)))

(define-public node-is-path-cwd-2.2.0
  (package
    (name "node-is-path-cwd")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-path-cwd/-/is-path-cwd-2.2.0.tgz")
        (sha256
          (base32
            "0bs3dmrz6ifsyy8ivd43jy30cibxnpl1rx02dmrsnqyn2gcrqjw3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/is-path-cwd#readme")
    (synopsis
      "Check if a path is the current working directory")
    (description
      "Check if a path is the current working directory")
    (license license:expat)))

(define-public node-is-path-inside-3.0.3
  (package
    (name "node-is-path-inside")
    (version "3.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-path-inside/-/is-path-inside-3.0.3.tgz")
        (sha256
          (base32
            "0dipvy02ypbyz43gyvsp3hjgaqmxs4lpjzww2xlyj3x2wrgnb4gn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/is-path-inside#readme")
    (synopsis
      "Check if a path is inside another path")
    (description
      "Check if a path is inside another path")
    (license license:expat)))

(define-public node-clean-stack-2.2.0
  (package
    (name "node-clean-stack")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/clean-stack/-/clean-stack-2.2.0.tgz")
        (sha256
          (base32
            "0manylf8kgqm9knb26lwxs7lfdf384r8hnxjwmhgzimq19k3fv05"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/clean-stack#readme")
    (synopsis "Clean up error stack traces")
    (description "Clean up error stack traces")
    (license license:expat)))

(define-public node-indent-string-4.0.0
  (package
    (name "node-indent-string")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/indent-string/-/indent-string-4.0.0.tgz")
        (sha256
          (base32
            "1822k378f65ipx6v9i132bywcnfjzk3rgilhnp443csfsz9p8sxw"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/indent-string#readme")
    (synopsis "Indent each line in a string")
    (description "Indent each line in a string")
    (license license:expat)))

(define-public node-aggregate-error-3.1.0
  (package
    (name "node-aggregate-error")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/aggregate-error/-/aggregate-error-3.1.0.tgz")
        (sha256
          (base32
            "0nr0ig4k5d5n019cjj4h4027316ppdjy8wnykv32b95bnnw0qdh3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-indent-string" ,node-indent-string-4.0.0)
        ("node-clean-stack" ,node-clean-stack-2.2.0)))
    (home-page
      "https://github.com/sindresorhus/aggregate-error#readme")
    (synopsis "Create an error from multiple errors")
    (description
      "Create an error from multiple errors")
    (license license:expat)))

(define-public node-p-map-4.0.0
  (package
    (name "node-p-map")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/p-map/-/p-map-4.0.0.tgz")
        (sha256
          (base32
            "147z64sp0ifrix961cr3f6sw2fs2wqx225d24mdbgvn3c5sj6c51"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-aggregate-error"
         ,node-aggregate-error-3.1.0)))
    (home-page
      "https://github.com/sindresorhus/p-map#readme")
    (synopsis "Map over promises concurrently")
    (description "Map over promises concurrently")
    (license license:expat)))

(define-public node-fs-realpath-1.0.0
  (package
    (name "node-fs-realpath")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz")
        (sha256
          (base32
            "174g5vay9jnd7h5q8hfdw6dnmwl1gdpn4a8sz0ysanhj2f3wp04y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/fs.realpath#readme")
    (synopsis
      "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
    (description
      "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
    (license license:isc)))

(define-public node-inflight-1.0.6
  (package
    (name "node-inflight")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz")
        (sha256
          (base32
            "16w864087xsh3q7f5gm3754s7bpsb9fq3dhknk9nmbvlk3sxr7ss"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-wrappy" ,node-wrappy-1.0.2)
        ("node-once" ,node-once-1.4.0)))
    (home-page "https://github.com/isaacs/inflight")
    (synopsis
      "Add callbacks to requests in flight to avoid async duplication")
    (description
      "Add callbacks to requests in flight to avoid async duplication")
    (license license:isc)))

(define-public node-inherits-2.0.4
  (package
    (name "node-inherits")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz")
        (sha256
          (base32
            "1bxg4igfni2hymabg8bkw86wd3qhhzhsswran47sridk3dnbqkfr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/inherits#readme")
    (synopsis
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (description
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (license license:isc)))

(define-public node-balanced-match-1.0.2
  (package
    (name "node-balanced-match")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.2.tgz")
        (sha256
          (base32
            "1hdwrr7qqb37plj7962xbwjx1jvjz7ahl7iqrwh82yhcvnmzfm6q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/juliangruber/balanced-match")
    (synopsis
      "Match balanced character pairs, like \"{\" and \"}\"")
    (description
      "Match balanced character pairs, like \"{\" and \"}\"")
    (license license:expat)))

(define-public node-concat-map-0.0.1
  (package
    (name "node-concat-map")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz")
        (sha256
          (base32
            "0qa2zqn9rrr2fqdki44s4s2dk2d8307i4556kv25h06g43b2v41m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/substack/node-concat-map")
    (synopsis "concatenative mapdashery")
    (description "concatenative mapdashery")
    (license license:expat)))

(define-public node-brace-expansion-1.1.11
  (package
    (name "node-brace-expansion")
    (version "1.1.11")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz")
        (sha256
          (base32
            "1nlmjvlwlp88knblnayns0brr7a9m2fynrlwq425lrpb4mcn9gc4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-concat-map" ,node-concat-map-0.0.1)
        ("node-balanced-match"
         ,node-balanced-match-1.0.2)))
    (home-page
      "https://github.com/juliangruber/brace-expansion")
    (synopsis
      "Brace expansion as known from sh/bash")
    (description
      "Brace expansion as known from sh/bash")
    (license license:expat)))

(define-public node-minimatch-3.1.2
  (package
    (name "node-minimatch")
    (version "3.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/minimatch/-/minimatch-3.1.2.tgz")
        (sha256
          (base32
            "0kd3h6q90kvmzzw1v7cc3dr911gjkb9s547cdvfncfqanq84p5hk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-brace-expansion"
         ,node-brace-expansion-1.1.11)))
    (home-page
      "https://github.com/isaacs/minimatch#readme")
    (synopsis "a glob matcher in javascript")
    (description "a glob matcher in javascript")
    (license license:isc)))

(define-public node-path-is-absolute-1.0.1
  (package
    (name "node-path-is-absolute")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz")
        (sha256
          (base32
            "0p7p04xxd8q495qhxmxydyjgzcf762dp1hp2wha2b52n3agp0vbf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/path-is-absolute#readme")
    (synopsis
      "Node.js 0.12 path.isAbsolute() ponyfill")
    (description
      "Node.js 0.12 path.isAbsolute() ponyfill")
    (license license:expat)))

(define-public node-slash-3.0.0
  (package
    (name "node-slash")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/slash/-/slash-3.0.0.tgz")
        (sha256
          (base32
            "01sxm2s0cvya4m52cb8w578a345nas5gwaw6dz7i2i9k9g8ig6pj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/slash#readme")
    (synopsis
      "Convert Windows backslash paths to slash paths")
    (description
      "Convert Windows backslash paths to slash paths")
    (license license:expat)))

(define-public node-del-6.1.1
  (package
    (name "node-del")
    (version "6.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/del/-/del-6.1.1.tgz")
        (sha256
          (base32
            "1z94j764hh0jrjm3vp25ny4f2f8frq4sbq8c2c0sdfg27f8bvv3j"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-slash" ,node-slash-3.0.0)
        ("node-rimraf" ,node-rimraf-3.0.2)
        ("node-p-map" ,node-p-map-4.0.0)
        ("node-is-path-inside"
         ,node-is-path-inside-3.0.3)
        ("node-is-path-cwd" ,node-is-path-cwd-2.2.0)
        ("node-is-glob" ,node-is-glob-4.0.3)
        ("node-graceful-fs" ,node-graceful-fs-4.2.10)
        ("node-globby" ,node-globby-11.1.0)))
    (home-page
      "https://github.com/sindresorhus/del#readme")
    (synopsis "Delete files and directories")
    (description "Delete files and directories")
    (license license:expat)))

(define-public node-is-stream-2.0.1
  (package
    (name "node-is-stream")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-stream/-/is-stream-2.0.1.tgz")
        (sha256
          (base32
            "1mxnc5nlh73zg34vwdd9k5mx2wcs5pc84j8a2yig2y0bl9rgy09m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/is-stream#readme")
    (synopsis
      "Check if something is a Node.js stream")
    (description
      "Check if something is a Node.js stream")
    (license license:expat)))

(define-public node-temp-dir-2.0.0
  (package
    (name "node-temp-dir")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/temp-dir/-/temp-dir-2.0.0.tgz")
        (sha256
          (base32
            "1hn8gx9dwlqhyxic2xka317anflf4dmqlq5frakrzv1l17fbyxsy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/temp-dir#readme")
    (synopsis
      "Get the real path of the system temp directory")
    (description
      "Get the real path of the system temp directory")
    (license license:expat)))

(define-public node-type-fest-0.16.0
  (package
    (name "node-type-fest")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/type-fest/-/type-fest-0.16.0.tgz")
        (sha256
          (base32
            "1mj0c1ijia8q655dh1swhg4700lqy4j3f1v8an63fyx8bmi1wmm7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/type-fest#readme")
    (synopsis
      "A collection of essential TypeScript types")
    (description
      "A collection of essential TypeScript types")
    (license #f)))

(define-public node-crypto-random-string-2.0.0
  (package
    (name "node-crypto-random-string")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/crypto-random-string/-/crypto-random-string-2.0.0.tgz")
        (sha256
          (base32
            "1angxzwxc9qiin2lbi5axp14aw2k3jdgbhc08fxp7h7hmnfk6s2m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/crypto-random-string#readme")
    (synopsis
      "Generate a cryptographically strong random string")
    (description
      "Generate a cryptographically strong random string")
    (license license:expat)))

(define-public node-unique-string-2.0.0
  (package
    (name "node-unique-string")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/unique-string/-/unique-string-2.0.0.tgz")
        (sha256
          (base32
            "1dc7vv5pvkdn20jk4acimb432iias4jzy62acpk35nahxgksxglf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-crypto-random-string"
         ,node-crypto-random-string-2.0.0)))
    (home-page
      "https://github.com/sindresorhus/unique-string#readme")
    (synopsis "Generate a unique random string")
    (description "Generate a unique random string")
    (license license:expat)))

(define-public node-tempy-1.0.1
  (package
    (name "node-tempy")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/tempy/-/tempy-1.0.1.tgz")
        (sha256
          (base32
            "0gkh0l99c2278q4wngfxgv05fdihyylz5jnf7nkl6dfkma85s94z"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-unique-string" ,node-unique-string-2.0.0)
        ("node-type-fest" ,node-type-fest-0.16.0)
        ("node-temp-dir" ,node-temp-dir-2.0.0)
        ("node-is-stream" ,node-is-stream-2.0.1)
        ("node-del" ,node-del-6.1.1)))
    (home-page
      "https://github.com/sindresorhus/tempy#readme")
    (synopsis
      "Get a random temporary file or directory path")
    (description
      "Get a random temporary file or directory path")
    (license license:expat)))

(define-public node-vscode-languageserver-7.0.0
  (package
    (name "node-vscode-languageserver")
    (version "7.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-languageserver/-/vscode-languageserver-7.0.0.tgz")
        (sha256
          (base32
            "12bp2f43ljs7idk37ssbxp6gijnq5hb1mkccyln1mnhxj8w2zr9p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-vscode-languageserver-protocol"
         ,node-vscode-languageserver-protocol-3.16.0)))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "Language server implementation for node")
    (description
      "Language server implementation for node")
    (license license:expat)))

(define-public node-vscode-jsonrpc-6.0.0
  (package
    (name "node-vscode-jsonrpc")
    (version "6.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-jsonrpc/-/vscode-jsonrpc-6.0.0.tgz")
        (sha256
          (base32
            "0g1v62c9dqzp9mdl9nz56ily9fj6b1n8k83xi28zd31xj4xqjiji"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "A json rpc implementation over streams")
    (description
      "A json rpc implementation over streams")
    (license license:expat)))

(define-public node-vscode-languageserver-types-3.16.0
  (package
    (name "node-vscode-languageserver-types")
    (version "3.16.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.16.0.tgz")
        (sha256
          (base32
            "0b3ajk37d3lr0w72cl03qc0ra5z15f90d1xf45fnpzd6xnjlx4zb"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "Types used by the Language server for node")
    (description
      "Types used by the Language server for node")
    (license license:expat)))

(define-public node-vscode-languageserver-protocol-3.16.0
  (package
    (name "node-vscode-languageserver-protocol")
    (version "3.16.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-languageserver-protocol/-/vscode-languageserver-protocol-3.16.0.tgz")
        (sha256
          (base32
            "19d3j2v3khl93l4ffgjgs6yb1hb3d698g056mwkh86bd2far67bf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-vscode-languageserver-types"
         ,node-vscode-languageserver-types-3.16.0)
        ("node-vscode-jsonrpc"
         ,node-vscode-jsonrpc-6.0.0)))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "VSCode Language Server Protocol implementation")
    (description
      "VSCode Language Server Protocol implementation")
    (license license:expat)))

(define-public node-vscode-languageserver-textdocument-1.0.5
  (package
    (name "node-vscode-languageserver-textdocument")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-languageserver-textdocument/-/vscode-languageserver-textdocument-1.0.5.tgz")
        (sha256
          (base32
            "06a477v74yp333afhcdvdj01qmzzwgczl4ydqid5ap5241126hjy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "A simple text document implementation for Node LSP servers")
    (description
      "A simple text document implementation for Node LSP servers")
    (license license:expat)))

(define-public node-vscode-uri-3.0.3
  (package
    (name "node-vscode-uri")
    (version "3.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-uri/-/vscode-uri-3.0.3.tgz")
        (sha256
          (base32
            "0xh3qkg5c6p4dwi0y3yjw3vzxr1nvnz6c1q5z1qyd42f1j2daqr6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/microsoft/vscode-uri#readme")
    (synopsis
      "The URI implementation that is used by VS Code and its extensions")
    (description
      "The URI implementation that is used by VS Code and its extensions")
    (license license:expat)))

(define-public node-isexe-2.0.0
  (package
    (name "node-isexe")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/isexe/-/isexe-2.0.0.tgz")
        (sha256
          (base32
            "0nc3rcqjgyb9yyqajwlzzhfcqmsb682z7zinnx9qrql8w1rfiks7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/isexe#readme")
    (synopsis
      "Minimal module to check if a file is executable.")
    (description
      "Minimal module to check if a file is executable.")
    (license license:isc)))

(define-public node-which-2.0.2
  (package
    (name "node-which")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/which/-/which-2.0.2.tgz")
        (sha256
          (base32
            "1p2fkm4lr36s85gdjxmyr6wh86dizf0iwmffxmarcxpbvmgxyfm1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-isexe" ,node-isexe-2.0.0)))
    (home-page
      "https://github.com/isaacs/node-which#readme")
    (synopsis
      "Like which(1) unix command. Find the first instance of an executable in the PATH.")
    (description
      "Like which(1) unix command. Find the first instance of an executable in the PATH.")
    (license license:isc)))

(define-public node-once-1.4.0
  (package
    (name "node-once")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/once/-/once-1.4.0.tgz")
        (sha256
          (base32
            "1kygzk36kdcfiqz01dhql2dk75rl256m2vlpigv9iikhlc5lclfg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-wrappy" ,node-wrappy-1.0.2)))
    (home-page
      "https://github.com/isaacs/once#readme")
    (synopsis "Run a function exactly one time")
    (description "Run a function exactly one time")
    (license license:isc)))

(define-public node-wrappy-1.0.2
  (package
    (name "node-wrappy")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz")
        (sha256
          (base32
            "1yzx63jf27yz0bk0m78vy4y1cqzm113d2mi9h91y3cdpj46p7wxg"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/npm/wrappy")
    (synopsis "Callback wrapping utility")
    (description "Callback wrapping utility")
    (license license:isc)))

(define-public node-typescript-language-server-0.11.1
  (package
    (name "node-typescript-language-server")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/typescript-language-server/-/typescript-language-server-0.11.1.tgz")
        (sha256
          (base32
            "0pr16c4skl8wg582h7smm5ij792rvvq4zgp67lg7mdfchzjf46vk"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-which" ,node-which-2.0.2)
        ("node-vscode-uri" ,node-vscode-uri-3.0.3)
        ("node-vscode-languageserver-textdocument"
         ,node-vscode-languageserver-textdocument-1.0.5)
        ("node-vscode-languageserver-protocol"
         ,node-vscode-languageserver-protocol-3.16.0)
        ("node-vscode-languageserver"
         ,node-vscode-languageserver-7.0.0)
        ("node-tempy" ,node-tempy-1.0.1)
        ("node-semver" ,node-semver-7.3.7)
        ("node-pkg-up" ,node-pkg-up-3.1.0)
        ("node-p-debounce" ,node-p-debounce-2.1.0)
        ("node-fs-extra" ,node-fs-extra-10.1.0)
        ("node-commander" ,node-commander-9.3.0)))
    (home-page
      "https://www.npmjs.com/package/node-typescript-language-server")
    (synopsis
      "Language Server Protocol (LSP) implementation for TypeScript using tsserver")
    (description
      "Language Server Protocol (LSP) implementation for TypeScript using tsserver")
    (license license:asl2.0)))

(define-public node-typescript-language-server node-typescript-language-server-0.11.1)

(define-public node-core-js-3.23.1
  (package
    (name "node-core-js")
    (version "3.23.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/core-js/-/core-js-3.23.1.tgz")
        (sha256
          (base32
            "1van5z0x9kfgiy3jav1s3l6k9d8wqx516l6n05zyd0w7ki3pb7ji"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/zloirock/core-js#readme")
    (synopsis "Standard library")
    (description "Standard library")
    (license license:expat)))

(define-public node-regenerator-runtime-0.13.9
  (package
    (name "node-regenerator-runtime")
    (version "0.13.9")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regenerator-runtime/-/regenerator-runtime-0.13.9.tgz")
        (sha256
          (base32
            "1f4rs0pn4g14xidzbyf8vmsr8lgm021rzzdc2y45bijx44vm8sqs"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://www.npmjs.com/package/node-regenerator-runtime")
    (synopsis
      "Runtime for Regenerator-compiled generator and async functions.")
    (description
      "Runtime for Regenerator-compiled generator and async functions.")
    (license license:expat)))

(define-public node-request-light-0.5.8
  (package
    (name "node-request-light")
    (version "0.5.8")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/request-light/-/request-light-0.5.8.tgz")
        (sha256
          (base32
            "1fbm8gr69sr9nx2bna7svpkf1jxs0kwsb900ncsn8185z944nvab"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/microsoft/node-request-light#readme")
    (synopsis
      "Lightweight request library. Promise based, with proxy support.")
    (description
      "Lightweight request library. Promise based, with proxy support.")
    (license license:expat)))

(define-public node-vscode-css-languageservice-5.4.2
  (package
    (name "node-vscode-css-languageservice")
    (version "5.4.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-css-languageservice/-/vscode-css-languageservice-5.4.2.tgz")
        (sha256
          (base32
            "0mk1gjbg3n9v9q1kx6ws51m60ij5q58acb0d9xm1aqjzvhnbg7a1"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-vscode-uri" ,node-vscode-uri-3.0.3)
        ("node-vscode-nls" ,node-vscode-nls-5.0.1)
        ("node-vscode-languageserver-types"
         ,node-vscode-languageserver-types-3.17.1)
        ("node-vscode-languageserver-textdocument"
         ,node-vscode-languageserver-textdocument-1.0.5)))
    (home-page
      "https://github.com/Microsoft/vscode-css-languageservice#readme")
    (synopsis
      "Language service for CSS, LESS and SCSS")
    (description
      "Language service for CSS, LESS and SCSS")
    (license license:expat)))

(define-public node-vscode-html-languageservice-4.2.5
  (package
    (name "node-vscode-html-languageservice")
    (version "4.2.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-html-languageservice/-/vscode-html-languageservice-4.2.5.tgz")
        (sha256
          (base32
            "06c8jz1fggg2gxbrc9fdjf8yav0gxb6x5icmixianhv15fz55p0c"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-vscode-uri" ,node-vscode-uri-3.0.3)
        ("node-vscode-nls" ,node-vscode-nls-5.0.1)
        ("node-vscode-languageserver-types"
         ,node-vscode-languageserver-types-3.17.1)
        ("node-vscode-languageserver-textdocument"
         ,node-vscode-languageserver-textdocument-1.0.5)))
    (home-page
      "https://github.com/Microsoft/vscode-html-languageservice#readme")
    (synopsis "Language service for HTML")
    (description "Language service for HTML")
    (license license:expat)))

(define-public node-jsonc-parser-3.0.0
  (package
    (name "node-jsonc-parser")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/jsonc-parser/-/jsonc-parser-3.0.0.tgz")
        (sha256
          (base32
            "1a2xq8xwydf3wcf8kn9q2c2449w9yssls8gncl8iiin6h7km6z40"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/microsoft/node-jsonc-parser#readme")
    (synopsis
      "Scanner and parser for JSON with comments.")
    (description
      "Scanner and parser for JSON with comments.")
    (license license:expat)))

(define-public node-vscode-json-languageservice-4.2.1
  (package
    (name "node-vscode-json-languageservice")
    (version "4.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-json-languageservice/-/vscode-json-languageservice-4.2.1.tgz")
        (sha256
          (base32
            "0inm0da25qkj55q017i8sr7zyix5wdxrcrcivrrqmjaypcc3152i"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-vscode-uri" ,node-vscode-uri-3.0.3)
        ("node-vscode-nls" ,node-vscode-nls-5.0.1)
        ("node-vscode-languageserver-types"
         ,node-vscode-languageserver-types-3.17.1)
        ("node-vscode-languageserver-textdocument"
         ,node-vscode-languageserver-textdocument-1.0.5)
        ("node-jsonc-parser" ,node-jsonc-parser-3.0.0)))
    (home-page
      "https://github.com/Microsoft/vscode-json-languageservice#readme")
    (synopsis "Language service for JSON")
    (description "Language service for JSON")
    (license license:expat)))

(define-public node-vscode-jsonrpc-8.0.1
  (package
    (name "node-vscode-jsonrpc")
    (version "8.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-jsonrpc/-/vscode-jsonrpc-8.0.1.tgz")
        (sha256
          (base32
            "1qzz0wr7fayf4253vdg0baqq3vxhxhfjdm00lxy17lcm6i1zcs4d"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "A json rpc implementation over streams")
    (description
      "A json rpc implementation over streams")
    (license license:expat)))

(define-public node-vscode-languageserver-types-3.17.1
  (package
    (name "node-vscode-languageserver-types")
    (version "3.17.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.17.1.tgz")
        (sha256
          (base32
            "1m9zk900px4dyz6vpi843aryw3n9sp0zbg85lpp6a828pv2ki8vp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "Types used by the Language server for node")
    (description
      "Types used by the Language server for node")
    (license license:expat)))

(define-public node-vscode-languageserver-protocol-3.17.1
  (package
    (name "node-vscode-languageserver-protocol")
    (version "3.17.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-languageserver-protocol/-/vscode-languageserver-protocol-3.17.1.tgz")
        (sha256
          (base32
            "136wqfq25g3ljvml8s4lz65svi5w8cfxrn00xl4p60wqbhscvycd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-vscode-languageserver-types"
         ,node-vscode-languageserver-types-3.17.1)
        ("node-vscode-jsonrpc"
         ,node-vscode-jsonrpc-8.0.1)))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "VSCode Language Server Protocol implementation")
    (description
      "VSCode Language Server Protocol implementation")
    (license license:expat)))

(define-public node-vscode-languageserver-8.0.1
  (package
    (name "node-vscode-languageserver")
    (version "8.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-languageserver/-/vscode-languageserver-8.0.1.tgz")
        (sha256
          (base32
            "0lc0px64klvcpzqsvkisl560z1p4pkasjdm5mdhhd9w0z1lph03g"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-vscode-languageserver-protocol"
         ,node-vscode-languageserver-protocol-3.17.1)))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "Language server implementation for node")
    (description
      "Language server implementation for node")
    (license license:expat)))

(define-public node-vscode-languageserver-textdocument-1.0.5
  (package
    (name "node-vscode-languageserver-textdocument")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-languageserver-textdocument/-/vscode-languageserver-textdocument-1.0.5.tgz")
        (sha256
          (base32
            "06a477v74yp333afhcdvdj01qmzzwgczl4ydqid5ap5241126hjy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Microsoft/vscode-languageserver-node#readme")
    (synopsis
      "A simple text document implementation for Node LSP servers")
    (description
      "A simple text document implementation for Node LSP servers")
    (license license:expat)))

(define-public node-vscode-nls-5.0.1
  (package
    (name "node-vscode-nls")
    (version "5.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-nls/-/vscode-nls-5.0.1.tgz")
        (sha256
          (base32
            "1warkarbxhi6snw18rffq8ab58rhrg42ym2njn848znfghq6xb50"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/Microsoft/vscode-nls#readme")
    (synopsis
      "NPM module to externalize and localize VSCode extensions")
    (description
      "NPM module to externalize and localize VSCode extensions")
    (license license:expat)))

(define-public node-vscode-uri-3.0.3
  (package
    (name "node-vscode-uri")
    (version "3.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-uri/-/vscode-uri-3.0.3.tgz")
        (sha256
          (base32
            "0xh3qkg5c6p4dwi0y3yjw3vzxr1nvnz6c1q5z1qyd42f1j2daqr6"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/microsoft/vscode-uri#readme")
    (synopsis
      "The URI implementation that is used by VS Code and its extensions")
    (description
      "The URI implementation that is used by VS Code and its extensions")
    (license license:expat)))

(define-public node-vscode-langservers-extracted-4.2.1
  (package
    (name "node-vscode-langservers-extracted")
    (version "4.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/vscode-langservers-extracted/-/vscode-langservers-extracted-4.2.1.tgz")
        (sha256
          (base32
            "03j0mrbfixyx34f4f0igq40r8iarpsmc8mz7gg16gy8r1fhwmvfl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build)
          (add-after 'unpack 'add-deps
                     (lambda _
                       (substitute* "package.json"
                                    (("\"dependencies\": \\{")
                                     "\"dependencies\": {
\"vscode-languageserver-protocol\": \"3.17.1\",")))))))
    (inputs
     `(("node-vscode-uri" ,node-vscode-uri-3.0.3)
        ("node-vscode-nls" ,node-vscode-nls-5.0.1)
        ("node-vscode-languageserver-protocol"
         ,node-vscode-languageserver-protocol-3.17.1)
        ("node-vscode-languageserver-textdocument"
         ,node-vscode-languageserver-textdocument-1.0.5)
        ("node-vscode-languageserver"
         ,node-vscode-languageserver-8.0.1)
        ("node-vscode-json-languageservice"
         ,node-vscode-json-languageservice-4.2.1)
        ("node-vscode-html-languageservice"
         ,node-vscode-html-languageservice-4.2.5)
        ("node-vscode-css-languageservice"
         ,node-vscode-css-languageservice-5.4.2)
        ("node-typescript" ,node-typescript-4.7.3)
        ("node-request-light" ,node-request-light-0.5.8)
        ("node-regenerator-runtime"
         ,node-regenerator-runtime-0.13.9)
        ("node-jsonc-parser" ,node-jsonc-parser-3.0.0)
        ("node-core-js" ,node-core-js-3.23.1)))
    (home-page
      "https://github.com/hrsh7th/vscode-langservers-extracted#readme")
    (synopsis
      "HTML/CSS/JSON language servers extracted from [vscode](https://github.com/Microsoft/vscode).")
    (description
      "HTML/CSS/JSON language servers extracted from [vscode](https://github.com/Microsoft/vscode).")
    (license license:expat)))

(define-public node-yarn-2.4.3
  (package
    (name "node-yarn")
    (version "2.4.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/yarn/-/yarn-2.4.3.tgz")
        (sha256
          (base32
            "14srjx7g5ddssxfx87nrrpi5l9x35s9j90bqcaaybl94nhdpp25z"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://www.npmjs.com/package/node-yarn")
    (synopsis
      "ð\x9f\x93¦ð\x9f\x90\x88 Safe, stable, reproducible projects")
    (description
      "ð\x9f\x93¦ð\x9f\x90\x88 Safe, stable, reproducible projects")
    (license #f)))

(define-public node-eslint-eslintrc-1.3.0
  (package
    (name "node-eslint-eslintrc")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@eslint/eslintrc/-/eslintrc-1.3.0.tgz")
        (sha256
          (base32
            "16x78rh16vc3rc8wcb7yyxl7vbsv87fwwn3qq4gp43l4yd4cjjj0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-strip-json-comments"
         ,node-strip-json-comments-3.1.1)
        ("node-minimatch" ,node-minimatch-3.1.2)
        ("node-js-yaml" ,node-js-yaml-4.1.0)
        ("node-import-fresh" ,node-import-fresh-3.3.0)
        ("node-ignore" ,node-ignore-5.2.0)
        ("node-globals" ,node-globals-13.15.0)
        ("node-espree" ,node-espree-9.3.2)
        ("node-debug" ,node-debug-4.3.4)
        ("node-ajv" ,node-ajv-6.12.6)))
    (home-page
      "https://github.com/eslint/eslintrc#readme")
    (synopsis
      "The legacy ESLintRC config file format for ESLint")
    (description
      "The legacy ESLintRC config file format for ESLint")
    (license license:expat)))

(define-public node-humanwhocodes-object-schema-1.2.1
  (package
    (name "node-humanwhocodes-object-schema")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@humanwhocodes/object-schema/-/object-schema-1.2.1.tgz")
        (sha256
          (base32
            "1h0p7vkkzz8q4b9ihhxbbs9zahly1pznal5np3bfh4dw94wdy67p"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/humanwhocodes/object-schema#readme")
    (synopsis "An object schema merger/validator")
    (description "An object schema merger/validator")
    (license license:bsd-3)))

(define-public node-humanwhocodes-config-array-0.9.5
  (package
    (name "node-humanwhocodes-config-array")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/@humanwhocodes/config-array/-/config-array-0.9.5.tgz")
        (sha256
          (base32
            "19g7aqn353lya7qyzkxdyqj6451ghlmw985jyy1j0wajk2f1j1qh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-minimatch" ,node-minimatch-3.1.2)
        ("node-debug" ,node-debug-4.3.4)
        ("node-humanwhocodes-object-schema"
         ,node-humanwhocodes-object-schema-1.2.1)))
    (home-page
      "https://github.com/humanwhocodes/config-array#readme")
    (synopsis "Glob-based configuration matching.")
    (description
      "Glob-based configuration matching.")
    (license license:asl2.0)))

(define-public node-fast-json-stable-stringify-2.1.0
  (package
    (name "node-fast-json-stable-stringify")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fast-json-stable-stringify/-/fast-json-stable-stringify-2.1.0.tgz")
        (sha256
          (base32
            "11qnzlan5yd2hg9nqi9hdv48bq6kwvw9pxsxir22n2iyqhighb8y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/epoberezkin/fast-json-stable-stringify")
    (synopsis
      "deterministic `JSON.stringify()` - a faster version of substack's json-stable-strigify without jsonify")
    (description
      "deterministic `JSON.stringify()` - a faster version of substack's json-stable-strigify without jsonify")
    (license license:expat)))

(define-public node-json-schema-traverse-0.4.1
  (package
    (name "node-json-schema-traverse")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/json-schema-traverse/-/json-schema-traverse-0.4.1.tgz")
        (sha256
          (base32
            "0rf0pvm62k8g81vs7n7zx080p6sfylwk52vc149jx1216vcssdgp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/epoberezkin/json-schema-traverse#readme")
    (synopsis
      "Traverse JSON Schema passing each schema object to callback")
    (description
      "Traverse JSON Schema passing each schema object to callback")
    (license license:expat)))

(define-public node-punycode-2.1.1
  (package
    (name "node-punycode")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/punycode/-/punycode-2.1.1.tgz")
        (sha256
          (base32
            "0g7z0kdxs15jrcijwbka2jajgr4b7bvpa6xmrcs0wf82pxwx1k75"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://mths.be/punycode")
    (synopsis
      "A robust Punycode converter that fully complies to RFC 3492 and RFC 5891, and works on nearly all JavaScript platforms.")
    (description
      "A robust Punycode converter that fully complies to RFC 3492 and RFC 5891, and works on nearly all JavaScript platforms.")
    (license license:expat)))

(define-public node-uri-js-4.4.1
  (package
    (name "node-uri-js")
    (version "4.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/uri-js/-/uri-js-4.4.1.tgz")
        (sha256
          (base32
            "0bcdxkngap84iv7hpfa4r18i3a3allxfh6dmcqzafgg8mx9dw4jn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-punycode" ,node-punycode-2.1.1)))
    (home-page "https://github.com/garycourt/uri-js")
    (synopsis
      "An RFC 3986/3987 compliant, scheme extendable URI/IRI parsing/validating/resolving library for JavaScript.")
    (description
      "An RFC 3986/3987 compliant, scheme extendable URI/IRI parsing/validating/resolving library for JavaScript.")
    (license #f)))

(define-public node-ajv-6.12.6
  (package
    (name "node-ajv")
    (version "6.12.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ajv/-/ajv-6.12.6.tgz")
        (sha256
          (base32
            "0jhk2dnzrk188p3micnkh7126lhdbkj9iip0pywhky6vh1dk8xcr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-uri-js" ,node-uri-js-4.4.1)
        ("node-json-schema-traverse"
         ,node-json-schema-traverse-0.4.1)
        ("node-fast-json-stable-stringify"
         ,node-fast-json-stable-stringify-2.1.0)
        ("node-fast-deep-equal"
         ,node-fast-deep-equal-3.1.3)))
    (home-page
      "https://github.com/ajv-validator/ajv")
    (synopsis "Another JSON Schema Validator")
    (description "Another JSON Schema Validator")
    (license license:expat)))

(define-public node-color-name-1.1.4
  (package
    (name "node-color-name")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/color-name/-/color-name-1.1.4.tgz")
        (sha256
          (base32
            "020p7x7k8rlph38lhsqpqvkx0b70lzlmk6mgal9r9sz8c527qysh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/colorjs/color-name")
    (synopsis "A list of color names and its values")
    (description
      "A list of color names and its values")
    (license license:expat)))

(define-public node-color-convert-2.0.1
  (package
    (name "node-color-convert")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/color-convert/-/color-convert-2.0.1.tgz")
        (sha256
          (base32
            "1qbw9rwfzcp7y0cpa8gmwlj7ccycf9pwn15zvf2s06f070ss83wj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-color-name" ,node-color-name-1.1.4)))
    (home-page
      "https://github.com/Qix-/color-convert#readme")
    (synopsis "Plain color conversion functions")
    (description "Plain color conversion functions")
    (license license:expat)))

(define-public node-ansi-styles-4.3.0
  (package
    (name "node-ansi-styles")
    (version "4.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ansi-styles/-/ansi-styles-4.3.0.tgz")
        (sha256
          (base32
            "0zwqsx67hr7m4a8dpd0jzkp2rjm5v7938x4rhcqh7djsv139llrc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-color-convert" ,node-color-convert-2.0.1)))
    (home-page
      "https://github.com/chalk/ansi-styles#readme")
    (synopsis
      "ANSI escape codes for styling strings in the terminal")
    (description
      "ANSI escape codes for styling strings in the terminal")
    (license license:expat)))

(define-public node-has-flag-4.0.0
  (package
    (name "node-has-flag")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/has-flag/-/has-flag-4.0.0.tgz")
        (sha256
          (base32
            "1cdmvliwz8h02nwg0ipli0ydd1l82sz9s1m7bj5bn9yr24afp9vp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/has-flag#readme")
    (synopsis "Check if argv has a specific flag")
    (description "Check if argv has a specific flag")
    (license license:expat)))

(define-public node-supports-color-7.2.0
  (package
    (name "node-supports-color")
    (version "7.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/supports-color/-/supports-color-7.2.0.tgz")
        (sha256
          (base32
            "0jjyglzdzscmhgidn43zc218q5jf9h03hmaaq9h4wqil2vywlspi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-has-flag" ,node-has-flag-4.0.0)))
    (home-page
      "https://github.com/chalk/supports-color#readme")
    (synopsis
      "Detect whether a terminal supports color")
    (description
      "Detect whether a terminal supports color")
    (license license:expat)))

(define-public node-chalk-4.1.2
  (package
    (name "node-chalk")
    (version "4.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/chalk/-/chalk-4.1.2.tgz")
        (sha256
          (base32
            "02prgl8d52k2vgxnssx06ha2sjm2vp6v6s6kqgkar1ryllx68k78"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-supports-color"
         ,node-supports-color-7.2.0)
        ("node-ansi-styles" ,node-ansi-styles-4.3.0)))
    (home-page
      "https://github.com/chalk/chalk#readme")
    (synopsis "Terminal string styling done right")
    (description
      "Terminal string styling done right")
    (license license:expat)))

(define-public node-path-key-3.1.1
  (package
    (name "node-path-key")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-key/-/path-key-3.1.1.tgz")
        (sha256
          (base32
            "14kvp849wnkg6f3dqgmcb73nnb5k6b3gxf65sgf0x0qlp6n9k2ab"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/path-key#readme")
    (synopsis
      "Get the PATH environment variable key cross-platform")
    (description
      "Get the PATH environment variable key cross-platform")
    (license license:expat)))

(define-public node-shebang-regex-3.0.0
  (package
    (name "node-shebang-regex")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/shebang-regex/-/shebang-regex-3.0.0.tgz")
        (sha256
          (base32
            "13wmb23w5srjpn9xx1c85yk5jbc5z9ypg0iz33h6nv5jdnmapnzy"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/shebang-regex#readme")
    (synopsis
      "Regular expression for matching a shebang line")
    (description
      "Regular expression for matching a shebang line")
    (license license:expat)))

(define-public node-shebang-command-2.0.0
  (package
    (name "node-shebang-command")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/shebang-command/-/shebang-command-2.0.0.tgz")
        (sha256
          (base32
            "0vjmdpwcz23glkhlmxny8hc3x01zyr6hwf4qb3grq7m532ysbjws"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-shebang-regex" ,node-shebang-regex-3.0.0)))
    (home-page
      "https://github.com/kevva/shebang-command#readme")
    (synopsis "Get the command from a shebang")
    (description "Get the command from a shebang")
    (license license:expat)))

(define-public node-isexe-2.0.0
  (package
    (name "node-isexe")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/isexe/-/isexe-2.0.0.tgz")
        (sha256
          (base32
            "0nc3rcqjgyb9yyqajwlzzhfcqmsb682z7zinnx9qrql8w1rfiks7"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/isexe#readme")
    (synopsis
      "Minimal module to check if a file is executable.")
    (description
      "Minimal module to check if a file is executable.")
    (license license:isc)))

(define-public node-cross-spawn-7.0.3
  (package
    (name "node-cross-spawn")
    (version "7.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/cross-spawn/-/cross-spawn-7.0.3.tgz")
        (sha256
          (base32
            "01bj9b7khakchhfl8dfbss3xm0w677h2hk1bzbpy65q214a8ii8i"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-which" ,node-which-2.0.2)
        ("node-shebang-command"
         ,node-shebang-command-2.0.0)
        ("node-path-key" ,node-path-key-3.1.1)))
    (home-page
      "https://github.com/moxystudio/node-cross-spawn")
    (synopsis
      "Cross platform child_process#spawn and child_process#spawnSync")
    (description
      "Cross platform child_process#spawn and child_process#spawnSync")
    (license license:expat)))

(define-public node-ms-2.1.2
  (package
    (name "node-ms")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ms/-/ms-2.1.2.tgz")
        (sha256
          (base32
            "0j7vrqxzg2fxip3q0cws360wk3cz2nprr8zkragipziz1piscmqi"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/zeit/ms#readme")
    (synopsis "Tiny millisecond conversion utility")
    (description
      "Tiny millisecond conversion utility")
    (license license:expat)))

(define-public node-debug-4.3.4
  (package
    (name "node-debug")
    (version "4.3.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/debug/-/debug-4.3.4.tgz")
        (sha256
          (base32
            "1kwbyb5m63bz8a2bvhy4gsnsma6ks5wa4w5qya6qb9ip5sdjr4h4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-ms" ,node-ms-2.1.2)))
    (home-page
      "https://github.com/debug-js/debug#readme")
    (synopsis
      "Lightweight debugging utility for Node.js and the browser")
    (description
      "Lightweight debugging utility for Node.js and the browser")
    (license license:expat)))

(define-public node-doctrine-3.0.0
  (package
    (name "node-doctrine")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/doctrine/-/doctrine-3.0.0.tgz")
        (sha256
          (base32
            "080xaz4gknw5yklixj4hhrszzm53fq8bzng48ik6vhpr8n4dy10h"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-esutils" ,node-esutils-2.0.3)))
    (home-page "https://github.com/eslint/doctrine")
    (synopsis "JSDoc parser")
    (description "JSDoc parser")
    (license license:asl2.0)))

(define-public node-escape-string-regexp-4.0.0
  (package
    (name "node-escape-string-regexp")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-4.0.0.tgz")
        (sha256
          (base32
            "06xi6f77ybg7cl673bawycpp6984h2ridzy2hl2kqylqlr6s2i2b"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/escape-string-regexp#readme")
    (synopsis "Escape RegExp special characters")
    (description "Escape RegExp special characters")
    (license license:expat)))

(define-public node-esrecurse-4.3.0
  (package
    (name "node-esrecurse")
    (version "4.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/esrecurse/-/esrecurse-4.3.0.tgz")
        (sha256
          (base32
            "0q9vg6dmzdcy4mmm6dnmz1d9dfrm1gvi32n8f2slfsr9sxq97kry"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-estraverse" ,node-estraverse-5.3.0)))
    (home-page
      "https://github.com/estools/esrecurse")
    (synopsis "ECMAScript AST recursive visitor")
    (description "ECMAScript AST recursive visitor")
    (license #f)))

(define-public node-eslint-scope-7.1.1
  (package
    (name "node-eslint-scope")
    (version "7.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/eslint-scope/-/eslint-scope-7.1.1.tgz")
        (sha256
          (base32
            "0jh7jsq8hzky7p48a0241ma3cxp4hsgxw5bfv2ciyiz3b6qfdg56"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-estraverse" ,node-estraverse-5.3.0)
        ("node-esrecurse" ,node-esrecurse-4.3.0)))
    (home-page
      "http://github.com/eslint/eslint-scope")
    (synopsis "ECMAScript scope analyzer for ESLint")
    (description
      "ECMAScript scope analyzer for ESLint")
    (license #f)))

(define-public node-eslint-visitor-keys-2.1.0
  (package
    (name "node-eslint-visitor-keys")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/eslint-visitor-keys/-/eslint-visitor-keys-2.1.0.tgz")
        (sha256
          (base32
            "065rhr6xhpqqzs3yx56v759hvm27d9nifx14qakr6p6n4jyvnh70"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/eslint/eslint-visitor-keys#readme")
    (synopsis
      "Constants and utilities about visitor keys to traverse AST.")
    (description
      "Constants and utilities about visitor keys to traverse AST.")
    (license license:asl2.0)))

(define-public node-eslint-utils-3.0.0
  (package
    (name "node-eslint-utils")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/eslint-utils/-/eslint-utils-3.0.0.tgz")
        (sha256
          (base32
            "068rs2rhk7i9xdja6ix21jsllani8w6hi1zmq3fvbdz10mc1jf4h"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-eslint-visitor-keys"
         ,node-eslint-visitor-keys-2.1.0)))
    (home-page
      "https://github.com/mysticatea/eslint-utils#readme")
    (synopsis "Utilities for ESLint plugins.")
    (description "Utilities for ESLint plugins.")
    (license license:expat)))

(define-public node-acorn-8.7.1
  (package
    (name "node-acorn")
    (version "8.7.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/acorn/-/acorn-8.7.1.tgz")
        (sha256
          (base32
            "1ic4hy4mhgk7lfcf6d1jwhn6z1fh9zmjry3sj60v7x1mb15566gm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/acornjs/acorn")
    (synopsis "ECMAScript parser")
    (description "ECMAScript parser")
    (license license:expat)))

(define-public node-acorn-jsx-5.3.2
  (package
    (name "node-acorn-jsx")
    (version "5.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/acorn-jsx/-/acorn-jsx-5.3.2.tgz")
        (sha256
          (base32
            "1k2qj4av0nmrqa5xcn0hrjv9r5kv8x83238fsxb6lff581gb0ryr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/acornjs/acorn-jsx")
    (synopsis "Modern, fast React.js JSX parser")
    (description "Modern, fast React.js JSX parser")
    (license license:expat)))

(define-public node-eslint-visitor-keys-3.3.0
  (package
    (name "node-eslint-visitor-keys")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/eslint-visitor-keys/-/eslint-visitor-keys-3.3.0.tgz")
        (sha256
          (base32
            "0p8hi4qjlrf1yg5j8a18ckq9vzhhvd1j4gbr16w6zyza9mpyxpz8"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/eslint/eslint-visitor-keys#readme")
    (synopsis
      "Constants and utilities about visitor keys to traverse AST.")
    (description
      "Constants and utilities about visitor keys to traverse AST.")
    (license license:asl2.0)))

(define-public node-espree-9.3.2
  (package
    (name "node-espree")
    (version "9.3.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/espree/-/espree-9.3.2.tgz")
        (sha256
          (base32
            "0zi6bs541ljsi9j8ljdqbpi0f3phfmr0hij9ynl16709cry6nx0r"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-eslint-visitor-keys"
         ,node-eslint-visitor-keys-3.3.0)
        ("node-acorn-jsx" ,node-acorn-jsx-5.3.2)
        ("node-acorn" ,node-acorn-8.7.1)))
    (home-page "https://github.com/eslint/espree")
    (synopsis
      "An Esprima-compatible JavaScript parser built on Acorn")
    (description
      "An Esprima-compatible JavaScript parser built on Acorn")
    (license #f)))

(define-public node-estraverse-5.3.0
  (package
    (name "node-estraverse")
    (version "5.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/estraverse/-/estraverse-5.3.0.tgz")
        (sha256
          (base32
            "19pxf86qwp2xl1i8k5w6q81aaldmspmgys0ksnla91c0bgd4b39y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/estools/estraverse")
    (synopsis
      "ECMAScript JS AST traversal functions")
    (description
      "ECMAScript JS AST traversal functions")
    (license #f)))

(define-public node-esquery-1.4.0
  (package
    (name "node-esquery")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/esquery/-/esquery-1.4.0.tgz")
        (sha256
          (base32
            "02gsrq29rclfdg2nbic4kdj8frbh9vhxc1ysyawyd00lf8fdsnkf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-estraverse" ,node-estraverse-5.3.0)))
    (home-page "https://github.com/estools/esquery/")
    (synopsis
      "A query library for ECMAScript AST using a CSS selector like query language.")
    (description
      "A query library for ECMAScript AST using a CSS selector like query language.")
    (license license:bsd-3)))

(define-public node-esutils-2.0.3
  (package
    (name "node-esutils")
    (version "2.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/esutils/-/esutils-2.0.3.tgz")
        (sha256
          (base32
            "03v4y32k50mbxwv70prr7ghwg59vd5gyxsdsbdikqnj919rvvbf5"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://github.com/estools/esutils")
    (synopsis
      "utility box for ECMAScript language tools")
    (description
      "utility box for ECMAScript language tools")
    (license #f)))

(define-public node-fast-deep-equal-3.1.3
  (package
    (name "node-fast-deep-equal")
    (version "3.1.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fast-deep-equal/-/fast-deep-equal-3.1.3.tgz")
        (sha256
          (base32
            "13vvwib6za4zh7054n3fg86y127ig3jb0djqz31qsqr71yca06dh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/epoberezkin/fast-deep-equal#readme")
    (synopsis "Fast deep equal")
    (description "Fast deep equal")
    (license license:expat)))

(define-public node-flatted-3.2.5
  (package
    (name "node-flatted")
    (version "3.2.5")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/flatted/-/flatted-3.2.5.tgz")
        (sha256
          (base32
            "0jrdiyjpcni3qpf0gfzh8nhk1k9j9igz5vxafzvs99s6j6n561i2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/WebReflection/flatted#readme")
    (synopsis
      "A super light and fast circular JSON parser.")
    (description
      "A super light and fast circular JSON parser.")
    (license license:isc)))

(define-public node-fs-realpath-1.0.0
  (package
    (name "node-fs-realpath")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz")
        (sha256
          (base32
            "174g5vay9jnd7h5q8hfdw6dnmwl1gdpn4a8sz0ysanhj2f3wp04y"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/fs.realpath#readme")
    (synopsis
      "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
    (description
      "Use node's fs.realpath, but fall back to the JS implementation if the native one fails")
    (license license:isc)))

(define-public node-inherits-2.0.4
  (package
    (name "node-inherits")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz")
        (sha256
          (base32
            "1bxg4igfni2hymabg8bkw86wd3qhhzhsswran47sridk3dnbqkfr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/isaacs/inherits#readme")
    (synopsis
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (description
      "Browser-friendly inheritance fully compatible with standard node.js inherits()")
    (license license:isc)))

(define-public node-path-is-absolute-1.0.1
  (package
    (name "node-path-is-absolute")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz")
        (sha256
          (base32
            "0p7p04xxd8q495qhxmxydyjgzcf762dp1hp2wha2b52n3agp0vbf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/path-is-absolute#readme")
    (synopsis
      "Node.js 0.12 path.isAbsolute() ponyfill")
    (description
      "Node.js 0.12 path.isAbsolute() ponyfill")
    (license license:expat)))

(define-public node-glob-7.2.3
  (package
    (name "node-glob")
    (version "7.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/glob/-/glob-7.2.3.tgz")
        (sha256
          (base32
            "10a336nxv867xkjs3ipgbharwdzp5lnz7wr8viawn1lc66qqx8zh"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-path-is-absolute"
         ,node-path-is-absolute-1.0.1)
        ("node-once" ,node-once-1.4.0)
        ("node-minimatch" ,node-minimatch-3.1.2)
        ("node-inherits" ,node-inherits-2.0.4)
        ("node-inflight" ,node-inflight-1.0.6)
        ("node-fs-realpath" ,node-fs-realpath-1.0.0)))
    (home-page
      "https://github.com/isaacs/node-glob#readme")
    (synopsis "a little globber")
    (description "a little globber")
    (license license:isc)))

(define-public node-rimraf-3.0.2
  (package
    (name "node-rimraf")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/rimraf/-/rimraf-3.0.2.tgz")
        (sha256
          (base32
            "0lkzjyxjij6ssh5h2l3ncp0zx00ylzhww766dq2vf1s7v07w4xjq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-glob" ,node-glob-7.2.3)))
    (home-page
      "https://github.com/isaacs/rimraf#readme")
    (synopsis
      "A deep deletion module for node (like `rm -rf`)")
    (description
      "A deep deletion module for node (like `rm -rf`)")
    (license license:isc)))

(define-public node-flat-cache-3.0.4
  (package
    (name "node-flat-cache")
    (version "3.0.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/flat-cache/-/flat-cache-3.0.4.tgz")
        (sha256
          (base32
            "0bhdl02yqa9ckh3rjw96ggrply2jr5rqnklnk07kq4lk4g1lhlfm"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-rimraf" ,node-rimraf-3.0.2)
        ("node-flatted" ,node-flatted-3.2.5)))
    (home-page
      "https://github.com/royriojas/flat-cache#readme")
    (synopsis
      "A stupidly simple key/value storage using files to persist some data")
    (description
      "A stupidly simple key/value storage using files to persist some data")
    (license license:expat)))

(define-public node-file-entry-cache-6.0.1
  (package
    (name "node-file-entry-cache")
    (version "6.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/file-entry-cache/-/file-entry-cache-6.0.1.tgz")
        (sha256
          (base32
            "11f1nrv59fzyandaggvfqw9874x2mqv81cmyxnkqf6s2rvmqpk8q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-flat-cache" ,node-flat-cache-3.0.4)))
    (home-page
      "https://github.com/royriojas/file-entry-cache#readme")
    (synopsis
      "Super simple cache for file metadata, useful for process that work o a given series of files and that only need to repeat the job on the changed ones since the previous run of the process")
    (description
      "Super simple cache for file metadata, useful for process that work o a given series of files and that only need to repeat the job on the changed ones since the previous run of the process")
    (license license:expat)))

(define-public node-functional-red-black-tree-1.0.1
  (package
    (name "node-functional-red-black-tree")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/functional-red-black-tree/-/functional-red-black-tree-1.0.1.tgz")
        (sha256
          (base32
            "03azfk7scbq2sr1h9ka3bn5gksp8f524ps6k66iq13vpc2kxyhnr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mikolalysenko/functional-red-black-tree")
    (synopsis
      "A fully persistent balanced binary search tree")
    (description
      "A fully persistent balanced binary search tree")
    (license license:expat)))

(define-public node-glob-parent-6.0.2
  (package
    (name "node-glob-parent")
    (version "6.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/glob-parent/-/glob-parent-6.0.2.tgz")
        (sha256
          (base32
            "0rjhim72pkv230y79xdahhpcnhy4y8fxr7dhr8sf47xyyd1bjnrl"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs `(("node-is-glob" ,node-is-glob-4.0.3)))
    (home-page
      "https://github.com/gulpjs/glob-parent#readme")
    (synopsis
      "Extract the non-magic parent path from a glob string.")
    (description
      "Extract the non-magic parent path from a glob string.")
    (license license:isc)))

(define-public node-type-fest-0.20.2
  (package
    (name "node-type-fest")
    (version "0.20.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/type-fest/-/type-fest-0.20.2.tgz")
        (sha256
          (base32
            "18k5jkkj8s629np26jzbdfbh7y6ywy22459ha12g2yi80pyd9mdr"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/type-fest#readme")
    (synopsis
      "A collection of essential TypeScript types")
    (description
      "A collection of essential TypeScript types")
    (license #f)))

(define-public node-globals-13.15.0
  (package
    (name "node-globals")
    (version "13.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/globals/-/globals-13.15.0.tgz")
        (sha256
          (base32
            "1wrvnhqr1rfbrxgpc7f3qncsbrnhrgyh6203g47b07gll43jq40q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-type-fest" ,node-type-fest-0.20.2)))
    (home-page
      "https://github.com/sindresorhus/globals#readme")
    (synopsis
      "Global identifiers from different JavaScript environments")
    (description
      "Global identifiers from different JavaScript environments")
    (license license:expat)))

(define-public node-ignore-5.2.0
  (package
    (name "node-ignore")
    (version "5.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ignore/-/ignore-5.2.0.tgz")
        (sha256
          (base32
            "1kaia5s9yhayx63kgqdrnk38l2cyvyp6al31g4qkhhcplmqsfkfn"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/kaelzhang/node-ignore#readme")
    (synopsis
      "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
    (description
      "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
    (license license:expat)))

(define-public node-callsites-3.1.0
  (package
    (name "node-callsites")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/callsites/-/callsites-3.1.0.tgz")
        (sha256
          (base32
            "17f8wf2bxv2s4k36ld1x4y2rbkh9a8vsmbhwab470vmz6ayl40hp"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/callsites#readme")
    (synopsis
      "Get callsites from the V8 stack trace API")
    (description
      "Get callsites from the V8 stack trace API")
    (license license:expat)))

(define-public node-parent-module-1.0.1
  (package
    (name "node-parent-module")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/parent-module/-/parent-module-1.0.1.tgz")
        (sha256
          (base32
            "01z4k8a3y21gqmpcwyf7gq9v8v4k2y5180f5g7qgswd4gq986zk0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-callsites" ,node-callsites-3.1.0)))
    (home-page
      "https://github.com/sindresorhus/parent-module#readme")
    (synopsis "Get the path of the parent module")
    (description "Get the path of the parent module")
    (license license:expat)))

(define-public node-resolve-from-4.0.0
  (package
    (name "node-resolve-from")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/resolve-from/-/resolve-from-4.0.0.tgz")
        (sha256
          (base32
            "1p11030pz8qdm9x2d9q0qi2p329447i2bb7a5j7hbsxxqbs2hhi4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/resolve-from#readme")
    (synopsis
      "Resolve the path of a module like `require.resolve()` but from a given path")
    (description
      "Resolve the path of a module like `require.resolve()` but from a given path")
    (license license:expat)))

(define-public node-import-fresh-3.3.0
  (package
    (name "node-import-fresh")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/import-fresh/-/import-fresh-3.3.0.tgz")
        (sha256
          (base32
            "1chk0qimpnkrd2bn072ywnlhvy69cjyndgbij59m2b9jf4rxp945"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-resolve-from" ,node-resolve-from-4.0.0)
        ("node-parent-module" ,node-parent-module-1.0.1)))
    (home-page
      "https://github.com/sindresorhus/import-fresh#readme")
    (synopsis
      "Import a module while bypassing the cache")
    (description
      "Import a module while bypassing the cache")
    (license license:expat)))

(define-public node-imurmurhash-0.1.4
  (package
    (name "node-imurmurhash")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/imurmurhash/-/imurmurhash-0.1.4.tgz")
        (sha256
          (base32
            "0q6bf91h2g5dhvcdss74sjvp5irimd97hp73jb8p2wvajqqs08xc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jensyt/imurmurhash-js")
    (synopsis
      "An incremental implementation of MurmurHash3")
    (description
      "An incremental implementation of MurmurHash3")
    (license license:expat)))

(define-public node-is-extglob-2.1.1
  (package
    (name "node-is-extglob")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-extglob/-/is-extglob-2.1.1.tgz")
        (sha256
          (base32
            "06dwa2xzjx6az40wlvwj11vican2w46710b9170jzmka2j344pcc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/is-extglob")
    (synopsis
      "Returns true if a string has an extglob.")
    (description
      "Returns true if a string has an extglob.")
    (license license:expat)))

(define-public node-is-glob-4.0.3
  (package
    (name "node-is-glob")
    (version "4.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/is-glob/-/is-glob-4.0.3.tgz")
        (sha256
          (base32
            "1imyq6pjl716cjc1ypmmnn0574rh28av3pq50mpqzd9v37xm7r1z"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-is-extglob" ,node-is-extglob-2.1.1)))
    (home-page
      "https://github.com/micromatch/is-glob")
    (synopsis
      "Returns `true` if the given string looks like a glob pattern or an extglob pattern. This makes it easy to create code that only uses external modules like node-glob when necessary, resulting in much faster code execution and initialization time, and a bet")
    (description
      "Returns `true` if the given string looks like a glob pattern or an extglob pattern. This makes it easy to create code that only uses external modules like node-glob when necessary, resulting in much faster code execution and initialization time, and a bet")
    (license license:expat)))

(define-public node-argparse-2.0.1
  (package
    (name "node-argparse")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/argparse/-/argparse-2.0.1.tgz")
        (sha256
          (base32
            "133jjyhcr25rf4vy7bca7x06dfmsyy819s1kbbyfc5c2zi3ki417"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/nodeca/argparse#readme")
    (synopsis
      "CLI arguments parser. Native port of python's argparse.")
    (description
      "CLI arguments parser. Native port of python's argparse.")
    (license #f)))

(define-public node-js-yaml-4.1.0
  (package
    (name "node-js-yaml")
    (version "4.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/js-yaml/-/js-yaml-4.1.0.tgz")
        (sha256
          (base32
            "1jpj5j4aiyh9sbcw7y8jjkwkyc6qmwrffw7a4qfb48ngb4jk7bhd"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-argparse" ,node-argparse-2.0.1)))
    (home-page
      "https://github.com/nodeca/js-yaml#readme")
    (synopsis "YAML 1.2 parser and serializer")
    (description "YAML 1.2 parser and serializer")
    (license license:expat)))

(define-public node-json-stable-stringify-without-jsonify-1.0.1
  (package
    (name "node-json-stable-stringify-without-jsonify")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/json-stable-stringify-without-jsonify/-/json-stable-stringify-without-jsonify-1.0.1.tgz")
        (sha256
          (base32
            "1smffz68mhcb5w6ckflwfh2n91nbi6z0vk8rhbxvk3lbhnn61gax"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/samn/json-stable-stringify")
    (synopsis
      "deterministic JSON.stringify() with custom sorting to get deterministic hashes from stringified results, with no public domain dependencies")
    (description
      "deterministic JSON.stringify() with custom sorting to get deterministic hashes from stringified results, with no public domain dependencies")
    (license license:expat)))

(define-public node-lodash-merge-4.6.2
  (package
    (name "node-lodash-merge")
    (version "4.6.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/lodash.merge/-/lodash.merge-4.6.2.tgz")
        (sha256
          (base32
            "15pgb54k3jl0qq1x5qdcvbm456y254gzyln667p1ajnna2ljfkm3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "https://lodash.com/")
    (synopsis
      "The Lodash method `_.merge` exported as a module.")
    (description
      "The Lodash method `_.merge` exported as a module.")
    (license license:expat)))

(define-public node-balanced-match-1.0.2
  (package
    (name "node-balanced-match")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.2.tgz")
        (sha256
          (base32
            "1hdwrr7qqb37plj7962xbwjx1jvjz7ahl7iqrwh82yhcvnmzfm6q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/juliangruber/balanced-match")
    (synopsis
      "Match balanced character pairs, like \"{\" and \"}\"")
    (description
      "Match balanced character pairs, like \"{\" and \"}\"")
    (license license:expat)))

(define-public node-concat-map-0.0.1
  (package
    (name "node-concat-map")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz")
        (sha256
          (base32
            "0qa2zqn9rrr2fqdki44s4s2dk2d8307i4556kv25h06g43b2v41m"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/substack/node-concat-map")
    (synopsis "concatenative mapdashery")
    (description "concatenative mapdashery")
    (license license:expat)))

(define-public node-natural-compare-1.4.0
  (package
    (name "node-natural-compare")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/natural-compare/-/natural-compare-1.4.0.tgz")
        (sha256
          (base32
            "1vkrwxgc7w81923739lrnqklmpnin52qmhd868vgfr10s7n6jmay"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/litejs/natural-compare-lite#readme")
    (synopsis
      "Compare strings containing a mix of letters and numbers in the way a human being would in sort order.")
    (description
      "Compare strings containing a mix of letters and numbers in the way a human being would in sort order.")
    (license license:expat)))

(define-public node-deep-is-0.1.4
  (package
    (name "node-deep-is")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/deep-is/-/deep-is-0.1.4.tgz")
        (sha256
          (base32
            "0g5z206z33f41cdh220vf7kpj61cfcnmlnrcrk1ffs5zqs1sl0qc"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/thlorenz/deep-is#readme")
    (synopsis
      "node's assert.deepEqual algorithm except for NaN being equal to NaN")
    (description
      "node's assert.deepEqual algorithm except for NaN being equal to NaN")
    (license license:expat)))

(define-public node-word-wrap-1.2.3
  (package
    (name "node-word-wrap")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/word-wrap/-/word-wrap-1.2.3.tgz")
        (sha256
          (base32
            "1ngw3nglmfh9a90b4ckay43yw96h61mbxmhm5g1qvk1j6h1dmyv4"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/jonschlinkert/word-wrap")
    (synopsis "Wrap words to a specified length.")
    (description "Wrap words to a specified length.")
    (license license:expat)))

(define-public node-prelude-ls-1.2.1
  (package
    (name "node-prelude-ls")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.2.1.tgz")
        (sha256
          (base32
            "14fjp2mzgn4a705pkwggdy3dib854q0rvlmylpyjz18zgwwzrs0l"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page "http://preludels.com")
    (synopsis
      "prelude.ls is a functionally oriented utility library. It is powerful and flexible. Almost all of its functions are curried. It is written in, and is the recommended base library for, LiveScript.")
    (description
      "prelude.ls is a functionally oriented utility library. It is powerful and flexible. Almost all of its functions are curried. It is written in, and is the recommended base library for, LiveScript.")
    (license license:expat)))

(define-public node-type-check-0.4.0
  (package
    (name "node-type-check")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/type-check/-/type-check-0.4.0.tgz")
        (sha256
          (base32
            "13iza44xzxi2sfdz411wdfprl9wmlzgvlv60i8sr49lfqh6d2dc0"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-prelude-ls" ,node-prelude-ls-1.2.1)))
    (home-page "https://github.com/gkz/type-check")
    (synopsis
      "type-check allows you to check the types of JavaScript values at runtime with a Haskell like type syntax.")
    (description
      "type-check allows you to check the types of JavaScript values at runtime with a Haskell like type syntax.")
    (license license:expat)))

(define-public node-levn-0.4.1
  (package
    (name "node-levn")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/levn/-/levn-0.4.1.tgz")
        (sha256
          (base32
            "1lb7409xvyj4sz36pslhrcz283yycc1fxzf0k4c554qgvza5fi0z"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-type-check" ,node-type-check-0.4.0)
        ("node-prelude-ls" ,node-prelude-ls-1.2.1)))
    (home-page "https://github.com/gkz/levn")
    (synopsis
      "Light ECMAScript (JavaScript) Value Notation - human written, concise, typed, flexible")
    (description
      "Light ECMAScript (JavaScript) Value Notation - human written, concise, typed, flexible")
    (license license:expat)))

(define-public node-fast-levenshtein-2.0.6
  (package
    (name "node-fast-levenshtein")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-2.0.6.tgz")
        (sha256
          (base32
            "0g5zgdlp38dli94qbbm8vhvmj90fh48sxpggfn2083wbdcq50jxv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/hiddentao/fast-levenshtein#readme")
    (synopsis
      "Efficient implementation of Levenshtein algorithm  with locale-specific collator support.")
    (description
      "Efficient implementation of Levenshtein algorithm  with locale-specific collator support.")
    (license license:expat)))

(define-public node-optionator-0.9.1
  (package
    (name "node-optionator")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/optionator/-/optionator-0.9.1.tgz")
        (sha256
          (base32
            "0nvw6pscr69hpi03451a4g8795ca0m68hf4aswh0pby0bq6j1rn3"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-fast-levenshtein"
         ,node-fast-levenshtein-2.0.6)
        ("node-levn" ,node-levn-0.4.1)
        ("node-type-check" ,node-type-check-0.4.0)
        ("node-word-wrap" ,node-word-wrap-1.2.3)
        ("node-deep-is" ,node-deep-is-0.1.4)
        ("node-prelude-ls" ,node-prelude-ls-1.2.1)))
    (home-page "https://github.com/gkz/optionator")
    (synopsis "option parsing and help generation")
    (description
      "option parsing and help generation")
    (license license:expat)))

(define-public node-regexpp-3.2.0
  (package
    (name "node-regexpp")
    (version "3.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/regexpp/-/regexpp-3.2.0.tgz")
        (sha256
          (base32
            "0zhahkmwgj2jl9k8ybqfbv59vhqyzzivjd7qwl3zqw7pmbm7fk7s"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/mysticatea/regexpp#readme")
    (synopsis
      "Regular expression parser for ECMAScript.")
    (description
      "Regular expression parser for ECMAScript.")
    (license license:expat)))

(define-public node-ansi-regex-5.0.1
  (package
    (name "node-ansi-regex")
    (version "5.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/ansi-regex/-/ansi-regex-5.0.1.tgz")
        (sha256
          (base32
            "1ng0r2k4mcz7b2bfr6g1dschnxm0vifaslsvv2smv06smb6ss3hf"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/chalk/ansi-regex#readme")
    (synopsis
      "Regular expression for matching ANSI escape codes")
    (description
      "Regular expression for matching ANSI escape codes")
    (license license:expat)))

(define-public node-strip-ansi-6.0.1
  (package
    (name "node-strip-ansi")
    (version "6.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/strip-ansi/-/strip-ansi-6.0.1.tgz")
        (sha256
          (base32
            "1jh81jj6cn1lli1c7m6xi0ynra9zdghb1g63v1nib7zlpz87bnwv"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-ansi-regex" ,node-ansi-regex-5.0.1)))
    (home-page
      "https://github.com/chalk/strip-ansi#readme")
    (synopsis
      "Strip ANSI escape codes from a string")
    (description
      "Strip ANSI escape codes from a string")
    (license license:expat)))

(define-public node-strip-json-comments-3.1.1
  (package
    (name "node-strip-json-comments")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/strip-json-comments/-/strip-json-comments-3.1.1.tgz")
        (sha256
          (base32
            "1vffnq04fk4spnmvrzq3wg41f88lcb9s1z0gqvshqiqw3imzw4q2"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/sindresorhus/strip-json-comments#readme")
    (synopsis
      "Strip comments from JSON. Lets you use comments in your JSON files!")
    (description
      "Strip comments from JSON. Lets you use comments in your JSON files!")
    (license license:expat)))

(define-public node-text-table-0.2.0
  (package
    (name "node-text-table")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/text-table/-/text-table-0.2.0.tgz")
        (sha256
          (base32
            "0wn7i0zl0yi4j9hccnfiin9p77w5vlqlk2812ycp60q69iqgd0yq"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/substack/text-table")
    (synopsis
      "borderless text tables with alignment")
    (description
      "borderless text tables with alignment")
    (license license:expat)))

(define-public node-v8-compile-cache-2.3.0
  (package
    (name "node-v8-compile-cache")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/v8-compile-cache/-/v8-compile-cache-2.3.0.tgz")
        (sha256
          (base32
            "0gwiizwmwzlzaaazw4hn4xsdfrlqk19fqbqqad6i9xfayg16qzlj"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (home-page
      "https://github.com/zertosh/v8-compile-cache#readme")
    (synopsis
      "Require hook for automatic V8 compile cache persistence")
    (description
      "Require hook for automatic V8 compile cache persistence")
    (license license:expat)))

;;; Packaged by migalmoreno
;;; Source: https://lists.sr.ht/~abcdw/rde-devel/patches/47025
(define-public node-eslint-8.17.0
  (package
    (name "node-eslint")
    (version "8.17.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://registry.npmjs.org/eslint/-/eslint-8.17.0.tgz")
        (sha256
          (base32
            "15bpflxlp9pgffr39wv4h6zb270h109ddrp9d9s530mp5qsmhw1q"))))
    (build-system node-build-system)
    (arguments
      `(#:tests?
        #f
        #:node ,node-stable
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'build))))
    (inputs
      `(("node-v8-compile-cache"
         ,node-v8-compile-cache-2.3.0)
        ("node-text-table" ,node-text-table-0.2.0)
        ("node-strip-json-comments"
         ,node-strip-json-comments-3.1.1)
        ("node-strip-ansi" ,node-strip-ansi-6.0.1)
        ("node-regexpp" ,node-regexpp-3.2.0)
        ("node-optionator" ,node-optionator-0.9.1)
        ("node-natural-compare"
         ,node-natural-compare-1.4.0)
        ("node-minimatch" ,node-minimatch-3.1.2)
        ("node-lodash-merge" ,node-lodash-merge-4.6.2)
        ("node-levn" ,node-levn-0.4.1)
        ("node-json-stable-stringify-without-jsonify"
         ,node-json-stable-stringify-without-jsonify-1.0.1)
        ("node-js-yaml" ,node-js-yaml-4.1.0)
        ("node-is-glob" ,node-is-glob-4.0.3)
        ("node-imurmurhash" ,node-imurmurhash-0.1.4)
        ("node-import-fresh" ,node-import-fresh-3.3.0)
        ("node-ignore" ,node-ignore-5.2.0)
        ("node-globals" ,node-globals-13.15.0)
        ("node-glob-parent" ,node-glob-parent-6.0.2)
        ("node-functional-red-black-tree"
         ,node-functional-red-black-tree-1.0.1)
        ("node-file-entry-cache"
         ,node-file-entry-cache-6.0.1)
        ("node-fast-deep-equal"
         ,node-fast-deep-equal-3.1.3)
        ("node-esutils" ,node-esutils-2.0.3)
        ("node-esquery" ,node-esquery-1.4.0)
        ("node-espree" ,node-espree-9.3.2)
        ("node-eslint-visitor-keys"
         ,node-eslint-visitor-keys-3.3.0)
        ("node-eslint-utils" ,node-eslint-utils-3.0.0)
        ("node-eslint-scope" ,node-eslint-scope-7.1.1)
        ("node-escape-string-regexp"
         ,node-escape-string-regexp-4.0.0)
        ("node-doctrine" ,node-doctrine-3.0.0)
        ("node-debug" ,node-debug-4.3.4)
        ("node-cross-spawn" ,node-cross-spawn-7.0.3)
        ("node-chalk" ,node-chalk-4.1.2)
        ("node-ajv" ,node-ajv-6.12.6)
        ("node-humanwhocodes-config-array"
         ,node-humanwhocodes-config-array-0.9.5)
        ("node-eslint-eslintrc"
         ,node-eslint-eslintrc-1.3.0)))
    (home-page "https://eslint.org")
    (synopsis
      "An AST-based pattern checker for JavaScript.")
    (description
      "An AST-based pattern checker for JavaScript.")
    (license license:expat)))
