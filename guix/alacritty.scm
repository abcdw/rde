(use-modules (guix packages)
             (guix git-download)
             (guix download)
             (guix build-system cargo)
             (guix licenses))

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
             (base32 "0d9qnymi8v4aqm2p300ccdsgavrnd64sv7v0cz5dp0sp5c0vd7jl"))))
   (build-system cargo-build-system)
   (synopsis "A cross-platform, GPU-accelerated terminal emulator ")
   (description "A cross-platform, GPU-accelerated terminal emulator ")
   (home-page "https://github.com/qwilm/alacritty")
   (license asl2.0)
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




alacritty
