(define-module (rde packages mail)
  #:use-module (gnu packages mail)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public go-gitlab.com-shackra-goimapnotify-next
  (package
    (inherit go-gitlab.com-shackra-goimapnotify)
    (version "2.4-rc3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/shackra/goimapnotify")
                    (commit version)))
              (file-name
               (git-file-name
                (package-name go-gitlab.com-shackra-goimapnotify) version))
              (sha256
               (base32
                "0dk12x0x5zan86fdi5wi5zv545vmccs15cdrc2ica9afy189zvdn"))))))
