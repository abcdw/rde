(define-module (rde packages mail)
  #:use-module (gnu packages mail)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public go-gitlab.com-shackra-goimapnotify-next
  (package
    (inherit go-gitlab.com-shackra-goimapnotify)
    (version "2.4-rc4")
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
                "11vfvjdpwxf5zx23xais836v3n9bq8vawgcfb4bx8fdd6gww157q"))))))
