(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:))

(define-public pipewire-latest
  (package
   (inherit pipewire-0.3)
   (name "pipewire")
   (version "0.3.29")
    (arguments
     '(#:configure-flags
       (list (string-append "-Dudevrulesdir=" (assoc-ref %outputs "out")
                            "/lib/udev/rules.d")
             "-Dsystemd=disabled")
       #:phases
       (modify-phases %standard-phases
		      ;; Skip shrink-runpath, otherwise validate-runpath fails.
		      (delete 'shrink-runpath))))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/PipeWire/pipewire")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "16jjxcnahxqfcawz77ywx837ybhwzcivn7hgqb9cmlp1y2syy8gk"))))))

(define-public rewrite-to-pipewire-latest
  ;; Replace all the packages called "openssl" with LibreSSL.
  (package-input-rewriting/spec `(("pipewire" . ,(const pipewire-latest)))))

(define-public xdg-desktop-portal-latest
  (rewrite-to-pipewire-latest
   (package
    (inherit xdg-desktop-portal)
    (name "xdg-desktop-portal")
    (version "1.8.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/flatpak/xdg-desktop-portal")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0pq0kmvzk56my396vh97pzw4wizwmlmzvv2kr2xv047x3044mr5n"))))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DESKTOP_PORTAL_DIR")
            (separator #f)
            (files '("share/xdg-desktop-portal/portals")))))
    (arguments
     `(#:phases
       (modify-phases
        %standard-phases
        (add-after 'unpack 'po-chmod
                   (lambda _
                     ;; Make sure 'msgmerge' can modify the PO files.
                     (for-each (lambda (po)
                                 (chmod po #o666))
                               (find-files "po" "\\.po$"))
                     #t))))))))

(define-public xdg-desktop-portal-wlr-latest
  (rewrite-to-pipewire-latest
   (package
    (inherit xdg-desktop-portal-wlr)
    (name "xdg-desktop-portal-wlr")
    (version "0.4.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/emersion/xdg-desktop-portal-wlr")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "13fbzh8bjnhk4xs8j9bpc01q3hy27zpbf0gkk1fnh3hm5pnyfyiv"))))
    (arguments
     `(#:configure-flags
       '("-Dsystemd=disabled"
         "-Dsd-bus-provider=libelogind")))
    (inputs
     (append (package-inputs xdg-desktop-portal-wlr)
             `(("inih" ,libinih)))))))

;; (define-module (rde obs)
;;   #:use-module (gnu packages)
;;   #:use-module (gnu packages video)
;;   #:use-module (guix packages)
;;   #:use-module (guix git-download)
;;   #:use-module (guix build-system cmake)
;;   #:use-module (guix gexp)
;;   #:use-module ((guix licenses) #:prefix license:))

;; (define-public obs-next
;;   (package/inherit obs
;;     (name "obs-next")
;;     (version "master")
;;     (source
;;      (local-file "/home/bob/work/obs-studio" #:recursive? #t))
;;     (native-search-paths
;;      (list
;;       (search-path-specification
;;        (variable "OBS_PLUGINS_DATA_PATH")
;;        (files '("share/obs/obs-plugins")))
;;       (search-path-specification
;;        (variable "OBS_PLUGINS_PATH")
;;        (files '("lib/obs-plugins")))))))


;; xdg-desktop-portal-latest
