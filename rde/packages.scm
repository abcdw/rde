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


;; (define-module (rde obs)
;;   #:use-module (gnu packages)
;;   #:use-module (gnu packages video)
;;   #:use-module (guix packages)
;;   #:use-module (guix git-download)
;;   #:use-module (guix build-system cmake)
;;   #:use-module (guix gexp)
;;   #:use-module ((guix licenses) #:prefix license:))

(use-modules (gnu packages video)
             (gnu packages glib))
(define-public obs-latest
  (package
   (inherit obs)
   (name "obs")
   (version "27.0.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/obsproject/obs-studio")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1sy58mg9dris261ia6l6xaswl4ks76xh4fcsj81i2hfg1sjy1vxv"))
            (patches
             (search-patches "obs-modules-location.patch"))))
   (inputs (append
            (package-inputs obs)
            `(("wayland" ,wayland)
              ("pipewire" ,pipewire-0.3)
              ("glib" ,glib)
              ("wayland-protocols" ,wayland-protocols))))
   (arguments
    `(#:configure-flags
      (list (string-append "-DOBS_VERSION_OVERRIDE=" ,version)
            "-DENABLE_UNIT_TESTS=TRUE"
            "-DBUILD_BROWSER=FALSE"
            "-DBUILD_VST=FALSE")
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-executable
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (plugin-path (getenv "QT_PLUGIN_PATH")))
              (wrap-program (string-append out "/bin/obs")
                `("QT_PLUGIN_PATH" ":" prefix (,plugin-path))))
            #t)))))
   ;; (native-search-paths
   ;;  (list
   ;;   (search-path-specification
   ;;    (variable "OBS_PLUGINS_DATA_PATH")
   ;;    (files '("share/obs/obs-plugins")))
   ;;   (search-path-specification
   ;;    (variable "OBS_PLUGINS_PATH")
   ;;    (files '("lib/obs-plugins")))))
   ))



;; xdg-desktop-portal-latest
