(define-module (rde obs)
  #:use-module (gnu packages)
  #:use-module (gnu packages video)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

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
