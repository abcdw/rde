(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
	(commit "1ac29b33f3ca19134fcbedd6dc22deb45c15229f")
        ;; Enable signature verification:
        ;; (introduction
        ;;  (make-channel-introduction
        ;;   "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
        ;;   (openpgp-fingerprint
        ;;    "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))
	)
       (channel
	(name 'guix)
	(url "https://git.savannah.gnu.org/git/guix.git")
	;; (commit "ad67d20869d7c7168941bc3d20218cb45ed82b5f")
	(branch "version-1.1.0")
	)
;;       %default-channels
       )
