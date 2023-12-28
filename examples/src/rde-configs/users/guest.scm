(define-module (rde-configs users guest)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (rde features base)
  #:use-module (rde features irc)
  #:use-module (rde features keyboard)
  #:use-module (rde features presets)
  #:use-module (rde features terminals)
  #:use-module (rde features version-control)
  #:use-module (rde features)
  #:use-module (rde home services wm)
  #:use-module (rde packages)
  #:use-module (srfi srfi-1))

(define %general-features
  (append
   rde-base
   rde-desktop
   rde-cli
   rde-emacs))

(define sway-wlr-settings-service
  (simple-service
   'sway-wlr-settings
   home-environment-variables-service-type
   ;; Make sway work on virtual gpu in qemu
   `(("WLR_RENDERER_ALLOW_SOFTWARE" . "1")
     ("WLR_NO_HARDWARE_CURSORS" . "1"))))

(define sway-live-extra-config-service
  (simple-service
   'sway-output-settings
   home-sway-service-type
   `((output Virtual-1 mode 1920x1080 scale 2)
     (exec emacs --eval "'(info \"(rde)Getting Started\")'")
     ;; A hack to make other emacsclient based apps (like app launcher) to
     ;; work.
     (exec "sleep 10s && emacsclient -a '' -c --eval '(delete-frame)'"))))

(define home-profile-live-extra-packages-service
  (simple-service
   'home-profile-live-extra-packages
   home-profile-service-type
   (append
    (strings->packages
     "icecat"
     "imv" "wev"
     "make"
     ;; "nroff" ;; man doesn't work according to report
     "adwaita-icon-theme" "gnome-themes-extra"
     "hicolor-icon-theme" ;; needed for nm icons

     "cryptsetup" "dosfstools" "btrfs-progs"
     "smartmontools" "parted" "gparted"

     "binutils" ;; needed for emacs native-comp stuff

     "emacs-arei" "guile-next" "guile-ares-rs"

     "ripgrep" "curl"))))

(define example-configs-service
  (simple-service
   'live-example-configs
   home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Create ~/example-rde-configs.")
     (requirement '())
     (provision '(rde-configs))
     (start
      (with-imported-modules '((guix build utils))
        #~(lambda ()
            (let ((rde-configs
                   #$(local-file
                      "../../.." "example-rde-configs"
                      #:recursive? #t
                      #:select?
                      (lambda (file _)
                        (not (string=? (basename file) "target")))))

                  (output
                   (string-append (getenv "HOME") "/example-rde-configs")))
              (when (not (file-exists? output))
                (mkdir-p output)
                (copy-recursively
                 rde-configs
                 output
                 #:copy-file (lambda (f t)
                               (copy-file f t)
                               (make-file-writable t)))
                ;; MAYBE: take this value from rde-config
                (system* #$(file-append (@ (gnu packages shellutils) direnv)
                                        "/bin/direnv") "allow" output))))))
     (auto-start? #t)
     (one-shot? #t)))))

(define (feature-additional-services)
  (feature-custom-services
    #:feature-name-prefix 'live
    #:home-services
    (list
     example-configs-service
     sway-live-extra-config-service
     sway-wlr-settings-service
     home-profile-live-extra-packages-service)))

(define-public %guest-features
  (append
   (remove
    (lambda (f) (member (feature-name f) '(git markdown)))
    %general-features)
   (list
    (feature-additional-services)
    (feature-user-info
     #:user-name "guest"
     #:full-name "rde user"
     #:email "guest@rde"
     ;; (crypt "guest" "$6$abc")
     #:user-initial-password-hash
     "$6$abc$9a9KlQ2jHee45D./UOzUZWLHjI/atvz2Dp6.Zz6hjRcP2KJv\
G9.lc/f.U9QxNW1.2MZdV1KzW6uMJ0t23KKoN/")

    (feature-keyboard
     ;; To get all available options, layouts and variants run:
     ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
     #:keyboard-layout
     (keyboard-layout
      "us,us"
      ",dvorak"
      #:options '("grp:shifts_toggle" "ctrl:nocaps")))
    (feature-git #:sign-commits? #f)
    (feature-foot)
    (feature-irc-settings
     #:irc-accounts (list
                     (irc-account
                      (id 'libera)
                      (network "irc.libera.chat")
                      (nick "rde-user"))
                     (irc-account
                      (id 'oftc)
                      (network "irc.oftc.net")
                      (nick "rde-user")))))))
