;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guix-arei-demo)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages terminals)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features terminals)
  #:use-module (rde features fontutils)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (rde api store))

;; TODO: [Andrew Tropin, 2026-03-02] Use eval to comment for demo

(define (rde-get-config-file config file-name)
  "Build and return the store path for FILE-NAME from CONFIG's
home xdg configuration files."
  (car
   (build
    (car
     (assoc-ref
      (service-value
       (fold-services
        (instantiate-missing-services
         (cons* (service home-xdg-configuration-files-service-type '())
                (service home-profile-service-type '())
                (rde-config-home-services config)))
        #:target-type home-xdg-configuration-files-service-type))
      file-name)))))

(define (rde-get-profile config)
  (build-derivation
   (profile-derivation
    (packages->manifest (rde-config-home-packages config)))))

(define (comment)
  ;; 1. Simple packages
  (build htop)
  (build foot)

  (system*
   (string-append (car (build foot)) "/bin/foot")
   (string-append (car (build htop)) "/bin/htop"))

  ;; 1.5 gexps evaluated in isolated environment

  (evaluate-gexp
   #~(begin
       (use-modules (ice-9 ftw))
       (display (getcwd))
       (newline)
       (for-each
        (lambda (f) (display f) (display " \n"))
        (scandir (getcwd)))
       (newline)
       ;; (system* #$(file-append coreutils "/bin/touch") #$output)
       ;; (exit 10)
       ))

  (evaluate-gexp
   #~(begin
       ;; (system* "touch" "output.txt")
       (symlink #$(file-append htop "/bin/htop") #$output)

       (system* #$(file-append coreutils "/bin/pwd"))
       (system* #$(file-append coreutils "/bin/ls") "-lia")
       (system* #$(file-append coreutils "/bin/ls") "/")
       ;; (exit 1)
       #$output))


  ;; 2. Customizing packages
  (define foot-gcc-14
    (package
      (inherit foot)
      (native-inputs
       (modify-inputs (package-native-inputs foot)
         (prepend gcc-14)))))

  foot
  foot-gcc-14

  (build foot)
  (build foot-gcc-14)

  ;; multiple versions of the same library
  ;; /usr/lib/library-a.so

  (define dev-profile
    (build-derivation
     (profile-derivation
      (packages->manifest (list foot htop python python-pyfiglet)))))
  dev-profile
  ;; => "/gnu/store/hxinsrkg2r6hh4i6qzn0688wjg7rlvzr-profile"

  (system*
   "bash" "-c"
   (string-append "source " dev-profile "/etc/profile && exec "
                  dev-profile "/bin/foot"))

  ;; python3 -c "from pyfiglet import figlet_format; print(figlet_format('Guix'))"

  ;; Polylang, Reproducible Development Environments

  (define features
    (list
     (feature-base-packages #:home-packages (list htop))
     (feature-foot #:theme "onedark")
     (feature-fonts #:default-font-size 16)))

  (define config
    (rde-config
     (features features)))

  (pretty-print-rde-config config)

  (define demo-profile
    (rde-get-profile config))

  (build foot)
  ;; => ("/gnu/store/8fhrfk1y4whkvi34qvn76c3grx48zhw0-foot-1.25.0")
  (define foot-config
          ;; => "/gnu/store/jn9di4iw048v0xmvxc1v7yjmhlb05rrz-foot.ini"
    (rde-get-config-file config "foot/foot.ini"))

  (system*
   "bash" "-c"
   (string-append
    "source " (rde-get-profile config) "/etc/profile && exec "
    "foot" " --config " (rde-get-config-file config "foot/foot.ini")))

  ;; Make alacritty and foot use the same colorscheme

  )
