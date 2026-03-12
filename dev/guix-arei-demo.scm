;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guix-arei-demo)
  #:use-module (ares guile prelude)
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
  #:use-module (srfi srfi-1)
  #:use-module ((rde api store) #:prefix store:))

(comment
 (progn
  (global-olivetti-mode 1)
  (fontaine-set-preset 'large)))

(define browse-url (lambda (url) 'hi))

(define (rde-get-config-file config file-name)
  "Build and return the store path for FILE-NAME from CONFIG's
home xdg configuration files."
  (car
   (store:build
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
  (store:build-derivation
   (profile-derivation
    (packages->manifest (rde-config-home-packages config)))))


;;;
;;; 1. Programmable
;;;

;; It would be cool to have the whole OS defined in one language, right?

(comment
 ;; Materials about functional package managers:
 (browse-url "https://trop.in/fossasia2026")

 ;; Familiar with REPL?
 (+ 1 2)
 (display "hi")

 htop

 (package-version htop)

 (package-source htop)
 ;; => #<origin #<<git-reference> url: "https://github.com/htop-dev/htop" commit: "3.4.1" recursive?: #f> #<content-hash sha256:058y4a4mvx9m179dyr4wi8mlm6i4ybywshadaj4cvfn9fv0r0nkx> () 7f598d334180>

 ;;-> store
 (store:build htop)
 ;; => ("/gnu/store/s1ldz1650jcdgjrkgaigy4pq7y272mba-htop-3.4.1")
 (store:build foot)
 ;; => ("/gnu/store/8fhrfk1y4whkvi34qvn76c3grx48zhw0-foot-1.25.0")

 ;; What is foott?
 ;;-> get syno
 (package-synopsis foot)

 (system* "xdg-open" (package-home-page foot))

 (system*
  (string-append (first (store:build foot)) "/bin/foot")
  (string-append (first (store:build htop)) "/bin/htop")))


;;;
;;; 2. Reproducible
;;;

;; Immutable store

(comment
 ;; Reproducible Inputs -> Reproducible Outputs :)

 ;;-> reproducibility via hash
 (package-source htop)
 ;; => #<origin #<<git-reference> url: "https://github.com/htop-dev/htop" commit: "3.4.1" recursive?: #f> #<content-hash sha256:058y4a4mvx9m179dyr4wi8mlm6i4ybywshadaj4cvfn9fv0r0nkx> () 7f598d334180>

 (store:build (package-source htop))
 ;; => ("/gnu/store/77sb5xlhszp2kc4m13cybx06wscf4kqz-htop-3.4.1-checkout")
 )

(comment
 ;; Modifying the package
 (define foot-gcc-14
   (package
     (inherit foot)
     (native-inputs
      (modify-inputs (package-native-inputs foot)
        (prepend gcc-14)))))

 foot
 ;; => #<package foot@1.25.0 gnu/packages/terminals.scm:872 7f598f7cc370>
 foot-gcc-14

 (store:build foot)
 ;; => ("/gnu/store/8fhrfk1y4whkvi34qvn76c3grx48zhw0-foot-1.25.0")
 (store:build foot-gcc-14)
 ;; => ("/gnu/store/01h8cxkav5129ca799hmwwhiq9xj4sjf-foot-1.25.0")

 ;; multiple versions of the same library
 ;; /usr/lib/library-a.so
 )

;; Delayed Computation and Isolated Build Environment

(comment
 (format #t "~a\n" (+ 1 2))

 ;; The build daemon and remote evaluation

 ;; G-Expressions
 (store:evaluate-gexp
  #~(format #t "~a\n" (+ 1 2)))

 (begin
   (use-modules (ice-9 ftw))
   (display (getcwd))
   (newline)
   (for-each
    (lambda (f) (display f) (display " \n"))
    (scandir (getcwd)))
   (newline))

 ;; Reproducibility via isolation
 (store:evaluate-gexp
  #~(begin
      (use-modules (ice-9 ftw))
      (display (getcwd))
      (newline)
      (for-each
       (lambda (f) (display f) (display " \n"))
       (scandir (getcwd)))
      (newline)))

 (system* "ls")
 (store:evaluate-gexp
  #~(system* "ls"))


 ;; Who is interested how delayed computations serialized and transfered to
 ;; build deamon?

 ;; Derivation and Build Daemon

 (define ls-gexp-with-deps
   #~(begin
       (display (string-append #$htop "/bin/htop"))
       (system* (string-append #$coreutils "/bin/pwd"))
       (system* (string-append #$coreutils "/bin/ls") "-lia")
       (system* (string-append #$coreutils "/bin/ls") "/" "-lia")
       ))

 (store:evaluate-gexp ls-gexp-with-deps)
 ;; => ()

 (store:run (gexp->derivation "ls-gexp" ls-gexp-with-deps))
 ;; => #<derivation /gnu/store/afq62hgnzfqvkxrf1favql1520d0y590-ls-gexp.drv =>  7f5986423820>

 ;; Not only expressions, but also config files
 (define fl (mixed-text-file
             "test.conf"

             "[general]\n"
             #~(string-append "binary = " #$htop "/bin/htop\n")
             ;; Hello FOSSASIA
             "greeting = value"))

 (store:build fl)

 )


;;;
;;; 3. Declarative
;;;

(comment
 ;; Profiles are combination of packages
 (begin
   (define dev-profile-drv
     (profile-derivation
      (packages->manifest (list foot htop python python-pyfiglet))))

   (define dev-profile
     (store:build-derivation
      dev-profile-drv))
   dev-profile)

 (store:run dev-profile-drv)

 (system* "foot")
 (system*
  "bash" "-c"
  (string-append "source " dev-profile "/etc/profile && exec "
                 dev-profile "/bin/foot"))

 ;; python3 -c "from pyfiglet import figlet_format; print(figlet_format('Guix'))"
 )

;; Polylang, Reproducible Development Environments

(comment
 (begin
   (define features
     (list
      (feature-base-packages #:home-packages (list btop))
      (feature-foot #:foot foot #:theme "zenburn")
      (feature-fonts #:default-font-size 19)))

   (define config
     (rde-config
      (features features)))

   (pretty-print-rde-config config)

   (define demo-profile
     (rde-get-profile config))

   ;; (store:build foot)

   (define foot-config
     (rde-get-config-file config "foot/foot.ini")))

 (system*
  "bash" "-c"
  (string-append
   "source " (rde-get-profile config) "/etc/profile && exec "
   "foot" " --config " (rde-get-config-file config "foot/foot.ini")))

 ;; Make alacritty and foot use the same colorscheme
 )


;;;
;;; 4. Dependable (Reliable)
;;;

;; Operating system and home environments

(comment
 (define ixy
   (@ (rde-configs configs) ixy-config))
 (pretty-print-rde-config ixy)
 ;; ~/.guix-home -> /gnu/store/v076xd5g964wnd1knxz7xvlgrzmkdwg0-home

 ;; /run/current-system -> /gnu/store/lah11gn17fd6bc1cza43knjp1ax5sifn-system

 (system* "foot" "bash" "-c" "guix home list-generations && sleep 10")
 ;; (rde-get-profile ixy)
 )


;;;
;;; 5. Deployable
;;;

;; This page is served from a remote machine, whose entire
;; operating system was deployed from emacs buffer -
;; declaratively, reproducibly, during a live presentation.

;;-> Ask for code phrase

(comment
 (define os
   (operating-system
     (host-name "pinky")
     (timezone "Europe/Amsterdam")
     (bootloader
      (bootloader-configuration
       (bootloader grub-bootloader)
       (targets '("/dev/vda"))))
     ...
     (services
      (append
       (list
        ...
        (service openssh-service-type ...)
        nginx-service)
       %base-services))))

 (define-public machines
   (list (machine
          (operating-system os)
          (environment managed-host-environment-type)
          (configuration
           (machine-ssh-configuration
            (host-name "pinky-ygg")
            (port 50621)
            (system "x86_64-linux")
            (user "bob")))))))

;; guix deploy guix-arei-demo.scm
;; => machine is deployed ... done.


;; ✓ The OS was deployed. ✓ You are looking at the result.
;;
;; Everything - the kernel, the services, this web server,
;; this page - defined in Scheme, built by Guix, running now.

;; Questions? Let's talk:
(browse-url "https://trop.in/contact")

;; P.S. We have stickers.

;;; The End.
