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
  #:use-module ((rde api store) #:prefix store:))

(comment
 (progn
  (global-olivetti-mode 1)
  (fontaine-set-preset 'large)))

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
;;; 1. Simple Packages
;;;

;; TODO: [Andrew Tropin, 2026-03-02] Use eval to comment for demo


(comment
 (store:build htop)
 (store:build foot)

 (system*
  (string-append (car (store:build foot)) "/bin/foot")
  (string-append (car (store:build htop)) "/bin/htop")))



;;;
;;; 1.5 G-Exps Evaluated in Isolated Environment
;;;

;; Explain the build daemon, store will be later

(comment
 (store:evaluate-gexp
  #~(begin
      (use-modules (ice-9 ftw))
      (display (getcwd))
      (newline)
      (for-each
       (lambda (f) (display f) (display " \n"))
       (scandir (getcwd)))
      (newline)))

 (begin
   (use-modules (ice-9 ftw))
   (display (getcwd))
   (newline)
   (for-each
    (lambda (f) (display f) (display " \n"))
    (scandir (getcwd)))
   (newline))

 (store:evaluate-gexp
  #~(system* "ls"))

 (define ls-gexp-with-deps
   #~(begin
       ;; (system* "touch" "output.txt")
       (symlink #$(file-append htop "/bin/htop") #$output)

       (system* #$(file-append coreutils "/bin/pwd"))
       (system* #$(file-append coreutils "/bin/ls") "-lia")
       (system* #$(file-append coreutils "/bin/ls") "/")
       ;; (exit 1)
       #$output))

 (store:run (gexp->derivation "ls-gexp" ls-gexp-with-deps))

 (store:evaluate-gexp ls-gexp-with-deps))



;;;
;;; 2. Customizing packages
;;;

(comment
 (define foot-gcc-14
   (package
     (inherit foot)
     (native-inputs
      (modify-inputs (package-native-inputs foot)
        (prepend gcc-14)))))

 foot
 foot-gcc-14

 (store:build foot)
 (store:build foot-gcc-14)

 ;; multiple versions of the same library
 ;; /usr/lib/library-a.so

 (define dev-profile
   (store:build-derivation
    (profile-derivation
     (packages->manifest (list foot htop python python-pyfiglet)))))
 dev-profile
 ;; => "/gnu/store/hxinsrkg2r6hh4i6qzn0688wjg7rlvzr-profile"

 (system*
  "bash" "-c"
  (string-append "source " dev-profile "/etc/profile && exec "
                 dev-profile "/bin/foot"))

 ;; python3 -c "from pyfiglet import figlet_format; print(figlet_format('Guix'))"
 )



;;;
;;; 3. Reproducible Development Environments
;;;

;; Polylang, Reproducible Development Environments

(comment
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

 (store:build foot)

 (define foot-config
   (rde-get-config-file config "foot/foot.ini"))

 (system*
  "bash" "-c"
  (string-append
   "source " (rde-get-profile config) "/etc/profile && exec "
   "foot" " --config " (rde-get-config-file config "foot/foot.ini")))

 ;; Make alacritty and foot use the same colorscheme
 )


;; TODO: [Andrew Tropin, 2026-03-06] Mobile internet + guix deploy
