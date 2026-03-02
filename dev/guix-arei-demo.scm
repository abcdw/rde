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
  #:use-module (rde api store))

;; TODO: [Andrew Tropin, 2026-03-02] Use eval to comment for demo

(define (comment)
  (build htop)
  (build foot)

  (system*
   (string-append (car (build foot)) "/bin/foot")
   (string-append (car (build htop)) "/bin/htop"))

  (define dev-profile
    (build-derivation
     (profile-derivation
      (packages->manifest (list foot htop python python-pyfiglet)))))

  (system*
   "bash" "-c"
   (string-append "source " dev-profile "/etc/profile && exec "
                  dev-profile "/bin/foot"))

  ;; python3 -c "from pyfiglet import figlet_format; print(figlet_format('Guix'))"

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
    (build-derivation
     (profile-derivation
      (packages->manifest (rde-config-home-packages config)))))

  (define foot-config
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
        "foot/foot.ini")))))

  (system*
   "bash" "-c"
   (string-append "source " demo-profile "/etc/profile && exec "
                  demo-profile "/bin/foot"
                  " --config " foot-config))

  ;; Make alacritty and foot use the same colorscheme
  )
