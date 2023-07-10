;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022, 2023 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde home services i2p)
  #:use-module (rde serializers ini)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages i2p)

  #:use-module (guix packages)
  #:use-module (guix gexp)

  #:use-module (srfi srfi-1)

  #:export (home-i2pd-service-type
            home-i2pd-configuration
            home-i2pd-extension))


(define-configuration home-i2pd-configuration
  (i2pd
   (file-like i2pd)
   "I2Pd package to use.")
  (i2pd-conf
   (ini-config '())
   "I2Pd configuration.")
  (tunnels-conf
   (ini-config '())
   "I2Pd tunnel configuration."))

(define-configuration home-i2pd-extension
  (i2pd-conf
   (ini-config '())
   "I2Pd configuration.")
  (tunnels-conf
   (ini-config '())
   "I2Pd tunnel configuration."))

(define (home-i2pd-shepherd-service config)
  (let ((i2pd (home-i2pd-configuration-i2pd config)))
    (list
     (shepherd-service
      (provision '(i2pd))
      (start #~(make-forkexec-constructor
                (list #$(file-append i2pd "/bin/i2pd"))
                #:pid-file (string-append (getenv "HOME") "/.i2pd/i2pd.pid")
                #:log-file (string-append
                            (getenv "XDG_STATE_HOME") "/log"
                            "/i2pd.log")))
      ;; TODO: Add graceful shutdown.
      (stop #~(make-kill-destructor))
      (documentation "Run I2Pd")))))

(define (i2pd-config-file name config)
  (apply mixed-text-file name (serialize-ini-config config)))

(define (add-i2pd-configuration config)
  `((".i2pd/i2pd.conf"
     ,(i2pd-config-file "i2pd-config.conf"
                       (home-i2pd-configuration-i2pd-conf config)))
    (".i2pd/tunnels.conf"
     ,(i2pd-config-file "i2pd-tunnels.conf"
                       (home-i2pd-configuration-tunnels-conf config)))))

(define (home-i2pd-extensions config extensions)
  (let ((extensions (reverse extensions)))
    (home-i2pd-configuration
     (inherit config)
     (i2pd-conf
      (fold
       ini-append
       '()
       (cons
        (home-i2pd-configuration-i2pd-conf config)
        (map home-i2pd-extension-i2pd-conf extensions))))
     (tunnels-conf
      (fold
       ini-append
       '()
       (cons
        (home-i2pd-configuration-tunnels-conf config)
        (map home-i2pd-extension-tunnels-conf extensions)))))))

(define home-i2pd-service-type
  (service-type (name 'home-i2pd)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-i2pd-shepherd-service)
                       (service-extension
                        home-files-service-type
                        add-i2pd-configuration)))
                (compose identity)
                (extend home-i2pd-extensions)
                (default-value (home-i2pd-configuration))
                (description "I2Pd service.")))
