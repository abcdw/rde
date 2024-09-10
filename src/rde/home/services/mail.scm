;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022 Miguel Ángel Moreno <me@mianmoreno.com>
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

(define-module (rde home services mail)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages mail)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix gexp)

  #:use-module (rde serializers json)

  #:export (home-goimapnotify-configuration
            home-goimapnotify-service-type))


;;;
;;; goimapnotify.
;;;

(define-configuration/no-serialization home-goimapnotify-configuration
  (goimapnotify
    (file-like go-gitlab.com-shackra-goimapnotify)
    "The @code{goimapnotify} package to use.")
  (config
   (json-config '())
   "Alist of pairs that make up the @code{goimapnotify} configuration."))

(define (add-home-goimapnotify-file config)
  `(("goimapnotify/goimapnotify.conf"
     ,(apply mixed-text-file "goimapnotify.conf"
             (serialize-json-config
              (home-goimapnotify-configuration-config config))))))

(define (home-goimapnotify-shepherd-service config)
  (list
   (shepherd-service
    (provision '(goimapnotify))
    (start #~(make-forkexec-constructor
              (list
               #$(file-append
                  (home-goimapnotify-configuration-goimapnotify config)
                  "/bin/goimapnotify"))
              #:log-file (string-append
                          (getenv "XDG_STATE_HOME") "/log"
                          "/goimapnotify.log")))
    (stop #~(make-kill-destructor)))))

(define (home-goimapnotify-profile-service config)
  (list (home-goimapnotify-configuration-goimapnotify config)))

(define home-goimapnotify-service-type
  (service-type
   (name 'home-goimapnotify)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-goimapnotify-profile-service)
     (service-extension
      home-shepherd-service-type
      home-goimapnotify-shepherd-service)
     (service-extension
      home-xdg-configuration-files-service-type
      add-home-goimapnotify-file)))
   (default-value (home-goimapnotify-configuration))
   (description "Configures the @code{goimapnotify} IMAP Mailbox notifier.")))

(define (generate-home-goimapnotify-documentation)
  (generate-documentation
   `((home-goimapnotify-configuration
      ,home-goimapnotify-configuration-fields))
   'home-goimapnotify-configuration))

