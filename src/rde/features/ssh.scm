;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (rde features ssh)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp)

  #:export (feature-ssh)

  #:re-export (home-ssh-configuration
               ssh-host
               ssh-match))


(define* (feature-ssh
          #:key
          (ssh openssh)
          (mosh mosh)
          (ssh-configuration (home-ssh-configuration))
          (ssh-agent? #f))
  "Setup and configure ssh and ssh-agent."
  (ensure-pred file-like? ssh)
  (ensure-pred file-like? mosh)
  (ensure-pred home-ssh-configuration? ssh-configuration)
  (ensure-pred boolean? ssh-agent?)

  (define (ssh-home-services config)
    "Returns home services related to SSH."
    (append
     (if ssh-agent?
         (service home-ssh-agent-service-type
                  (home-ssh-agent-configuration
                   (openssh ssh)))
         '())
     (list
      (simple-service
       'ssh-mosh
       home-profile-service-type
       (list mosh))
      (service home-ssh-service-type
               ssh-configuration))))

  (feature
   (name 'ssh)
   (values `((ssh . ,ssh)
             ,@(if ssh-agent?
                   '((ssh-agent? . #t))
                   '())))
   (home-services-getter ssh-home-services)))
