;;; rde --- Reproducible development environment
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features containers)
  #:use-module (rde features)
  #:use-module (rde lib file)
  #:use-module (gnu packages containers)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (rde home services shells)

  #:export (feature-podman))

(define* (feature-podman
          #:key
          (podman podman)
          (docker-alias? #t))
  "Install and configure rootless podman.  Be aware that this podman
configuration relies on btrfs."
  (ensure-pred file-like? podman)
  (ensure-pred boolean? docker-alias?)

  (define f-name 'podman)
  (define (get-home-services config)
    (list
     (when (and (get-value 'zsh config) docker-alias?)
       (simple-service
        'direnv-zsh-hook
        home-zsh-service-type
        (home-zsh-extension
         (zshrc
          (list "alias docker=podman")))))
     (simple-service
      'podman-add-podman-package
      home-profile-service-type
      (list podman podman-compose))
     (simple-service
      'podman-configs
      home-xdg-configuration-files-service-type
      `(("containers/registries.conf"
         ,(plain-file
           "registries.conf"
           "unqualified-search-registries = ['docker.io', \
'registry.fedoraproject.org', \
'registry.access.redhat.com', \
'registry.centos.org']"))
        ("containers/storage.conf"
         ,(plain-file
           "storage.conf"
           "[storage]\ndriver = \"btrfs\""))
	("containers/policy.json"
         ,(plain-file
           "policy.json"
           "{\"default\": [{\"type\": \"insecureAcceptAnything\"}]}"))))))

  (define (get-system-services config)
    (define user-name (get-value 'user-name config))
    (list
     (simple-service
      'podman-subuid-subgid
      ;; If subuid/subgid will be needed somewhere else, the service must be
      ;; created to handle it.
      etc-service-type
      `(("subuid"
         ,(plain-file
           "subuid"
           (string-append user-name ":100000:65536\n")))
        ("subgid"
         ,(plain-file
           "subgid"
           (string-append user-name ":100000:65536\n")))))))

  (feature
   (name f-name)
   (values `((,f-name . ,podman)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
