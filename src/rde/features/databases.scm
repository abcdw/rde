;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
;;; Copyright © 2023, 2024 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (rde features databases)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services databases)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (feature-postgresql
            feature-sqlite)
  #:re-export (postgresql-role))

(define-public (list-of-postgresql-roles? lst)
  (and (list? lst) (every postgresql-role? lst)))

(define-public (maybe-list-of-postgresql-roles? x)
  (or (list-of-postgresql-roles? x) (not x)))

(define* (feature-postgresql
          #:key
          (postgresql postgresql)
          (extension-packages '())
          (postgresql-roles #f))
  "Configure the PostgreSQL relational database."
  (ensure-pred file-like? postgresql)
  (ensure-pred list-of-file-likes? extension-packages)
  (ensure-pred maybe-list-of-postgresql-roles? postgresql-roles)

  (define (postgresql-role->user-account role)
    (user-account
     (name (postgresql-role-name role))
     (group "postgres")
     (system? #t)
     (comment "PostgreSQL user")
     (home-directory "/var/empty")
     (shell (file-append shadow "/sbin/nologin"))))

  (define f-name 'postgresql)

  (define (get-system-services config)
    "Return system services related to PostgreSQL."
    (append
     (list
      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql)
                (extension-packages extension-packages))))
     (if postgresql-roles
         (list
          (service postgresql-role-service-type
                   (postgresql-role-configuration
                    (roles postgresql-roles)))
          ;; Default upstream pg_hba.conf is configured for peer authentication
          ;; so it is necessary to have dedicated system accounts.
          (simple-service
           'postgresql-roles-accounts
           account-service-type
           (map postgresql-role->user-account postgresql-roles)))
         '())))

  (define (get-home-services config)
    "Return home services related to PostgreSQL."
    (append
     (list
      (simple-service
       'postgresql-xdg-base-dirs-specification
       home-environment-variables-service-type
       '(("PSQL_HISTORY" . "$XDG_STATE_HOME/psql_history"))))
     (if (get-value 'emacs config #f)
         (list
          (rde-elisp-configuration-service
           f-name
           config
           `(,@(if (get-value 'emacs-org config #f)
                   '((with-eval-after-load 'ob-core
                       (require 'ob-sql))
                     (with-eval-after-load 'ob-sql
                       (setq org-babel-default-header-args:sql
                             '((:engine . "postgresql")))))
                   '()))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . ,postgresql)))
   (system-services-getter get-system-services)
   (home-services-getter get-home-services)))

(define* (feature-sqlite
          #:key
          (sqlite sqlite))
  "Configure the SQLite relational database."
  (ensure-pred file-like? sqlite)

  (define f-name 'sqlite)

  (define (get-home-services config)
    "Return home services related to SQLite."
    (list
     (simple-service
      'add-sqlite-home-package
      home-profile-service-type
      (list sqlite))
     (simple-service
      'sqlite-xdg-base-dirs-specification
      home-environment-variables-service-type
      '(("SQLITE_HISTORY" . "$XDG_CACHE_HOME/sqlite_history")))))

  (feature
   (name f-name)
   (values `((,f-name . ,sqlite)))
   (home-services-getter get-home-services)))
