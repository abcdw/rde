;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (rde system services web)
  #:use-module (rde serializers nginx)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages web)
  #:use-module (gnu packages admin)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)

  #:export (nginx-configuration
            nginx-configuration-nginx-conf
            nginx-extension
            nginx-service-type))

;; TODO: Restart nginx without killing existing processes
;; TODO: Add reload action to nginx shepherd service
;; TODO: Set default worker_processes to auto to use all cores

(define-configuration/no-serialization nginx-configuration
  (nginx
   (file-like nginx)
   "Nginx package to use.")
  (shepherd-requirement
   (list '())
   "List of shepherd services.")
  (run-directory
   (string "/var/run/nginx")
   "Nginx run directory")
  (initial-conf
   (nginx-config
    `((user nginx nginx)
      (pid /var/run/nginx/pid)
      ,#~""))
   "Initial Nginx configuration.")
  (nginx-conf
   (nginx-config '())
   "Nginx configuration."))

(define-configuration/no-serialization nginx-extension
  (nginx-conf
   (nginx-config '())
   "Nginx configuration."))

(define (get-nginx-conf-file config)
  (let ((file (mixed-text-file
               "intermediate-nginx.conf"
               (nginx-serialize (nginx-configuration-nginx-conf config)))))
    (computed-file
     "nginx.conf"
     (with-imported-modules '((guix build utils))
       #~(begin
           ;; TODO: Add check config functionality, mb eval/container?
           ;; (use-modules (guix build utils))
           ;; (mkdir-p "/tmp/nginx/logs")
           ;; (invoke #$(file-append nginx "/sbin/nginx")
           ;;         "-t" "-p" "/tmp/nginx" "-c" #$file)
           (copy-file #$file #$output))))))

(define (nginx-extensions original-config extensions)
  (let ((extensions (reverse extensions)))
    (nginx-configuration
     (inherit original-config)
     (nginx-conf
      (apply
       nginx-merge
       (nginx-configuration-initial-conf original-config)
       (nginx-configuration-nginx-conf original-config)
       (map nginx-extension-nginx-conf extensions))))))

(define %nginx-accounts
  (list (user-group (name "nginx") (system? #t))
        (user-account
         (name "nginx")
         (group "nginx")
         (system? #t)
         (comment "nginx server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (nginx-activation config)
  (match-record config <nginx-configuration>
    (nginx run-directory)
   #~(begin
       (use-modules (guix build utils))

       (format #t "creating nginx run and logs directory '~a'~%"
               #$run-directory)
       (mkdir-p #$run-directory)
       (mkdir-p #$(string-append run-directory "/logs"))
       ;; Check configuration file syntax. (Probably should be done build-time)
       (system* (string-append #$nginx "/sbin/nginx")
                "-c" #$(get-nginx-conf-file config)
                "-p" #$run-directory
                "-t"))))

(define (nginx-shepherd-service config)
  (match-record config <nginx-configuration>
    (nginx run-directory shepherd-requirement)
   (let* ((nginx-binary (file-append nginx "/sbin/nginx"))
          (pid-file (in-vicinity run-directory "pid"))
          (config-file (get-nginx-conf-file config))
          (nginx-action
           (lambda args
             #~(lambda _
                 (invoke #$nginx-binary "-c" #$config-file #$@args)
                 (match '#$args
                   (("-s" . _) #f)
                   (_ #$#~(read-pid-file #$pid-file)))))))

     (list (shepherd-service
            (provision '(nginx))
            (documentation "Run the nginx daemon.")
            (requirement `(user-processes loopback ,@shepherd-requirement))
            (modules `((ice-9 match)
                       ,@%default-modules))
            (start (nginx-action "-p" run-directory))
            (stop (nginx-action "-s" "stop"))
            (actions
              (list
               (shepherd-configuration-action config-file)
               (shepherd-action
                 (name 'reload)
                 (documentation "\
Reload nginx configuration file and restart worker processes.  This has the
effect of killing old worker processes and starting new ones, using the same
configuration file.  It is useful for situations where the same nginx
configuration file can point to different things after a reload, such as
renewed TLS certificates, or @code{include}d files.")
                 (procedure (nginx-action "-s" "reload"))))))))))

(define nginx-service-type
  (service-type (name 'nginx)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          nginx-shepherd-service)
                       (service-extension activation-service-type
                                          nginx-activation)
                       (service-extension account-service-type
                                          (const %nginx-accounts))))
                (compose identity)
                (extend nginx-extensions)
                (default-value (nginx-configuration))
                (description "Run the nginx Web server.")))
