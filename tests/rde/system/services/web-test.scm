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

(define-module (rde system services web-test)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages web)
  #:use-module (rde serializers nginx)
  #:use-module (rde system services web)
  #:use-module (rde tests)
  #:use-module (rde api store)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

(define (serialize-config config)
  (eval-with-store (nginx-serialize config)))

(define (hardening-settings host)
  `((harden ,host please)
    (harden very much)))

(define base-services
  (list
   (service system-service-type '())
   (service etc-service-type '())
   ;; (service shepherd-root-service-type)
   (service profile-service-type '())
   ;; (service user-processes-service-type)
   (service boot-service-type #f)
   (service activation-service-type #f)
   (service account-service-type '())))

(define (services->config-string services)
  (serialize-config
   (nginx-configuration-nginx-conf
    (service-value (fold-services
                    (instantiate-missing-services
                     (append base-services services))
                    #:target-type nginx-service-type)))))

(define (drop-nth-line s n)
  (let ((lines (string-split s #\newline)))
    (when (not (<= 0 n (1- (length lines))))
      (raise-exception
       (make-exception
        (make-assertion-failure)
        (make-exception-with-message
         (format #f "n should be in a range [~a, ~a]" 0 (1- (length lines)))))))
    (string-join
     (append
      (take lines n)
      (take-right lines (- (length lines) n 1)))
     "\n")))

(define-test nginx-basic-config
  (define services
    (list
     (service
      nginx-service-type
      (nginx-configuration
       (nginx-conf
        `((load_module ,(file-append nginx-rtmp-module
                                     "/etc/nginx/modules/ngx_rtmp_module.so"))
          ,#~""
          (events (,#~""))
          (http
           ((server
             ((listen 80)
              (listen 443 ssl)
              ,@(hardening-settings 'trop.in)
              (ssl_protocols TLSv1.2)
              (server_name trop.in *.trop.in)
              (location /one (,#~""))))))

          ,#~""
          (rtmp (,#~"# The content of rtmp context will be appended here"))))))

     (simple-service
      'simple-nginx-extension
      nginx-service-type
      (nginx-extension
       (nginx-conf
        `((rtmp
           ((server (,@(hardening-settings 'rtmp.trop.in)
                     (,#~(format #f "list~a" "en") 1935)))))
          (http
           ((server trop.in ((location /one ((c d)))
                             (location /two ((e f)))))))))))))

  (define pattern
    "\
user nginx nginx;
pid /var/run/nginx/pid;

load_module /gnu/store/19apmplkgpmnvn963cfydgjhhnvpf9fs-nginx-rtmp-module-1.2.2/etc/nginx/modules/ngx_rtmp_module.so;

events {

}
http {
  server {
    listen 80;
    listen 443 ssl;
    harden trop.in please;
    harden very much;
    ssl_protocols TLSv1.2;
    server_name trop.in *.trop.in;
    location /one {

    }
  }
  server trop.in {
    location /one {
      c d;
    }
    location /two {
      e f;
    }
  }
}

rtmp {
# The content of rtmp context will be appended here
  server {
    harden rtmp.trop.in please;
    harden very much;
    listen 1935;
  }
}
")

  (test-group "nginx basic service config"
    ;; Should start failing once configuration check implemented
    (test-equal "service, simple-service http+rtmp contexts"
      (drop-nth-line pattern 3)
      (drop-nth-line (services->config-string services) 3))))
