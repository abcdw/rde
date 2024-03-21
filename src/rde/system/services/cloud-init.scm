;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021 David Dashyan <mail@davie.li>
;;; Copyright © 2024 Nikita Domnitskii
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde system services cloud-init)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module ((gnu packages linux) #:select (e2fsprogs))
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu services shepherd)
  #:use-module (guix channels)
  #:use-module (guix describe)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (cloud-init-service-type))

;; TODO: [Nikita Domnitskii, 2024-02-12] think of a better way to
;; implement datasources
(define metadata-host
  (make-parameter "169.254.169.254"))

(define metadata-port
  (make-parameter 80))

(define metadata-path
  (make-parameter "/metadata/v1.json"))

(define-record-type* <cloud-init-config>
  cloud-init-config make-cloud-init-config
  cloud-init-config?
  (metadata-host cloud-init-config-metadata-host
                 (default (metadata-host)))
  (metadata-path cloud-init-config-metadata-path
                 (default (metadata-path)))
  (log-file      cloud-init-config-file
                 (default "/var/log/cloud-init.log"))
  (requirements  cloud-init-config-requirements
                 (default '()))) ;; + loopback

(define* (query-metadata)
  (let ((select? (match-lambda
                   (('json rest ...)  #t)
                   (_ #f))))
    (scheme-file
     "query-metadata"
     (with-extensions (list guile-json-4)
       (with-imported-modules
           (source-module-closure '((json)) #:select? select?)
         #~(begin
             (use-modules (json)
                          (ice-9 match)
                          (web client)
                          (web request)
                          (web response)
                          (web uri)
                          (rnrs bytevectors)
                          (srfi srfi-71))
             (let* ((xhost #$(metadata-host))
                    (xpath #$(metadata-path))
                    (xport #$(metadata-port))
                    (uri (build-uri 'http
                                    #:host xhost
                                    #:path xpath
                                    #:port xport))
                    (response body (http-request uri #:method 'GET)))
               (if (eq? (response-code response) 200)
                   (cond
                    ((and (string? body) body) => json-string->scm)
                    ((and (bytevector? body) body) =>
                     (compose json-string->scm utf8->string)))
                   (throw 'metadata-query-error response)))))))))

(define (resize-partition config)
  (program-file
   "resize-partition"
   (with-extensions (list guile-parted guile-bytestructures)
     #~(begin
         (use-modules (parted)
                      (bytestructures guile)
                      (srfi srfi-71)
                      ((system foreign) #:prefix ffi:))
         (let* ((device          (get-device "/dev/vda"))
                (disk            (disk-new device))
                (main-part       (disk-get-partition disk 1))
                (next-part       (partition-next main-part))
                (main-part-start (geometry-start (partition-geom main-part)))
                (next-part-end   (geometry-end (partition-geom next-part)))
                (start-range     (make-geometry))
                (start-range-bs  (geometry-bytestructure start-range))
                (device-addr     (ffi:pointer-address (device->pointer device)))

                (_ (bytestructure-set! start-range-bs 'dev device-addr))
                (_ (bytestructure-set! start-range-bs 'start main-part-start))
                (_ (bytestructure-set! start-range-bs 'end next-part-end))
                (_ (bytestructure-set! start-range-bs 'length 1))

                (end-range  (geometry-new
                             device
                             #:start next-part-end
                             #:length 1))
                (constraint (constraint-new
                             #:start-align 'any
                             #:end-align   'any
                             #:start-range start-range
                             #:end-range   end-range
                             #:min-size    1
                             #:max-size    (device-length device)))

                (errno _ (disk-maximize-partition disk main-part constraint)))
           (unless (zero? errno)
             (disk-commit disk)
             (system* (string-append #$e2fsprogs "/sbin/resize2fs")
                      "/dev/vda1")))))))

(define (cloud-init-shepherd-services config)
  (list
   (shepherd-service
    (documentation "Initialize the machine's host name.")
    (provision '(cloud-host-name))
    (requirement '(host-name networking))
    (start #~(lambda _
               (let* ((metadata (load #$(query-metadata)))
                      (hostname (assoc-ref metadata "hostname")))
                 (sethostname hostname))))
    (one-shot? #t))
   (shepherd-service
    (documentation "Extend the machine's partition")
    (provision '(cloud-resize-partition))
    (requirement `(root-file-system))
    (start #~(lambda _ (system* #$(resize-partition config))))
    (one-shot? #t))))

;; FIXME: [Nikita Domnitskii, 2024-02-13] Would be nice to have, but not
;; necessary at the moment. Needs modifications to openssh-service to
;; correctly work with gexp extensions
(define (cloud-init-ssh-keys config)
  `(("root")))

(define cloud-init-service-type
  (service-type
   (name 'cloud-init)
   (description "")
   (extensions
    (list (service-extension
           shepherd-root-service-type
           cloud-init-shepherd-services)
          #;(service-extension
           openssh-service-type
           cloud-init-ssh-keys)
          #;(service-extension
          static-networking-service-type
          cloud-init-static-networking)))
   (default-value (cloud-init-config))))
