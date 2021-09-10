;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu home-services mcron)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-mcron-configuration
            home-mcron-service-type))

;;; Commentary:
;;
;; Service for the GNU mcron cron job manager.
;;
;; Example configuration, the first job runs mbsync once every ten
;; minutes, the second one writes "Mcron service" to ~/mcron-file once
;; every minute.
;;
;; (service home-mcron-service-type
;;            (home-mcron-configuration
;;             (jobs (list #~(job '(next-minute
;;                                  (range 0 60 10))
;;                                (lambda ()
;;                                  (system* "mbsync" "--all")))
;;                         #~(job next-minute-from
;;                                (lambda ()
;;                                  (call-with-output-file (string-append (getenv "HOME")
;;                                                                        "/mcron-file")
;;                                    (lambda (port)
;;                                      (display "Mcron service" port)))))))))
;;
;;; Code:

(define-record-type* <home-mcron-configuration> home-mcron-configuration
  make-home-mcron-configuration
  home-mcron-configuration?
  (package home-mcron-configuration-package ; package
           (default mcron))
  (jobs home-mcron-configuration-jobs   ; list of jobs
        (default '())))

(define job-files (@@ (gnu services mcron) job-files))
(define shepherd-schedule-action
  (@@ (gnu services mcron) shepherd-schedule-action))

(define home-mcron-shepherd-services
  (match-lambda
    (($ <home-mcron-configuration> mcron '()) ; no jobs to run
     '())
    (($ <home-mcron-configuration> mcron jobs)
     (let ((files (job-files mcron jobs)))
       (list (shepherd-service
              (documentation "User cron jobs.")
              (provision '(mcron))
              (modules `((srfi srfi-1)
                         (srfi srfi-26)
                         (ice-9 popen)            ; for the 'schedule' action
                         (ice-9 rdelim)
                         (ice-9 match)
                         ,@%default-modules))
              (start #~(make-forkexec-constructor
                        (list #$(file-append mcron "/bin/mcron") #$@files)
                        #:log-file (string-append
                                    (or (getenv "XDG_LOG_HOME")
                                        (format #f "~a/.local/var/log"
                                                (getenv "HOME")))
                                    "/mcron.log")))
              (stop #~(make-kill-destructor))
              (actions
               (list (shepherd-schedule-action mcron files)))))))))

(define home-mcron-profile (compose list home-mcron-configuration-package))

(define (home-mcron-extend config jobs)
  (home-mcron-configuration
   (inherit config)
   (jobs (append (home-mcron-configuration-jobs config)
                 jobs))))

(define home-mcron-service-type
  (service-type (name 'home-mcron)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-mcron-shepherd-services)
                       (service-extension
                        home-profile-service-type
                        home-mcron-profile)))
                (compose concatenate)
                (extend home-mcron-extend)
                (default-value (home-mcron-configuration))
                (description
                 "Install and configure the GNU mcron cron job manager.")))
