(define-module (gnu home-services mcron)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)

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

;; TODO: Refer to guix system mcron service for documentation, it
;; already has good docs.
(define-record-type* <home-mcron-configuration> home-mcron-configuration
  make-home-mcron-configuration
  home-mcron-configuration?
  (package home-mcron-configuration-package ; package
           (default mcron))
  (jobs home-mcron-configuration-jobs   ; list of jobs
        (default '())))

;; TODO: Export these upstream.
;; <https://issues.guix.gnu.org/47238>
(define (job-files mcron jobs)
  "Return a list of file-like object for JOBS, a list of gexps."
  (define (validated-file job)
    ;; This procedure behaves like 'scheme-file' but it runs 'mcron
    ;; --schedule' to detect any error in JOB.
    (computed-file "mcron-job"
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils))

                         (call-with-output-file "prologue"
                           (lambda (port)
                             ;; This prologue allows 'mcron --schedule' to
                             ;; proceed no matter what #:user option is passed
                             ;; to 'job'.
                             (write '(set! getpw
                                       (const (getpwuid (getuid))))
                                    port)))

                         (call-with-output-file "job"
                           (lambda (port)
                             (write '#$job port)))

                         (invoke #+(file-append mcron "/bin/mcron")
                                 "--schedule=20" "prologue" "job")
                         (copy-file "job" #$output)))
                   #:options '(#:env-vars (("COLUMNS" . "150")))))

  (map validated-file jobs))

(define (shepherd-schedule-action mcron files)
  "Return a Shepherd action that runs MCRON with '--schedule' for the given
files."
  (shepherd-action
   (name 'schedule)
   (documentation
    "Display jobs that are going to be scheduled.")
   (procedure
    #~(lambda* (_ #:optional (n "5"))
        ;; XXX: This is a global side effect.
        (setenv "GUILE_AUTO_COMPILE" "0")

        ;; Run 'mcron' in a pipe so we can explicitly redirect its output to
        ;; 'current-output-port', which at this stage is bound to the client
        ;; connection.
        (let ((pipe (open-pipe* OPEN_READ
                                #$(file-append mcron "/bin/mcron")
                                (string-append "--schedule=" n)
                                #$@files)))
          (let loop ()
            (match (read-line pipe 'concat)
              ((? eof-object?)
               (catch 'system-error
                 (lambda ()
                   (zero? (close-pipe pipe)))
                 (lambda args
                   ;; There's a race with the SIGCHLD handler, which
                   ;; could call 'waitpid' before 'close-pipe' above does.  If
                   ;; we get ECHILD, that means we lost the race, but that's
                   ;; fine.
                   (or (= ECHILD (system-error-errno args))
                       (apply throw args)))))
              (line
               (display line)
               (loop)))))))))

(define home-mcron-shepherd-services
  (match-lambda
    (($ <home-mcron-configuration> mcron '()) ; no jobs to run
     '())
    (($ <home-mcron-configuration> mcron jobs)
     (let ((files (job-files mcron jobs)))
       (list (shepherd-service
              (documentation "User cron jobs.")
              (provision '(home-mcron))
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

;; Append new jobs
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
