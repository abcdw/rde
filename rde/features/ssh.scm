(define-module (rde features ssh)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu home services)
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
          (ssh-configuration (home-ssh-configuration))
          (ssh-agent? #f))
  "Setup and configure ssh and ssh-agent."
  (ensure-pred file-like? ssh)
  (ensure-pred home-ssh-configuration? ssh-configuration)
  (ensure-pred boolean? ssh-agent?)

  (define (ssh-home-services config)
    "Returns home services related to SSH."
    (append
     (if ssh-agent?
         (let* ((sock "ssh-agent.sock"))
           (list
            (simple-service
             'start-ssh-agent-at-startup
             home-shepherd-service-type
             (list
              (shepherd-service
               (documentation "Run the ssh-agent at startup.")
               (provision '(ssh-agent))
               (requirement '())
               (start
                #~(make-forkexec-constructor
                   (list (string-append
                          #$(get-value 'ssh config)
                          "/bin/ssh-agent")
                         "-d" "-a"
                         (string-append (getenv "XDG_RUNTIME_DIR") "/" #$sock))
                   #:log-file (string-append
                               (or (getenv "XDG_LOG_HOME")
                                   (format #f "~a/.local/var/log"
                                           (getenv "HOME")))
                               "/ssh-agent.log")))
               (stop #~(make-kill-destructor)))))
            (simple-service
             'ssh-auth-socket-env-export
             home-environment-variables-service-type
             `(("SSH_AUTH_SOCK" . ,(string-append "$XDG_RUNTIME_DIR/" sock))))))
         '())
     (list (service home-ssh-service-type
                    ssh-configuration))))

  (feature
   (name 'ssh)
   (values `((ssh . ,ssh)
             ,@(if ssh-agent?
                   '((ssh-agent? . #t))
                   '())))
   (home-services-getter ssh-home-services)))
