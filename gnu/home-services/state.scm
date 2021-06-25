(define-module (gnu home-services state)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages ssh)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix records)

  #:export (home-state-service-type
	    state-generic
	    state-git
            state-hg
	    state-rsync))

(use-modules (gnu packages version-control))
(define* (state-hg path remote #:key (config #f))
  (state-generic
   path
   #:init-gexp
   #~(lambda* (_ self)
       (let* ((meta (car (action self 'metadata)))
              (path (assoc-ref meta 'path))
              (remote (assoc-ref meta 'remote)))
         (format #t "Initializing ~a.\n" self)
         (let* ((port ((@@ (guix build utils) open-pipe-with-stderr)
                       #$(file-append mercurial "/bin/hg") "clone" remote path)))
           (waitpid WAIT_ANY)
           (display ((@@ (ice-9 rdelim) read-delimited) "" port))
           (close-port port))

         (when '#$config
           (call-with-output-file (string-append path "/.hg/hgrc")
             (lambda (port) (display (string-append
                                      #$@(serialize-hg-config config)) port))))))
   #:additional-metadata `((remote . ,remote)
                           (general-sync? . #f))))

(define* (state-git path remote #:key (config #f))
  (state-generic
   path
   #:init-gexp
   #~(lambda* (_ self)
       (let* ((meta (car (action self 'metadata)))
	      (path (assoc-ref meta 'path))
	      (remote (assoc-ref meta 'remote)))
	 (format #t "Initializing ~a.\n" self)
	 ;; TODO: revisit git clone implementation
	 ;; FIXME: Hang up shepherd if username/password asked
	 (let* ((port ((@@ (guix build utils) open-pipe-with-stderr)
		       #$(file-append git "/bin/git") "clone" remote path)))
	   (waitpid WAIT_ANY)
	   (display ((@@ (ice-9 rdelim) read-delimited) "" port))
	   (close-port port))

	 (when #$config
	   (call-with-output-file (string-append path "/.git/config")
	     (lambda (port) (display #$config port))))))
   #:additional-metadata `((remote . ,remote)
			   (general-sync? . #f))))

(use-modules (gnu packages rsync))
(define* (state-rsync path remote)
  (state-generic
   path
   #:init-gexp
   #~(lambda* (_ self)
       (let* ((meta (car (action self 'metadata)))
	      (path (assoc-ref meta 'path))
	      (remote (assoc-ref meta 'remote)))
	 (format #t "Initializing ~a.\n" self)
	 ;; TODO: revisit git clone implementation
	 (let* ((port ((@@ (guix build utils) open-pipe-with-stderr)
		       #$(file-append rsync "/bin/rsync") "-aP" remote path)))
	   (waitpid WAIT_ANY)
	   (display ((@@ (ice-9 rdelim) read-delimited) "" port))
	   (close-port port))))
   #:sync-gexp
   #~(lambda* (_ self)
       (let* ((meta (car (action self 'metadata)))
	      (path (assoc-ref meta 'path))
	      (remote (assoc-ref meta 'remote)))
	 (format #t "Synchronizing ~a.\n" self)
	 (let* ((port ((@@ (guix build utils) open-pipe-with-stderr)
		       #$(file-append rsync "/bin/rsync") "-aP" path remote)))
	   (waitpid WAIT_ANY)
	   (display ((@@ (ice-9 rdelim) read-delimited) "" port))
	   (close-port port))))
   #:additional-metadata `((remote . ,remote)
			   (general-sync? . #t))))

(define* (state-generic
	  path
	  #:key
	  (init-gexp
	   #~(lambda* (_ self)
	       (let ((path (assoc-ref (car (action self 'metadata)) 'path)))
		 (format #t "Initializing ~a.\n" self)
		 (format #t "Creating ~a directory..." path)
		 (mkdir-p path)
		 (display " done\n"))))
	  (sync-gexp
	   #~(lambda* (_ self)
	       (let ((path (assoc-ref (car (action self 'metadata)) 'path)))
		 (format #t "Synchronizing ~a.\n" self)
		 (format #t "Nothing to synchronize.\n"))))
	  (additional-metadata '((general-sync? . #f))))
  "A function which returns a shepherd-service with all required
actions for state management, should be used as a basis for other
state related items like git-state, rsync-state, etc."
  (let ((self (string->symbol
	       (format #f "state-~a" path))))
    (shepherd-service
     (documentation (format #f "Managing state at ~a." path))
     (provision (list self))
     (auto-start? #f)
     (start #~(lambda ()
		(if (car (action '#$self 'state-exists?))
		    #t
		    (begin
		      (format #t "~a is not initilized yet." '#$self)
		      #f))))
     (actions (list
	       (shepherd-action
		(name 'state-exists?)
		(documentation "Check if state file/directory exists.")
		(procedure #~(lambda* (#:rest rest)
			       (file-exists? #$path))))
	       (shepherd-action
		(name 'unchecked-init)
		(documentation "Do not use this action directly.")
		(procedure init-gexp))
	       (shepherd-action
		(name 'metadata)
		(documentation "Returns metadata related to the state.")
		(procedure #~(lambda* _
			       (append
				'((path . #$path)
				  (self . #$self))
				'#$additional-metadata))))
	       (shepherd-action
		(name 'sync)
		(documentation "Sync the state.")
		(procedure sync-gexp))
	       (shepherd-action
		(name 'init)
		(documentation "Generic initialize.")
		(procedure #~(lambda* (#:rest rest)
			       (if (car (action '#$self 'state-exists?))
				   (format #t "~a already initialized.\n" '#$self)
				   (begin
				     (action '#$self 'unchecked-init '#$self)
				     (start '#$self)))))))))))

(define (add-shepherd-services services)
  (let* ((service-names
	  (map
	   (lambda (service) (car (shepherd-service-provision service)))
	   services)))
    (append
     services
     (list
      (shepherd-service
       (documentation "Init, update and maybe destroy state.")
       (provision '(state))
       (auto-start? #t)
       (start #~(lambda ()
		  (map (lambda (name)
			 (when (car (action name 'state-exists?))
			   (start name)))
		       '#$service-names)))
       (actions (list
		 (shepherd-action
		  (name 'sync)
		  (documentation
                   "Sync all the state. Highly dependent on state type.")
		  (procedure
		   #~(lambda _
		       (map (lambda (name)
			      (when (assoc-ref (car (action name 'metadata))
					       'general-sync?)
				(action name 'sync name)))
			    '#$service-names))))
		 (shepherd-action
		  (name 'init)
		  (documentation "Initialize all the state.")
		  (procedure #~(lambda _
				 (map (lambda (name)
					(when (not (car (action name 'state-exists?)))
					  (action name 'init)
					  (start name)))
				      '#$service-names)))))))))))


(define home-state-service-type
  (service-type (name 'home-state)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        add-shepherd-services)))
                (default-value '())
		(compose concatenate)
		(extend append)
                (description "A toolset for initializing state.")))
