(define-module (gnu home-services symlink-manager)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (home-symlink-manager-service-type))

(define (update-symlinks-script)
  (program-file
   "update-symlinks"
   #~(begin
       (use-modules (ice-9 ftw)
		    (ice-9 curried-definitions)
		    (ice-9 match)
 		    (ice-9 pretty-print)
		    (srfi srfi-1))
       (define ((simplify-file-tree parent) file)
	 (match file
	   ((name stat) `(file . ,(string-append parent name)))
	   ((name stat children ...)
	    (cons `(dir . ,(string-append parent name))
		  (map (simplify-file-tree
			(if (equal? name ".")
			    ""
			    (string-append parent name "/")))
		       children)))))

       (define ((file-tree-traverse preordering) node)
	 "Traverses the file tree in different orders, depending on PREORDERING.

if PREORDERING is @code{#t} resulting list will contain folders before
files located in those folders, otherwise folders will appear only
after all nested items already listed."
	 (let ((prepend (lambda (a b) (append b a))))
	   (match node
	     (('file . path) (list node))
	     ((('dir . path) . rest)
	      ((if preordering append prepend)
	       (list (cons 'dir path))
	       (append-map (file-tree-traverse preordering) rest))))))

       (define (save-tree tree path)
	 (call-with-output-file path
	   (lambda (port)
	     (display ";; Don't touch this file, it used for proper cleanup on
;; guix home reconfigure.\n" port)
	     (write tree port))))

       (define (load-tree path)
	 (if (file-exists? path)
	     (call-with-input-file path
	       (lambda (port)
		 (read port)))
	     #f))

       (use-modules (guix build utils))
       (let* ((tree-file-name "/.guix-home-environment-file-tree")
	      (config-home    (or (getenv "XDG_CONFIG_HOME")
				  (string-append (getenv "HOME") "/.config")))
	      (tree-file-path (string-append config-home tree-file-name))

	      (he-path (or (getenv "GUIX_HOME_ENVIRONMENT_DIRECTORY")
			   (string-append (getenv "HOME") "/.guix-home-environment")))
	      (files-path (string-append he-path "/files"))
	      ;; Leading dot is required, because files itself is symlink and
	      ;; to make file-system-tree works it should be a directory.
	      (files-dir-path (string-append files-path "/."))

	      (home-path (getenv "HOME"))
	      (backup-dir (string-append home-path "/"
					 (number->string (current-time))
					 "-guix-home-legacy-configs-backup"))

	      (old-tree (load-tree tree-file-path))
	      (new-tree ((simplify-file-tree "")
			 (file-system-tree files-dir-path)))

	      (get-source-path
	       (lambda (path)
		 ;; REVIEW: Do we need to create symlink to object in the
		 ;; store or it's better to have symlinks pointing to
		 ;; ~/.guix-home-enironment/files/... ?
		 (readlink (string-append files-path "/" path))))

	      (get-target-path
	       (lambda (path)
		 (string-append home-path "/." path)))

	      (get-backup-path
	       (lambda (path)
		 (string-append backup-dir "/." path)))

	      (directory?
	       (lambda (path)
		 (equal? (stat:type (stat path)) 'directory)))

	      (empty-directory?
	       (lambda (dir)
		 (equal? (scandir dir) '("." ".."))))

	      (backup-file
	       (lambda (path)
		 (mkdir-p backup-dir)
		 (format #t "Backing up ~a..." (get-target-path path))
		 (mkdir-p (dirname (get-backup-path path)))
		 (rename-file (get-target-path path) (get-backup-path path))
		 (display " done\n")))

	      (cleanup-symlinks
	       (lambda ()
		 (let ((to-delete ((file-tree-traverse #f) old-tree)))
		   (display
		    "Cleaning up symlinks from previous home-environment.\n\n")
		   (map
		    (match-lambda
		      (('dir . ".")
		       (display "Cleanup finished.\n\n"))

		      (('dir . path)
		       (if (and
			    (file-exists? (get-target-path path))
			    (directory? (get-target-path path))
			    (empty-directory? (get-target-path path)))
			   (begin
			     (format #t "Removing ~a..." (get-target-path path))
			     (rmdir (get-target-path path))
			     (display " done\n"))
			   (format #t "Skipping ~a... done\n"
				   (get-target-path path))))

		      (('file . path)
		       (when (file-exists? (get-target-path path))
			 (format #t "Removing ~a..." (get-target-path path))
			 (delete-file (get-target-path path))
			 (display " done\n"))))
		    to-delete))))

	      (create-symlinks
	       (lambda ()
		 (let ((to-create ((file-tree-traverse #t) new-tree)))
		   (map
		    (match-lambda
		      (('dir . ".")
		       (display
			"New symlinks to home-environment will be created soon.\n")
		       (format
			#t "All conflicting files will go to ~a.\n\n" backup-dir))

		      (('dir . path)
		       (let ((target-path (get-target-path path)))
			 (when (and (file-exists? target-path)
				    (not (directory? target-path)))
			   (backup-file path))

			 (if (file-exists? target-path)
			     (format #t "Skipping creation of directory ~a... done\n"
				     target-path)
			     (begin
			       (format #t "Creating directory ~a..." target-path)
			       (mkdir target-path)
			       (display " done\n")))))

		      (('file . path)
		       (when (file-exists? (get-target-path path))
			 (backup-file path))
		       (format #t "Symlinking ~a -> ~a..."
			       (get-target-path path) (get-source-path path))
		       (symlink (get-source-path path) (get-target-path path))
		       (display " done\n")))
		    to-create)))))

	 (when old-tree
	   (cleanup-symlinks))

	 (create-symlinks)

	 (format #t "Persisting used file-tree to ~a..." tree-file-path)
	 (save-tree new-tree tree-file-path)
	 (display " done\nFinished updating symlinks.\n\n")))))


(define (update-symlinks-gexp _)
  #~(primitive-load #$(update-symlinks-script)))

(define home-symlink-manager-service-type
  (service-type (name 'home-symlink-manager)
                (extensions
                 (list
		  (service-extension
		   home-run-on-reconfigure-service-type
                   update-symlinks-gexp)))
		(default-value #f)
                (description "Provides create-symlinks and
remove-symlinks scripts.")))
