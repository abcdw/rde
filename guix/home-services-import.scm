(define-module (guix home-services-import)
  #:use-module (gnu home-services-utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (modules+configurations))
            

;;; Commentary:
;;;
;;; This module provides utilities for generating home service
;;; configurations from existing "dotfiles".
;;;
;;; Code:


(define (generate-bash-module+configuration)
  (let ((rc (string-append (getenv "HOME") "/.bashrc"))
        (profile (string-append (getenv "HOME") "/.bash_profile"))
        (logout (string-append (getenv "HOME") "/.bash_logout")))
    `((gnu home-services bash)
      (service home-bash-service-type
                 (home-bash-configuration
                  ,@(optional (file-exists? rc)
                              `((bashrc
                                 (list (slurp-file-gexp (local-file ,rc))))))
                  ,@(optional (file-exists? profile)
                              `((bash-profile
                                 (list (slurp-file-gexp
                                        (local-file ,profile))))))
                  ,@(optional (file-exists? logout)
                              `((bash-logout
                                 (list (slurp-file-gexp
                                        (local-file ,logout)))))))))))


(define %files-configurations-alist
  `((".bashrc" . ,generate-bash-module+configuration)
    (".bash_profile" . ,generate-bash-module+configuration)
    (".bash_logout" . ,generate-bash-module+configuration)))

(define (modules+configurations)
  (let ((configurations (delete-duplicates
                         (filter-map (match-lambda
                                ((file . proc)
                                 (if (file-exists?
                                      (string-append (getenv "HOME") "/" file))
                                     proc
                                     #f)))
                                     %files-configurations-alist)
                         (lambda (x y)
                           (equal? (procedure-name x) (procedure-name y))))))
    (map (lambda (proc) (proc)) configurations)))
                             
