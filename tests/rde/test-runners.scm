(define-module (rde test-runners)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)

  #:export (run-project-tests-cli))

(define test-runner-default (@@ (gider test-runners) test-runner-default))

(use-modules (gider test-runners))


;;; Run project tests

(use-modules (guix discovery)
             (rde tests))

(define (get-test-modules)
  (define this-module-file
    (canonicalize-path
     (search-path %load-path "rde/test-runners.scm")))

  (define tests-root-dir
    (dirname (dirname this-module-file)))
  ;; TODO: Reimplement or migrate to (geiser module) (a different from guix
  ;; discovery, so it doesn't find nested modules)

  ;; ((@@ (gesier modules) all-child-modules) (resolve-module '(rde)))
  (all-modules (list tests-root-dir)))

(define (run-project-tests-cli)
  (let* ((summary (test-runner-summary (run-project-tests
                                        #:test-modules (get-test-modules))))
         (fail-count (assoc-ref summary 'fail)))
    (exit (zero? fail-count))))
