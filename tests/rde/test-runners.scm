(define-module (rde test-runners)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)

  #:export (%previous-runner
            get-test-module

            rerun-tests
            run-test
            run-module-tests
            run-project-tests
            run-project-tests-cli

            test-runner-summary))

(define test-runner-default (@@ (gider test-runners) test-runner-default))


;;; Run project tests

(define (get-test-module)
  ;; TODO: Handle the case, when test module doesn't exist.
  "Return a test module related to the current one.  Usually it's a module with
-test prefix.  Return current module if it already contains -test prefix."
  (let* ((m-name (module-name (current-module)))
         (m-tail (last m-name))
         (test-m-tail
          (if (string-suffix? "-test" (symbol->string m-tail))
              m-tail
              (symbol-append m-tail '-test))))
    (resolve-module
     (append
      (drop-right m-name 1)
      (list test-m-tail)))))

(test-runner-factory test-runner-default)

(use-modules (guix discovery)
             (rde tests))

(define (test-runner-summary runner)
  "Return alist of helpful statistics for the test-runner RUNNER."
  `((pass . ,(test-runner-pass-count runner))
    (xfail . ,(test-runner-xfail-count runner))
    (xpass . ,(test-runner-xpass-count runner))
    (fail . ,(test-runner-fail-count runner))))

(define (test-runner-test-results-stack runner)
  (or (assoc-ref (test-runner-aux-value runner) 'test-results-stack)
      '()))

(define (test-runner-test-results runner)
  (reverse (test-runner-test-results-stack runner)))

(define (record-test-run-result runner old-fail-count t)
  (let* ((aux-value (test-runner-aux-value runner))
         (test-result (if (< old-fail-count (test-runner-fail-count runner))
                          `((test . ,t) (status . fail))
                          `((test . ,t) (status . pass))))
         (test-results-stack (test-runner-test-results-stack runner))
         (new-test-results (cons test-result test-results-stack)))
    (test-runner-aux-value! runner
     (assoc-set! aux-value 'test-results-stack new-test-results))))

(define* (run-test
          t
          #:key (runner (test-runner-create)))
  (let ((old-fail-count (test-runner-fail-count runner)))
    (test-with-runner runner
      (t)
      (record-test-run-result runner old-fail-count t)))
  runner)

(define (get-module-tests module)
  (fold-module-public-variables
   (lambda (variable acc)
     (if (test? variable) (cons variable acc) acc))
   '()
   (list module)))

(define* (run-module-tests
          module
          #:key (runner (test-runner-create)))
  (define module-tests (get-module-tests module))
  (test-with-runner runner
    (let ((test-name (format #f "module ~a" (module-name module))))
      (test-group test-name
        (map (lambda (t) (run-test t #:runner runner)) module-tests))))
  runner)

(define (get-test-modules)
  (define this-module-file
    (canonicalize-path
     (search-path %load-path "rde/test-runners.scm")))

  (define tests-root-dir
    (dirname (dirname this-module-file)))
  (all-modules (list tests-root-dir)))

;; (test-runner-current #f)
(define* (run-project-tests
          #:key (runner (test-runner-create)))
  (define test-modules (get-test-modules))
  (test-with-runner runner
    (test-group "PROJECT TEST"
      (map (lambda (m)
             (let ((module-tests (get-module-tests m)))
               (when (not (null? module-tests))
                 (run-module-tests m #:runner runner))))
           test-modules)))
  runner)

(define (run-project-tests-cli)
  (let* ((summary (test-runner-summary (run-project-tests)))
         (fail-count (assoc-ref summary 'fail)))
    (exit (zero? fail-count))))

(define* (rerun-tests
          previous-runner
          #:key
          (runner (test-runner-create))
          (filter-fn (const #t)))
  (when previous-runner
    (let* ((test-results (test-runner-test-results previous-runner))
           (get-test (lambda (x) (assoc-ref x 'test)))
           (filtered-tests (map get-test (filter filter-fn test-results))))
      (test-with-runner runner
        (test-group "RERUN TESTS"
          (map (lambda (t) (run-test t #:runner runner)) filtered-tests)))))
  runner)

;; https://www.mail-archive.com/geiser-users%40nongnu.org/msg00323.html
;; https://rednosehacker.com/revisiting-guile-xunit

;; Test runners:
;; https://github.com/aconchillo/guile-json/blob/master/tests/runner.scm
;; https://luis-felipe.gitlab.io/guile-proba/
;; https://git.systemreboot.net/run64/tree/bin/run64
;; https://framagit.org/Jeko/guile-spec

;; Common lisp testing frameworks:
;; https://sabracrolleton.github.io/testing-framework

;; Clojure testing libraries:
;; https://jakemccrary.com/blog/2014/06/22/comparing-clojure-testing-libraries-output/

;; Scheme testing libraries:
;; https://github.com/tali713/mit-scheme/blob/master/tests/unit-testing.scm
;; https://code.call-cc.org/svn/chicken-eggs/release/5/test/trunk/test.scm

;; (define-test our-super-test-suite
;;   (test-group "Something"
;;     (few asserts)))

;; (run-tests
;;  test-runner
;;  ;; (select-all-loaded-tests)
;;  (select-only-every-second-loaded-test))
