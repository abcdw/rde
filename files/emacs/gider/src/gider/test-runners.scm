(define-module (gider test-runners)
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

(define (string-repeat s n)
  "Returns string S repeated N times."
  (fold
   (lambda (_ str)
     (string-append str s))
   ""
   (iota n)))

(define (test-runner-default)
  (let ((runner (test-runner-null)))
    (test-runner-on-group-begin! runner
      (lambda (runner name count)
        (format #t "~a> ~a\n"
                (string-repeat "-" (length (test-runner-group-stack runner)))
                name)))
    (test-runner-on-group-end! runner
      (lambda (runner)
        (format #t "<~a ~a\n"
                (string-repeat "-"
                               (1- (length (test-runner-group-stack runner))))
                (car (test-runner-group-stack runner)))))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (format #t "[~a] ~a\n"
                (test-result-ref runner 'result-kind)
                (if (test-result-ref runner 'test-name)
                    (test-runner-test-name runner)
                    "<>"))
        (case (test-result-kind runner)
          ((fail)
           (if (test-result-ref runner 'expected-value)
               (format #t "~a:~a\n -> expected: ~s\n -> obtained: ~s\n\n"
                       (test-result-ref runner 'source-file)
                       (test-result-ref runner 'source-line)
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value))))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format #t "Source: ~a\nAsserts: pass = ~a, xfail = ~a, fail = ~a\n\n"
                (test-result-ref runner 'source-file)
                (test-runner-pass-count runner)
                (test-runner-xfail-count runner)
                (test-runner-fail-count runner))))
    runner))


;;; run64
;; https://git.systemreboot.net/run64/tree/bin/run64

(define (color code str)
  (string-append (string #\esc)
                 "["
                 (number->string code)
                 "m"
                 str
                 (string #\esc)
                 "[0m"))

(define (bold str)
  (color 1 str))

(define (red str)
  (color 31 str))

(define (green str)
  (color 32 str))

(define (yellow str)
  (color 33 str))

(define (magenta str)
  (color 35 str))

(define (headline text color)
  "Display headline TEXT in COLOR. COLOR is a function that wraps a
given string in an ANSI escape code."
  (display (color (string-append "==== " text)))
  (newline))

(define (run64-report runner)
  (unless (zero? (test-runner-fail-count runner))
    (headline "FAILURES" red)
    (for-each (lambda (failure)
                (let ((name (assq-ref failure 'test-name))
                      (file (assq-ref failure 'source-file))
                      (line (assq-ref failure 'source-line)))
                  (when file
                    (display file)
                    (display ":")
                    (when line
                      (display line)
                      (display ":"))
                    (display " "))
                  (display name)
                  (newline)))
              (test-runner-aux-value runner))
    (newline))
  (headline
   (string-join
    (filter-map (lambda (count text color)
                  (if (zero? count)
                      #f
                      (color (string-append (number->string count)
                                            " " text))))
                (list (test-runner-pass-count runner)
                      (test-runner-fail-count runner)
                      (test-runner-xpass-count runner)
                      (test-runner-xfail-count runner)
                      (test-runner-skip-count runner))
                (list "passed" "failed"
                      "unexpected passes"
                      "expected failures"
                      "skipped")
                (list green red yellow yellow yellow))
    ", ")
   (cond
    ((not (zero? (test-runner-fail-count runner)))
     red)
    ((or (not (zero? (test-runner-xpass-count runner)))
         (not (zero? (test-runner-xfail-count runner))))
     yellow)
    (else green))))

(define (test-runner-run64)
  (let ((runner (test-runner-null)))

    (test-runner-on-group-begin! runner
      (lambda (runner suite-name count)
        (when (null? (test-runner-group-stack runner))
          (headline "test session starts" bold))
        (display suite-name)
        (display " ")))
    (test-runner-on-group-end! runner
      (lambda _
        (newline)))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (let ((name (test-runner-test-name runner))
              (result (string-upcase
                       (symbol->string (test-result-kind runner))))
              (result-alist (test-result-alist runner)))
          (display (case (test-result-kind runner)
                     ((pass) (green "."))
                     ((fail) (red "F"))
                     ((xfail xpass) (yellow "X"))
                     ((skip) (yellow "S"))))
          (when (eq? (test-result-kind runner)
                     'fail)
            ;; Prepend test failure details to aux value.
            (test-runner-aux-value! runner
                                    (cons (cons (cons 'test-name (test-runner-test-name runner))
                                                (test-result-alist runner))
                                          (test-runner-aux-value runner)))))))
    ;; Initialize aux value to the empty list.
    (test-runner-on-final! runner (lambda (runner) (run64-report runner)))
    (test-runner-aux-value! runner '())
    runner))


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
  ;; TODO: Load test module if it's not loaded yet.
  ;; (reload-module module)
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
  ;; TODO: Reimplement or migrate to (geiser module) (a different from guix
  ;; discovery, so it doesn't find nested modules)

  ;; ((@@ (gesier modules) all-child-modules) (resolve-module '(rde)))
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

;; (set! %previous-runner (run-project-tests))
;; (rerun-tests %previous-runner
;;              #:filter-fn (lambda (x) (equal? 'fail (assoc-ref x 'status))))

;; (module-clear! (resolve-module '(rde serializers nginx-test)))
;; (get-module-tests (resolve-module '(rde serializers nginx-test)))

;; (run-module-tests (resolve-module '(rde serializers nginx-test)))

;; (use-modules (ice-9 pretty-print))

;; (rerun-tests %previous-runner)
;; (define prev-runner (test-runner-default))

;; (let ((runner %previous-runner))
;;   (run-module-tests
;;    (resolve-module '(rde serializers nginx-test))
;;    #:runner runner)
;;   (pretty-print (test-runner-test-results runner)))

;; (re-run-failed-tests prev-runner)

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

;; TODO:
;; - Make test-assert to show line, where it fails.
;; - Implement test-match, which uses ice-9 match like patterns and provides
;;   meaningful report.

;; - Write ADR for serializers or/and implement template/interface for serializers.
