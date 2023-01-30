(define-module (rde test-runners)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)

  #:export (run-project-tests))

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
        (format #t "Source: ~a\npass = ~a, xfail = ~a, fail = ~a\n\n"
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

(define (run-tests-for-module module)
  (let ((test-name (format #f "module ~a" (module-name module))))
    (test-begin test-name)
    (reload-module module)
    (test-end test-name)))

(define (run-tests-for-file file)
  (primitive-load file))

(define (discover-tests directory)
  #| Return a list of paths to testing scripts in the given directory.

  Two kinds of files are included:

  + Any ".scm" file suffixed "-test".

  DIRECTORY (string)
    Absolute path to a directory containing a test suite.

  RETURN VALUE (list of string)
    A list where every element represents a path to a testing
    script. |#
  ;; Enter to any directory.
  (define (enter? name stat result) #true)

  (define (leaf name stat result)
    (if (string-suffix? "-test.scm" (basename name))
        (cons name result)
        result))

  ;; Don't append directories to the resulting list.
  (define (down name stat result) result)
  (define (up name stat result) result)

  ;; Likewise for skipped directories.
  (define (skip name stat result) result)

  ;; Ignore unreadable files/directories but warn the user.
  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: ~a~%"
            name (strerror errno))
    result)

  (file-system-fold enter? leaf down up skip error (list) directory))

(define (submodules module)
  (hash-map->list (lambda (k v) v) (module-submodules module)))

(define (run-project-tests)
  (test-runner-factory test-runner-default)
  ;; (test-runner-current (test-runner-create))
  ;; ;; (test-runner-current)

  (test-begin "PROJECT TESTS")
  (define this-module-file
    (canonicalize-path
     (search-path %load-path
                  (module-filename
                   (resolve-module '(rde test-runners))))))
  (define test-files
    (discover-tests (dirname this-module-file)))

  (map primitive-load test-files)

  (define fail-count (test-runner-fail-count (test-runner-current)))
  (test-end "PROJECT TESTS")
  (exit (zero? fail-count)))

;; https://www.mail-archive.com/geiser-users%40nongnu.org/msg00323.html
;; https://rednosehacker.com/revisiting-guile-xunit

;; Test runners:
;; https://github.com/aconchillo/guile-json/blob/master/tests/runner.scm
;; https://luis-felipe.gitlab.io/guile-proba/
;; https://git.systemreboot.net/run64/tree/bin/run64
;; https://framagit.org/Jeko/guile-spec

;; Common lisp testing frameworks:
;; https://sabracrolleton.github.io/testing-framework

;; (define-test our-super-test-suite
;;   (test-group "Something"
;;     (few asserts)))

;; (run-tests
;;  test-runner
;;  ;; (select-all-loaded-tests)
;;  (select-only-every-second-loaded-test))
