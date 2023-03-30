;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde serializers nginx-test)
  #:use-module (guix gexp)
  #:use-module (rde serializers nginx)
  #:use-module (rde tests)
  #:use-module (rde tests store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(define (serialize-config config)
  (eval-with-store (nginx-serialize config)))

(define-test nginx-terms
  (test-group "nginx terms"
    (test-equal "number"
      "123"
      (serialize-nginx-term 123))
    (test-equal "string"
      "\"string here\""
      (serialize-nginx-term "string here"))
    (test-equal "symbol"
      "symbol-here"
      (serialize-nginx-term 'symbol-here))
    (test-assert "gexp"
      (gexp?
       (serialize-nginx-term #~"gexp")))
    (test-assert "file-like"
      (file-like?
       (serialize-nginx-term (plain-file "name" "content"))))

    (test-error "true" #t (serialize-nginx-term #t))
    (test-error "false" #t (serialize-nginx-term #f))
    (test-error "list" #t (serialize-nginx-term '(a b c)))
    (test-error "vector" #t (serialize-nginx-term #(a b c)))))

(define-test nginx-vectors
  (test-group "nginx vectors (conditions)"
    (test-assert "vector with a few terms"
      (match (serialize-nginx-vector
              `#(symbol "string" 123 ,#~(format #f "gexp")))
        (("(" "symbol" " " "\"string\"" " " "123" " " (? gexp? _) ")") #t)
        (_ #f)))

    (test-error "nested list" #t (serialize-nginx-vector #(())))
    (test-error "nested vector" #t (serialize-nginx-vector #(#())))))


(define-test nginx-contexts
  (test-group "nginx contexts"
    (test-assert "simple nested config"
      (match (serialize-nginx-context
              `((a b ((c d)))))
        (("" "a" " " "b" " {\n"
          "  " "c" " " "d" ";" "\n"
          "" "}\n") #t)
        (_ #f)))

    (test-assert "double nested config"
      (match (serialize-nginx-context
              `((a ((b ((c d)))))))
        (("" "a" " {\n"
          "  " "b" " {\n"
          "    " "c" " " "d" ";" "\n"
          "  " "}\n"
          "" "}\n") #t)
        (_ #f)))

    (test-assert "double nested config with vector"
      (match (serialize-nginx-context
              `((a ((b #() ((c)))))))
        (("" "a" " {\n"
          "  " "b" " " "(" ")" " {\n"
          "    " "c" ";" "\n"
          "  " "}\n"
          "" "}\n") #t)
        (_ #f)))))

(define-test basic-config
  (test-group "basic config"
    (test-equal "key-value pairs"
      "\
a b;
c d;
"
      (serialize-config '((a b)
                          (c d))))

    (test-equal "nested context"
      "\
a b {
  c d;
}
"
      (serialize-config '((a b ((c d))))))

    (test-equal "simple if statement"
      "\
if (a ~ b) {
  c d;
}
"
      (serialize-config '((if #(a ~ b) ((c d))))))))

(define-test gexps
  (test-group "gexps"
    (test-equal "simple gexps"
      "\
a hehe;
"
      (serialize-config
       `((a ,#~(format #f "hehe")))))

    (test-equal "simple identation of gexps"
      "\
a {
# gexp
}
"
      (serialize-config
       `((a (,#~"# gexp")))))

    (test-equal "advanced identation of gexps"
      "\
a {
  a gexp-generated value;
# unindented
  # indented again;
}
"
      (serialize-config
       `((a ((a ,#~"gexp-generated" value)
             ,#~"# unindented"
             (,#~"# indented again"))))))))

(define-test example-config
  (test-group "example config"
    (test-equal "location with nested if and empty body"
      "\
location ~* ^/if-and-alias/(?<file>.*) {
  alias /tmp/$file;
  set $true 1;
  if ($true) {
    # nothing;
  }
}
"
      (serialize-config
       `((location ~*
                   #{^/if-and-alias/(?<file>.*)}# ; guile symbol read syntax
                   ;; ,#~"^/if-and-alias/(?<file>.*)"
                   ((alias /tmp/$file)
                    (set $true 1)
                    (if #($true) ((,#~"# nothing"))))))))

    (test-equal "location with nested if 2"
      "\
location / {
  error_page 418 = @other;
  recursive_error_pages on;
  if ($something) {
    return 418;
  }
}
"
      (serialize-config
       '((location / ((error_page 418 = @other)
                      (recursive_error_pages on)
                      (if #($something)
                          ((return 418))))))))))

(define-test nginx-config-merge
  (test-group "nginx config merge"
    (test-equal "naive merge"
      '((a ((b c)))
        (d ((e f)))
        (g ((h i))))
      (nginx-merge '((a ((b c))))
                   '((d ((e f))))
                   '((g ((h i))))))

    ;; Right now nginx-merge doesn't actually merge, it just concatenates.
    ;; Fix the implementation and remove test-expect-fail.
    (test-expect-fail 1)
    (test-equal "advanced merge"
      '((a ((b c)
            (e f)
            (h i))))
      (nginx-merge '((a ((b c))))
                   '((a ((e f))))
                   '((a ((h i))))))

    (test-expect-fail 1)
    (test-equal "deep merge"
      '((a ((b ((c d)
                (e f)
                (g h)))
            (i j))))
      (nginx-merge '((a ((b ((c d))))))
                   '((a ((b ((e f))))))
                   '((a ((b ((g h)))
                         (i j))))))))

(define-test nginx-config-predicate
  (test-group "nginx config predicate"
    (test-assert "valid: simple case"
      (nginx-config? `((if #(ho) ((b c)))
                       ,#~"# heh")))

    ;; Current implementation doesn't traverse the data structure and doesn't
    ;; check elements of nginx expression.
    (test-expect-fail 1)
    (test-assert "not valid: two subcontexts"
      (not (nginx-config? '((a ((e f)) ((b c)))))))

    ;; Current implementation doesn't traverse the data structure and checks
    ;; only the top level context.
    (test-expect-fail 1)
    (test-assert "not valid: incorrect subcontext"
      (not (nginx-config? '((a (c))))))))

;; if ($http_user_agent ~ MSIE) {
;;     rewrite ^(.*)$ /msie/$1 break;
;; }

;; if ($http_cookie ~* "id=([^;]+)(?:;|$)") {
;;     set $id $1;
;; }

;; if ($request_method = POST) {
;;     return 405;
;; }

;; if ($slow) {
;;     limit_rate 10k;
;; }

;; if ($invalid_referer) {
;;     return 403;
;; }

;; location ~* ^/if-and-alias/(?<file>.*) {
;;     alias /tmp/$file;
;;     set $true 1;
;;     if ($true) {
;;         # nothing
;;     }
;; }

;; if ($args ~ post=140){
;;   rewrite ^ http://example.com/ permanent;
;; }

;; https://www.digitalocean.com/community/tutorials/understanding-the-nginx-configuration-file-structure-and-configuration-contexts
;; https://stackoverflow.com/questions/2936260/what-language-are-nginx-conf-files
;; https://www.nginx.com/blog/using-free-ssltls-certificates-from-lets-encrypt-with-nginx/
