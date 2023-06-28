;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
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

(define-module (rde serializers yaml-test)
  #:use-module (guix gexp)
  #:use-module (rde serializers yaml)
  #:use-module (rde tests)
  #:use-module (rde tests store)
  #:use-module (ice-9 match))

(define (serialize-yaml config)
  (eval-with-store (yaml-serialize config)))

(define-test yaml-terms
  (test-group "YAML basic values"
    (test-equal "number"
      "123"
      (serialize-yaml-term 123))
    (test-equal "string"
      "\"string here\""
      (serialize-yaml-term "string here"))
    (test-equal "symbol"
      "symbol_here"
      (serialize-yaml-term 'symbol_here))
    (test-assert "gexp"
      (gexp?
       (serialize-yaml-term #~"gexp")))
    (test-equal "true" "true" (serialize-yaml-term #t))
    (test-equal "false" "false" (serialize-yaml-term #f))
    (test-error "list" #t (serialize-yaml-term '(a b c)))
    (test-error "vector" #t (serialize-yaml-term #(a b c)))))

(define-test yaml-lists
  (test-group "YAML lists"
    (test-equal "basic list"
      "[a, b, c]"
      (serialize-yaml '(a b c))))
  (test-group "YAML alists"
    (test-equal "basic alist"
      "a: b"
      (serialize-yaml '((a . b))))
    (test-equal "nested alist"
      "logging: \

  print_level: debug"
      (serialize-yaml '((logging . ((print_level . debug))))))
    (test-error "invalid key" #t (serialize-yaml-config '((1 . test))))))

(define-test yaml-vectors
  (test-group "YAML vectors"
    (test-equal "basic vector"
      "
- a
- b
- c"
      (serialize-yaml #(a b c)))
    (test-equal "nested alist"
      "
- names: [client, federation]
  compress: false"
      (serialize-yaml #(((names . (client federation))
                         (compress . #f)))))
    (test-error "nested list" #t (serialize-yaml #(())))))

(define-test yaml-example-config
  (test-group "example config"
    (test-equal "full length example"
      "\
server_name: \"matrix.org\"
public_base_url: \"https://matrix.org\"
media_store_path: \"/var/lib/matrix-synapse/media_store\"
max_upload_size: \"50M\"
enable_registration: false
report_stats: true
database: \

  name: psycopg2
  allow_unsafe_locale: true
  args: \

    user: \"matrix-synapse\"
    database: \"matrix-synapse\"
    host: localhost
    port: 5432
    cp_min: 5
    cp_max: 10
listeners: \

  - port: 8008
    tls: false
    type: http
    x_forwarded: true
    bind_addresses: \

      - \"::1\"
      - \"127.0.0.1\"
    resources: \

      - names: [client, federation]
        compress: false
trusted_key_servers: \

  - server_name: \"matrix.org\""
      (serialize-yaml
       '((server_name . "matrix.org")
         (public_base_url . "https://matrix.org")
         (media_store_path . "/var/lib/matrix-synapse/media_store")
         (max_upload_size . "50M")
         (enable_registration . #f)
         (report_stats . #t)
         (database . ((name . psycopg2)
                      (allow_unsafe_locale . #t)
                      (args . ((user . "matrix-synapse")
                               (database . "matrix-synapse")
                               (host . localhost)
                               (port . 5432)
                               (cp_min . 5)
                               (cp_max . 10)))))
         (listeners . #(((port . 8008)
                         (tls . #f)
                         (type . http)
                         (x_forwarded . true)
                         (bind_addresses . #("::1" "127.0.0.1"))
                         (resources . #(((names . (client federation))
                                         (compress . #f)))))))
         (trusted_key_servers . #(((server_name . "matrix.org")))))))))
