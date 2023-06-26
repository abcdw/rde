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

(define-module (rde home services ocaml)
  #:use-module (gnu home services)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (home-ocaml-configuration
            home-ocaml-service-type))

(define-configuration/no-serialization home-ocaml-configuration
  (ocaml
   (file-like ocaml)
   "The @code{ocaml} package to use.")
  (config
   (list '())
   "List of strings that make up a @file{.ocamlinit} configuration."))

(define (home-ocaml-files-service config)
  `(("ocaml/init.ml"
     ,(mixed-text-file
       "init-ml"
       #~(string-append
          #$@(interpose (home-ocaml-configuration-config config)
                        "\n" 'suffix))))))

(define (home-ocaml-profile-service config)
  (list (home-ocaml-configuration-ocaml config)))

(define home-ocaml-service-type
  (service-type
   (name 'home-ocaml)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-ocaml-profile-service)
     (service-extension
      home-xdg-configuration-files-service-type
      home-ocaml-files-service)))
   (default-value (home-ocaml-configuration))
   (description "Home service for the OCaml programming language.")))

(define (generate-home-ocaml-documentation)
  (generate-documentation
   `((home-ocaml-configuration
      ,home-ocaml-configuration-fields))
   'home-ocaml-configuration))
