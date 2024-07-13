;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features sourcehut)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (feature-sourcehut))

;; https://lists.sr.ht/~abcdw/rde-devel/%3Ccover.1678315998.git.contact%40conses.eu%3E#%3C86bkgz24q5.fsf@migalmoreno.com%3E
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Bug-Reference.html
;; TODO: [Andrew Tropin, 2024-05-06] Move
;; rde-message-srht-add-email-control-code from feature-emacs-message
(define* (feature-sourcehut
          #:key
          (hut hut)
          (emacs-srht (@ (rde packages emacs-xyz) emacs-srht-latest))
          (user-name-fn (lambda (config)
                          "Get user-name RDE value."
                          (get-value 'user-name config))))
  "Configure SourceHut related packages."
  (ensure-pred file-like? hut)
  (ensure-pred file-like? emacs-srht)

  (define f-name 'sourcehut)

  (define (get-home-services config)
    (append
     (list
      (simple-service
       'add-sourcehut-packages
       home-profile-service-type
       (list hut))
      (rde-elisp-configuration-service
       'sourcehut
       config
       `((with-eval-after-load 'srht
           (setopt srht-username ,(user-name-fn config))))
       #:summary "\
SourceHut API in Emacs"
       #:commentary "\
Configures srht.el package and adds couple helpers."
       #:keywords '(convenience)
       #:elisp-packages (list emacs-srht)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
