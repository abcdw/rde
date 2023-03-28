;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Moreno <mmoreno@mmoreno.eu>
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

(define-module (rde features nyxt-xyz)
  #:use-module (rde features)
  #:use-module (rde features web-browsers)
  #:use-module (rde home services web-browsers)
  #:use-module (rde serializers lisp)
  #:use-module (gnu home services)
  #:export (feature-nyxt-userscript))

(define (maybe-lisp-config? x)
  (or (lisp-config? x) (not x)))

(define* (feature-nyxt-userscript
          #:key
          (userscripts #f)
          (userstyles #f))
  "Configure Nyxt's userscript-mode to add custom USERSCRIPTS and USERSTYLES.
See @uref{nyxt:manual#user-scripts} inside Nyxt to learn more on how to
construct these."
  (ensure-pred maybe-lisp-config? userscripts)
  (ensure-pred maybe-lisp-config? userstyles)

  (define nyxt-f-name 'userscript)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))
  (define nyxt-config-name (symbol-append 'rde- nyxt-f-name))
  (define nyxt-service-type-name (symbol-append 'nyxt- nyxt-config-name))

  (define nyxt-rde-userscript-service-type
    (make-nyxt-service-type nyxt-service-type-name))

  (define (get-home-services config)
    "Return home services related to userscript-mode."
    (list
     (service
      nyxt-rde-userscript-service-type
      (home-nyxt-lisp-configuration
       (name nyxt-config-name)
       (config
        `((define-configuration web-buffer
            ((default-modes `(nyxt/user-script-mode:user-script-mode
                              ,@%slot-value%))))
          (define-configuration nyxt/user-script-mode:user-script-mode
            (,@(if userstyles
                   `((nyxt/user-script-mode:user-styles
                      (list ,@userstyles)))
                   '())
             ,@(if userscripts
                   `((nyxt/user-script-mode:user-scripts
                      (list ,@userscripts)))
                   '())))))))))

  (feature
   (name f-name)
   (values
    `((,f-name . #t)
      (nyxt-rde-userscript-service-type . ,nyxt-rde-userscript-service-type)))
   (home-services-getter get-home-services)))
