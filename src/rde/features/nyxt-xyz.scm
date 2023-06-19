;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
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
  #:use-module (rde features predicates)
  #:use-module (rde features web-browsers)
  #:use-module (rde home services web-browsers)
  #:use-module (rde serializers lisp)
  #:use-module (gnu home services)
  #:export (feature-nyxt-blocker
            feature-nyxt-nx-search-engines
            feature-nyxt-userscript))

(define* (feature-nyxt-blocker
          #:key
          (blocked-hosts '()))
  "Configure Nyxt's blocker mode."
  (ensure-pred list-of-strings? blocked-hosts)

  (define nyxt-f-name 'blocker)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))
  (define nyxt-config-name (symbol-append 'rde- nyxt-f-name))
  (define nyxt-service-type-name (symbol-append 'nyxt- nyxt-config-name))

  (define nyxt-rde-blocker-service-type
    (make-nyxt-service-type nyxt-service-type-name))

  (define (get-home-services config)
    "Return home services related to blocker-mode."
    (list
     (service
      nyxt-rde-blocker-service-type
      (home-nyxt-lisp-configuration
       (name nyxt-config-name)
       (config
        `((define-configuration web-buffer
            ((default-modes `(nyxt/blocker-mode:blocker-mode
                              ,@%slot-value%))))
          (define-configuration nyxt/blocker-mode:blocker-mode
            ((nyxt/blocker-mode:hostlists
              (append
               (list
                (nyxt/blocker-mode:make-hostlist
                 :hosts ',blocked-hosts))
               (list %slot-default%)))))))))))

  (feature
   (name f-name)
   (values
    `((,f-name . #t)
      (nyxt-rde-blocker-service-type . ,nyxt-rde-blocker-service-type)))
   (home-services-getter get-home-services)))

(define (maybe-lisp-config? x)
  (or (lisp-config? x) (not x)))

(define (%rde-nx-search-engines config)
  `((engines:wordnet
     :shortcut "wn"
     :show-examples t
     :show-word-frequencies t
     :show-sense-numbers t)
    (engines:github
     :shortcut "gh"
     :object :advanced)
    (engines:startpage
     :shortcut "sp")
    (engines:sourcehut
     :shortcut "sh")
    (engines:libgen
     :shortcut "lg"
     :covers t
     :results 100
     :object :files
     :fallback-url (quri:uri "http://libgen.gs")
     :base-search-url "https://libgen.gs/index.php?req=~a")
    (engines:lemmy
     :shortcut "le")
    (engines:discourse
     :shortcut "ae")
    (engines:meetup
     :shortcut "me")
    (engines:gitea
     :shortcut "gi")
    (engines:gitea-users
     :shortcut "giu")
    (engines:hacker-news
     :shortcut "hn"
     :fallback-url (quri:uri "https://news.ycombinator.com")
     :search-type :all)
    (engines:lobsters
     :shortcut "lo")
    (engines:google
     :shortcut "go"
     :safe-search nil
     :lang-ui :english
     :results-number 50
     :new-window t)
    (engines:reddit
     :shortcut "re")
    (engines:teddit
     :shortcut "re")
    (engines:whoogle
     :shortcut "who"
     :theme :system
     :alternatives nil
     :lang-results :english
     :lang-ui :english
     :view-image t
     :no-javascript t
     :new-tab t)))

(define* (feature-nyxt-nx-search-engines
          #:key
          (engines %rde-nx-search-engines)
          (default-engine-shortcut #f)
          (extra-engines #f)
          (auto-complete? #f)
          (auto-complete-non-prefix? #f))
  "Configure nx-search-engines, a collection of easy-to-setup
search engines for Nyxt.

You can pass additional search engines via EXTRA-ENGINES, a
single argument procedure that takes the current rde configuration
and returns Lisp configuration containing the engines."
  (ensure-pred maybe-procedure? engines)
  (ensure-pred maybe-string? default-engine-shortcut)
  (ensure-pred maybe-procedure? extra-engines)
  (ensure-pred boolean? auto-complete?)
  (ensure-pred boolean? auto-complete-non-prefix?)

  (define nyxt-f-name 'nx-search-engines)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))
  (define nyxt-config-name (symbol-append 'rde- nyxt-f-name))
  (define nyxt-service-type-name (symbol-append 'nyxt- nyxt-config-name))

  (define nyxt-rde-nx-search-engines-service-type
    (make-nyxt-service-type nyxt-service-type-name))

  (define (get-home-services config)
    "Return home services related to nx-search-engines."
    (list
     (service
      nyxt-rde-nx-search-engines-service-type
      (home-nyxt-lisp-configuration
       (name nyxt-config-name)
       (config
        `((define-configuration context-buffer
            ((search-auto-complete-p ,(if auto-complete? 't 'nil))
             (search-always-auto-complete-p
              ,(if auto-complete-non-prefix? 't 'nil))
             (search-engines
              (append
               %slot-value%
               ,(if extra-engines
                    `(list ,@(extra-engines config))
                    '())
               ,@(if default-engine-shortcut
                     `((remove ,default-engine-shortcut
                               (list ,@(engines config)
                                     ,@(if extra-engines
                                          (extra-engines config)
                                          '()))
                               :key 'shortcut :test 'string=)
                       (list
                        (find ,default-engine-shortcut
                              (list ,@(if extra-engines
                                          (extra-engines config)
                                          '())
                                    ,@(engines config))
                              :key 'shortcut :test 'string=)))
                     `(list ,@(engines config)))))))))
       (lisp-packages '(nx-search-engines))))))

  (feature
   (name f-name)
   (values
    `((,f-name . #t)
      (nyxt-rde-nx-search-engines-service-type
       . ,nyxt-rde-nx-search-engines-service-type)))
   (home-services-getter get-home-services)))

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
