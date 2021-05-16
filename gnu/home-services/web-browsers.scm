(define-module (gnu home-services web-browsers)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services files)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages gnuzilla)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (home-icecat-service-type
            home-icecat-configuration
            icecat-profile))

;;; Commentary:
;;;
;;; Web browser related services.
;;;
;;; Code:


;;;
;;; IceCat.
;;;
;;; (home-icecat-configuration
;;;    (profiles
;;;     (list (icecat-profile
;;;            (default? #t)
;;;            (name "default")
;;;            (id 0)
;;;            (settings '((browser.urlbar.shortcuts.history . #t)
;;;                        (browser.fullscreen.autohide . #t)))
;;;            (user-chrome "\
;;; #TabsToolbar { visibility: collapse !important; }")
;;;            (user-content "\
;;; :root{ scrollbar-width: none !important; }"))
;;;           (icecat-profile
;;;            (default? #f)
;;;            (name "github")
;;;            (id 1)
;;;            (settings '((browser.urlbar.shortcuts.bookmarks . #f)
;;;                        (browser.fullscreen.autohide . #t)))))))
;;;

(define %icecat-config-path "mozilla/icecat")
(define (listof-icecat-profile? val)
  (listof icecat-profile?))

(define-configuration icecat-profile
  (default?
    (boolean #f)
    "Whether or not this is the default profile for IceCat.")
  (name
   (string)
   "The name of this IceCat profile.  This name will show up in the
@uref{about:profiles} page, and it has to be unique for each
@code{icecat-profile}.")
  (id
   (integer)
   "The profile id assigned to this profile.  This has to be unique to
each @code{icecat-profile}.")
  (settings
   (alist '())
   "An association list of options to set in this profile.  These are
options you can see in the @uref{about:config} page.  The key of the
pair can be a string or a symbol, and the value can be a string,
symbol, boolean, or number.")
  (user-chrome
   (string "")
   "Custom user chrome CSS.  See @uref{https://www.userchrome.org/} for
details on how to customize the look and feel of IceCat.")
  (user-content
   (string "")
   "Custom user content CSS.")
  (no-serialization))

;; TODO: Extensions.
(define-configuration home-icecat-configuration
  (package
    (package icecat)
    "The IceCat package to use.")
  (profiles
   (listof-icecat-profile '())
   "List of IceCat profiles.")
  (no-serialization))

(define (icecat-profile-service config)
  (list (home-icecat-configuration-package config)))

(define (icecat-files-service config)
  (define (serialize-field key val)
    (let ((primitive-val-str (cond
                              ((boolean? val) (boolean->true-or-false val))
                              ((string? val) (format #f "~s" val))
                              ((number? val) (format #f "~a" val))
                              (else #f))))
      (append (list (format #f "user_pref(\"~a\", " key))
              (if primitive-val-str
                  (list primitive-val-str)
                  (list "\"" val "\""))
              (list ");\n"))))

  (define (serialize-settings settings)
    #~(apply string-append
             '#$(generic-serialize-alist append serialize-field settings)))

  (define (check-duplicate-field field-name fields)
    (fold (lambda (field acc)
            (if (null? acc)
                (cons field acc)
                (if (equal? field (car acc))
                    (raise (formatted-message
                            (G_ "`home-icecat-configuration' cannot contain \
`icecat-profile's with duplicate ~as: `~a'")
                            field-name head))
                    (cons field acc))))
          '()
          (sort (map maybe-object->string fields) string<)))

  (define (check-only-one-default defaults)
    (if (= (length (filter identity defaults)) 1)
        #t
        (raise (formatted-message
                (G_ "Only one `icecat-profile' in `home-icecat-configuration' can \
be the default profile.")))))

  ;; Return a list where the first element is a list of alists
  ;; representing the content of profiles.ini, and the rest of the
  ;; elements is a containing the content of the config files
  ;; (user.js, userChrome.css, userContent.css)
  (define (serialize-profiles profiles)
    (match profiles
      (($ <icecat-profile> location default? name id
                           settings user-chrome user-content)
       (let ((profile-path (if default? "default" name))
             (file-name (string-append "icecat-" name "-profile")))
         (filter
          (compose not null?)
          `(((,(format #f "Profile~a" id)
              ((default . ,(if default? 1 0))
               (is-relative . 1)
               (name . ,(string-capitalize name))
               (path . ,profile-path))))
            ,(if (not (null? settings))
                 (list
                  (format #f "~a/~a/user.js"
                          %icecat-config-path profile-path)
                  (mixed-text-file
                   (string-append file-name "-settings")
                   (serialize-settings settings)))
                 '())
            ,(if (not (string=? user-chrome ""))
                 (list
                  (format #f "~a/~a/chrome/userChrome.css"
                          %icecat-config-path profile-path)
                  (mixed-text-file
                   (string-append file-name "-user-chrome")
                   user-chrome))
                 '())
            ,(if (not (string=? user-content ""))
                 (list
                  (format #f "~a/~a/chrome/userContent.css"
                          %icecat-config-path profile-path)
                  (mixed-text-file
                   (string-append file-name "-user-content")
                   user-content))
                 '())))))))

  (match config
    (($ <home-icecat-configuration> location package profiles)
     (begin
       (check-only-one-default (map icecat-profile-default? profiles))
       (check-duplicate-field "name" (map icecat-profile-name profiles))
       (check-duplicate-field "id" (map icecat-profile-id profiles))
       (let* ((configs (map serialize-profiles profiles))
              (profile-ini (append '((General
                                      ((start-with-last-profile . 1))))
                                   (append-map first configs))))
         (define (serialize-field key val)
           (let ((val (cond
                       ((boolean? val) (if val 1 0))
                       (else val))))
             (format #f "~a=~a\n" (object->camel-case-string key 'upper) val)))
         `((,(format #f "~a/profiles.ini" %icecat-config-path)
            ,(mixed-text-file
              "icecat-profile-ini"
              (generic-serialize-ini-config
               #:serialize-field serialize-field
               #:fields profile-ini)))
           ,@(apply append (map rest configs))))))))

(define home-icecat-service-type
  (service-type (name 'home-icecat)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        icecat-files-service)
                       (service-extension
                        home-profile-service-type
                        icecat-profile-service)))))
