(define-module (gnu home-services xdg)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
  #:use-module (gnu home-services-utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)


  #:export (home-xdg-base-directories-service-type
            home-xdg-base-directories-configuration
            home-xdg-user-directories-service-type
            home-xdg-user-directories-configuration
            home-xdg-mime-applications-service-type
            home-xdg-mime-applications-configuration))

;;; Commentary:
;;
;; This module contains services related to XDG directories and
;; applications.
;;
;; - XDG base directories
;; - XDG user directories
;; - XDG MIME applications
;;
;; TODO: Add proper docs for XDG
;; TODO: Deal with state/log directories
;;
;;; Code:


;;;
;;; XDG base directories.
;;;

(define (serialize-path field-name val) "")
(define path? string?)

;; TODO: Very long name, should we shorten it?
(define-configuration home-xdg-base-directories-configuration
  (cache-home
   (path "$HOME/.cache")
   "Base directory for programs to store non-essential user data, like
cache.")
  (config-home
   (path "$HOME/.config")
   "Base directory for programs to store configuration files.")
  (data-home
   (path "$HOME/.local/share")
   "Base directory for programs to store user data, like history."))

(define (home-xdg-base-directories-environment-vars-service config)
  ;; XXX: We don't use 'serialize-configuration' because it returns an
  ;; opaque Gexp which we can't map over.  Should we create a version
  ;; which just returns a string or list?
  `(("$XDG_CACHE_HOME" . ,(home-xdg-base-directories-configuration-cache-home
                           config))
    ("$XDG_CONFIG_HOME " . ,(home-xdg-base-directories-configuration-config-home
                             config))
    ("$XDG_CACHE_HOME" . ,(home-xdg-base-directories-configuration-data-home
                           config))))

(define home-xdg-base-directories-service-type
  (service-type (name 'home-xdg-base-directories)
                (extensions
                 (list (service-extension
                        home-environment-vars-service-type
                        home-xdg-base-directories-environment-vars-service)))
                (default-value (home-xdg-base-directories-configuration))
                (description "Configure XDG base directories.")))

(define (generate-home-xdg-base-directories-documentation)
  (generate-documentation
   `((home-xdg-base-directories-configuration
      ,home-xdg-base-directories-configuration-fields))
   'home-xdg-base-directories-configuration))


;;;
;;; XDG user directories.
;;;

(define (serialize-string field-name val)
  ;; The path has to be quoted
  (format #f "~a=\"~a\"\n"
          (symbol->snake-case field-name 'upper "XDG_") val))

(define-configuration home-xdg-user-directories-configuration
  (desktop-dir
   (string "$HOME/Desktop")
   "Default ``desktop'' directory, this is what you see on your desktop
when using a desktop environment, e.g. GNOME (@pxref{X
Window,,,guix.info}).")
  (documents-dir
   (string "$HOME/Documents")
   "Default directory to put documents like PDFs.")
  (download-dir
   (string "$HOME/Downloads")
   "Default directory downloaded files, this is where your Web-broser
will put downloaded files in.")
  (music-dir
   (string "$HOME/Music")
   "Default directory for audio files.")
  (pictures-dir
   (string "$HOME/Pictures")
   "Default directory for pictures and images.")
  ;; TODO: I have no idea what this is used for, it doesn't say
  ;; anything about it in the docs.
  (publicshare-dir
   (string "$HOME/Public")
   "I dunno ????")
  (templates-dir
   (string "$HOME/Templates")
   "I dunno???")
  (videos-dir
   (string "$HOME/Videos")
   "Default directory for videos."))

;; TODO: Generate user-dirs.locale?
(define (home-xdg-user-directories-files-service config)
  `(("config/user-dirs.dirs"
     ,(mixed-text-file
       "xdg-user-dirs"
      (serialize-configuration
       config
       home-xdg-user-directories-configuration-fields)))))

;; Run 'xdg-user-dirs-update' on each login to keep things up to date.
(define (home-xdg-user-directories-on-login-service config)
  (list (file-append xdg-user-dirs "/bin/xdg-user-dirs-update")))

(define home-xdg-user-directories-service-type
  (service-type (name 'home-xdg-user-directories)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-xdg-user-directories-files-service)
                       (service-extension
                        home-run-on-first-login-service-type
                        home-xdg-user-directories-on-login-service)))
                (default-value (home-xdg-user-directories-configuration))
                (description "Configure XDG user directories.")))

(define (generate-home-xdg-user-directories-documentation)
  (generate-documentation
   `((home-xdg-user-directories-configuration
     ,home-xdg-user-directories-configuration-fields))
   'home-xdg-user-directories-configuration))


;;;
;;; XDG MIME applications.
;;;

;; See
;; <https://specifications.freedesktop.org/shared-mime-info-spec/shared-mime-info-spec-latest.html>
;; <https://specifications.freedesktop.org/mime-apps-spec/mime-apps-spec-latest.html>

;; TODO: Create a way to add custom desktop entries?
;; (define-record-type <xdg-desktop-entry>)

(define (serialize-ini-config field-name val)
  (define (serialize-field field-name val)
    (format #f "~a=~a\n" field-name val))

  (define (format-section section)
    (string-capitalize
     (string-replace-substring
      (maybe-object->string section) "-" " ")))
  
  (generic-serialize-ini-config
   #:format-section format-section
   #:serialize-field serialize-field
   #:fields val))

;; TODO: Or should we use records to stop the user from potentially
;; creating an invalid config?
(define-configuration home-xdg-mime-applications-configuration
  (config
   (ini-config '())
   "List of lists representing an INI config file.
Association list of MIME type and desktop entry.  An example like
this:

@example
(config '((added-associations
           ((application/x-bittorrent . torrent.desktop)
            (inode/directory . file.desktop)))
          (default-associations
            ((x-scheme-handler/magnet . torrent.desktop)))))
@end example

would result in

@example
[Added Associations]
application/x-bittorrent=torrent.desktop
inode/directory=file.desktop

[Default Applications]
x-scheme-handler/magnet=torrent.desktop
@end example

This would only have an effect if you have already defined
@file{torrent.desktop} and @file{file.desktop} somewhere else."))

(define (home-xdg-mime-applications-files-service config)
  `(("local/share/applications/mimeapps.list"
     ,(mixed-text-file
      "xdg-mime-appplications"
      (serialize-configuration
       config
       home-xdg-mime-applications-configuration-fields)))))

;; TODO: Make it extendable?
(define home-xdg-mime-applications-service-type
  (service-type (name 'home-xdg-mime-applications)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-xdg-mime-applications-files-service)))
                (default-value (home-xdg-mime-applications-configuration))
                (description "Configure XDG MIME applications.")))
