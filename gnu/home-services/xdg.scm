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
  `(("XDG_CACHE_HOME" . ,(home-xdg-base-directories-configuration-cache-home
                           config))
    ("XDG_CONFIG_HOME" . ,(home-xdg-base-directories-configuration-config-home
                             config))
    ("XDG_DATA_HOME" . ,(home-xdg-base-directories-configuration-data-home
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
  (format #f "XDG_~a_DIR=\"~a\"\n"
          (symbol->snake-case field-name 'upper) val))

(define-configuration home-xdg-user-directories-configuration
  (desktop
   (string "$HOME/Desktop")
   "Default ``desktop'' directory, this is what you see on your
desktop when using a desktop environment,
e.g. GNOME (@pxref{XWindow,,,guix.info}).")
  (documents
   (string "$HOME/Documents")
   "Default directory to put documents like PDFs.")
  (download
   (string "$HOME/Downloads")
   "Default directory downloaded files, this is where your Web-broser
will put downloaded files in.")
  (music
   (string "$HOME/Music")
   "Default directory for audio files.")
  (pictures
   (string "$HOME/Pictures")
   "Default directory for pictures and images.")
  (publicshare
   (string "$HOME/Public")
   "Default directory for shared files, which can be accessed by other
users on local machine or via network.")
  (templates
   (string "$HOME/Templates")
   "Default directory for templates.  They can be used by graphical
file manager or other apps for creating new files with some
pre-populated content.")
  (videos
   (string "$HOME/Videos")
   "Default directory for videos."))

(define (home-xdg-user-directories-files-service config)
  `(("config/user-dirs.conf"
     ,(mixed-text-file
       "user-dirs.conf"
       "enabled=False\n"))
    ("config/user-dirs.dirs"
     ,(mixed-text-file
       "user-dirs.dirs"
      (serialize-configuration
       config
       home-xdg-user-directories-configuration-fields)))))

(define (home-xdg-user-directories-on-reconfigure config)
  (let ((dirs (map (lambda (field)
		     ((configuration-field-getter field) config))
		   home-xdg-user-directories-configuration-fields)))
    #~(let ((ensure-dir
	     (lambda (path)
	       (mkdir-p
		((@@ (ice-9 string-fun) string-replace-substring)
		 path "$HOME" (getenv "HOME"))))))
	(display "Creating XDG user directories...")
	(map ensure-dir '#$dirs)
	(display " done\n"))))

(define home-xdg-user-directories-service-type
  (service-type (name 'home-xdg-user-directories)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-xdg-user-directories-files-service)
                       (service-extension
                        home-run-on-reconfigure-service-type
                        home-xdg-user-directories-on-reconfigure)))
                (default-value (home-xdg-user-directories-configuration))
                (description "Configure XDG user directories.  To
disable a directory, point it to the $HOME.")))

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
