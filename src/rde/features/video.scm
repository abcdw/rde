;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features video)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages video)
  #:use-module (rde packages video)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (rde home services video)
  #:use-module (guix gexp)
  #:use-module (guix deprecation)
  #:use-module (srfi srfi-1)
  #:export (feature-mpv
            feature-yt-dlp
            feature-youtube-dl))

;; Nice mpv configs
;; https://libreddit.tiekoetter.com/r/mpv/comments/1149cpm/recommended_profiles/
(define* (feature-mpv
          #:key
          (mpv mpv)
          (uosc? #t)
          (emacs-mpv emacs-mpv)
          (mpv-key "m"))
  "Setup and configure the mpv command-line player.  UOSC? enables minimalist,
but feature-rich proximity based UI and in addition to that thumbfast for
on-the-fly thumbnail generation for progress bar."
  (ensure-pred file-like? mpv)
  (ensure-pred boolean? uosc?)
  (ensure-pred file-like? emacs-mpv)
  (ensure-pred string? mpv-key)

  (define f-name 'mpv)

  (define (get-home-services config)
    (define font-sans-serif (and=> (get-value 'font-sans config) font-name))
    (append
     (if uosc?
         (list
          ;; Reach out rde-devel mailing list if you don't know how to extend
          ;; the configuration.
          (simple-service
           'mpv-disable-builtin-ui
           home-mpv-service-type
           (home-mpv-extension
            (input-conf
             `("tab script-binding uosc/toggle-ui"
               "menu script-binding uosc/menu"))
            (mpv-conf
             `((global ((osc . no)
                        (osd-bar . no)
                        (border . no)))))))
          ;; This approach is kinda hacky and hard to extend, but was the
          ;; simplest one at the moment to get everything up and running.  It
          ;; could be a separate service shared via rde values for future
          ;; extensibility.  Also, instead of creating files in xdg config
          ;; home options could be passed as command line arguments inside
          ;; wrapper or something like that.
          (simple-service
           'mpv-add-uosc
           home-xdg-configuration-files-service-type
           `(("mpv/fonts" ,(file-append mpv-uosc "/share/mpv/fonts"))
             ("mpv/scripts/thumbfast.lua"
              ,(file-append mpv-thumbfast "/share/mpv/scripts/thumbfast.lua"))
             ("mpv/script-opts/thumbfast.conf"
              ,(mixed-text-file "thumbfast.conf" "network=yes"))
             ("mpv/scripts/uosc.lua"
              ,(file-append mpv-uosc "/share/mpv/scripts/uosc.lua"))
             ("mpv/scripts/uosc_shared"
              ,(file-append mpv-uosc "/share/mpv/scripts/uosc_shared")))))
         '())
     (list
      (service
       home-mpv-service-type
       (home-mpv-configuration
        (mpv mpv)
        (mpv-conf
         `((global ((script . ,(file-append mpv-mpris "/lib/mpris.so"))
                    ,@(if font-sans-serif
                          `((osd-font . ,font-sans-serif)
                            (sub-font . ,font-sans-serif))
                          '())))))))
      (simple-service
       'add-mpv-mime-entries
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration
        (default
         '((video/mp4 . mpv.desktop)
           (video/mkv . mpv.desktop)
           (video/webm . mpv.desktop)
           (audio/mpeg . mpv.desktop))))))
     (if (get-value 'emacs config)
         (let ((emacs-embark (get-value 'emacs-embark config)))
           (list
            (rde-elisp-configuration-service
             f-name
             config
             `((eval-when-compile
                 (require 'mpv))
               (defvar rde-mpv-map nil
                 "Map to bind `mpv' commands under.")
               (define-prefix-command 'rde-mpv-map)

               ,@(if (get-value 'emacs-ytdl config)
                     `((eval-when-compile
                         (require 'ytdl)
                         (require 'cl-lib))
                       (cl-defun rde-mpv-play-url (url &optional format
                                                       &key
                                                       audio repeat
                                                       (formats t) (select t)
                                                       playlist)
                         "Play URL with `mpv-start'.
You can specify whether to PLAY the file as AUDIO, if you want to be
prompted for FORMATS or use FORMAT, to REPEAT the file, manually SELECT what to
do with the file, and whether to add the file to the current PLAYLIST."
                         (interactive "sURI: ")
                         (require 'mpv)
                         (let* ((sel-format
                                 (or format (and formats
                                                 (ytdl-select-format url))))
                                (extra-args
                                 (split-string
                                  (concat
                                   (format "--ytdl-format=%s"
                                           (or sel-format "best"))
                                   (and audio " --video=no")
                                   (and repeat " --loop-file=inf")))))
                           (if (and select (mpv-get-property "playlist"))
                               (pcase (completing-read "Play or Enqueue: "
                                                       '("Play" "Enqueue"))
                                 ("Play" (apply 'mpv-start url extra-args))
                                 ("Enqueue" (apply 'mpv-playlist-append-url url
                                                   extra-args)))
                             (if (and playlist (mpv-get-property "playlist"))
                                 (apply 'mpv-playlist-append-url url extra-args)
                               (apply 'mpv-start url extra-args)))))

                       (defun rde-mpv-download ()
                         "Download current mpv playback via `ytdl'."
                         (interactive)
                         (require 'ytdl)
                         (if-let* ((dl-type (ytdl--get-download-type))
                                   (track (mpv-get-property "path"))
                                   (title (mpv-get-property "media-title")))
                             (ytdl--download-async
                              track
                              (expand-file-name title
                                                (ytdl--eval-field (nth 1 dl-type)))
                              (ytdl--eval-list (ytdl--eval-field (nth 2 dl-type)))
                              'ignore
                              (car dl-type))
                           (error "Mpv is not currently active")))

                       ,@(if emacs-embark
                             '((with-eval-after-load 'embark
                                 (define-key embark-url-map "v" 'rde-mpv-play-url)))
                             '())

                       (let ((map rde-mpv-map))
                         (define-key map (kbd "RET") 'rde-mpv-play-url)
                         (define-key map (kbd "s") 'rde-mpv-download)))
                     '())

               (defun rde-mpv-seek-start ()
                 "Seek to the start of the current MPV stream."
                 (interactive)
                 (mpv-seek 0))

               (defun rde-mpv-playlist-shuffle ()
                 "Toggle the shuffle state for the current playlist."
                 (interactive)
                 (mpv-run-command "playlist-shuffle"))

               (defun rde-mpv-kill-path ()
                 "Copy the path of the current mpv stream to the clibpoard."
                 (interactive)
                 (when-let* ((title (mpv-get-property "media-title"))
                             (path (mpv-get-property "path")))
                   (kill-new path)
                   (message (format "Copied \"%s\" to the system clipboard"
                                    title))
                   path))

               ,@(if emacs-embark
                     `((with-eval-after-load 'embark
                         (defvar rde-mpv-chapter-embark-actions
                           (let ((map (make-sparse-keymap)))
                             (define-key map "r" 'mpv-set-chapter-ab-loop)
                             map))
                         (add-to-list 'embark-keymap-alist
                                      (cons 'mpv-chapter
                                            'rde-mpv-chapter-embark-actions))
                         (defvar rde-mpv-file-embark-actions
                           (let ((map (make-sparse-keymap)))
                             (define-key map "d" 'mpv-remove-playlist-entry)
                             map))
                         (add-to-list 'embark-keymap-alist
                                      (cons 'mpv-file
                                            'rde-mpv-file-embark-actions))))
                     '())

               (with-eval-after-load 'rde-keymaps
                 (define-key rde-app-map (kbd ,mpv-key) 'rde-mpv-map))
               (let ((map rde-mpv-map))
                 (define-key map (kbd "a") 'rde-mpv-seek-start)
                 (define-key map (kbd "w") 'rde-mpv-kill-path)
                 (define-key map (kbd "c") 'mpv-jump-to-chapter)
                 (define-key map (kbd "l") 'mpv-jump-to-playlist-entry)
                 (define-key map (kbd "n") 'mpv-playlist-next)
                 (define-key map (kbd "p") 'mpv-playlist-prev)
                 (define-key map (kbd "N") 'mpv-chapter-next)
                 (define-key map (kbd "P") 'mpv-chapter-prev)
                 (define-key map (kbd "f") 'mpv-seek-forward)
                 (define-key map (kbd "b") 'mpv-seek-backward)
                 (define-key map (kbd "q") 'mpv-quit)
                 (define-key map (kbd "R") 'mpv-set-ab-loop)
                 (define-key map (kbd "SPC") 'mpv-pause)
                 (define-key map (kbd "r") 'mpv-toggle-loop)
                 (define-key map (kbd "v") 'mpv-toggle-video)
                 (put 'mpv-seek-forward 'repeat-map 'rde-mpv-map)
                 (put 'mpv-seek-backward 'repeat-map 'rde-mpv-map)
                 (put 'mpv-pause 'repeat-map 'rde-mpv-map))
               (with-eval-after-load 'mpv
                 (setq mpv-seek-step 3)))
             #:elisp-packages (append
                               (or (and=> emacs-embark list) '())
                               (or (and=> (get-value 'emacs-ytdl config) list)
                                   '())
                               (list emacs-mpv)))))
      '())))

    (feature
     (name f-name)
     (values (make-feature-values mpv emacs-mpv))
     (home-services-getter get-home-services)))

(define rde-yt-dlp-config
  `(("--format" . "247+251") ; 720p webm
    ("--output" . "%(title)s [%(id)s].%(ext)s")))

(define* (feature-yt-dlp
          #:key
          (yt-dlp yt-dlp)
          (yt-dlp-command (file-append yt-dlp "/bin/yt-dlp"))
          (emacs-ytdl emacs-ytdl)
          (music-dl-args '())
          (video-dl-args '())
          (ytdl-key "y"))
  "Configure the yt-dlp program to download videos from YouTube
and various other sites."
  (ensure-pred any-package? yt-dlp)
  (ensure-pred file-like? yt-dlp-command)
  (ensure-pred file-like? emacs-ytdl)
  (ensure-pred list? music-dl-args)
  (ensure-pred list? video-dl-args)
  (ensure-pred string? ytdl-key)

  (define f-name 'yt-dlp)

  (define (get-home-services config)
    "Return home services related to yt-dlp."
    (define ffmpeg-bin
      (file-append (get-value 'ffmpeg config ffmpeg) "/bin/ffmpeg"))

    (require-value 'xdg-user-directories-configuration config)
    (define user-dirs (get-value 'xdg-user-directories-configuration config))
    (define download-dir
      (home-xdg-user-directories-configuration-download user-dirs))
    (define music-dir
      (home-xdg-user-directories-configuration-music user-dirs))
    (define video-dir
      (home-xdg-user-directories-configuration-videos user-dirs))

    (list
     (simple-service
      'yt-dlp-config
      home-xdg-configuration-files-service-type
      `(("yt-dlp/config"
         ,(apply
           mixed-text-file
           "yt-dlp.conf"
           (append-map
            (lambda (pair)
              (let ((head (car pair))
                    (tail (cdr pair)))
                (append
                 (list head)
                 ;; It should be quoted to prevent misstreating it as a few
                 ;; options
                 (if (not (null? tail)) `(" \"" ,tail "\"") '())
                 (list "\n"))))
            rde-yt-dlp-config)))))
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,ytdl-key) 'ytdl-show-list))
        (with-eval-after-load 'ytdl
          (require 'env)
          (define-key ytdl--dl-list-mode-map "a" 'ytdl-download)
          (setq ytdl-command ,yt-dlp-command)
          (setq ytdl-download-folder (substitute-env-vars ,download-dir))
          (setq ytdl-music-folder (substitute-env-vars ,music-dir))
          (setq ytdl-video-folder (substitute-env-vars ,video-dir))
          (setq ytdl-mode-line nil)
          (setq ytdl-music-extra-args
                (list ,@music-dl-args "--ffmpeg-location" ,ffmpeg-bin))
          (setq ytdl-video-extra-args
                (list ,@video-dl-args "--ffmpeg-location" ,ffmpeg-bin))))
      #:elisp-packages (list emacs-ytdl))))

  (feature
   (name f-name)
   (values `((,f-name . ,yt-dlp)
             (emacs-ytdl . ,emacs-ytdl)))
   (home-services-getter get-home-services)))

(define-deprecated/alias feature-youtube-dl feature-yt-dlp)
