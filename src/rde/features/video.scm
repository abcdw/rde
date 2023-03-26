;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
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
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services video)
  #:use-module (guix gexp)
  #:export (feature-mpv
            feature-youtube-dl))

(define* (feature-mpv
          #:key
          (mpv mpv)
          (emacs-mpv emacs-mpv)
          (mpv-key "m")
          (extra-bindings '())
          (extra-mpv-conf '()))
  "Setup and configure the mpv command-line player."
  (ensure-pred file-like? mpv)
  (ensure-pred file-like? emacs-mpv)
  (ensure-pred alist? extra-mpv-conf)
  (ensure-pred alist? extra-bindings)
  (ensure-pred string? mpv-key)

  (define f-name 'mpv)

  (define (get-home-services config)
    (require-value 'fonts config)
    (define font-sans-serif (font-name (get-value 'font-sans config)))

    (append
     (list
      (service
       home-mpv-service-type
       (home-mpv-configuration
        (package mpv)
        (bindings extra-bindings)
        (default-options
         `((script . ,(file-append mpv-mpris "/lib/mpris.so"))
           (keep-open . #t)
           (save-position-on-quit . #t)
           (osd-font . ,font-sans-serif)
           (sub-font . ,font-sans-serif)
           ,@extra-mpv-conf))))
      (simple-service
       'add-mpv-mime-entries
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration
        (default
         '((video/mp4 . mpv.desktop)
           (video/mkv . mpv.desktop)
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

               ,@(if (get-value 'emacs-emms config)
                     '((defun rde-mpv-kill ()
                         "Kill mpv process unless it's `emms-player-mpv-proc'."
                         (interactive)
                         (require 'emms-player-mpv)
                         (when (equal mpv--process
                                      emms-player-mpv-proc)
                           (emms-stop))
                         (when mpv--queue
                           (tq-close mpv--queue))
                         (when (and (mpv-live-p)
                                    (not (equal mpv--process
                                                emms-player-mpv-proc)))
                           (kill-process mpv--process))
                         (with-timeout
                             (0.5 (error "Failed to kill mpv"))
                           (while (and (mpv-live-p)
                                       (not (equal mpv--process
                                                   emms-player-mpv-proc)))
                             (sleep-for 0.05)))
                         (setq mpv--process nil)
                         (setq mpv--queue nil)
                         (run-hooks 'mpv-finished-hook))

                       (defun rde-mpv-connect-to-emms-proc ()
                         "Connect to a running emms mpv process."
                         (interactive)
                         (setq mpv-playing-time-string "")
                         (when (not (equal mpv--process
                                           emms-player-mpv-proc))
                           (mpv-kill))
                         (setq mpv--process emms-player-mpv-proc)
                         (set-process-query-on-exit-flag mpv--process nil)
                         (set-process-sentinel
                          mpv--process
                          (lambda (p _e)
                            (when (memq (process-status p) '(exit signal))
                              (when (not (equal mpv--process
                                                emms-player-mpv-proc))
                                (mpv-kill))
                              (run-hooks 'mpv-on-exit-hook))))
                         (unless mpv--queue
                           (setq mpv--queue
                                 (tq-create
                                  (make-network-process
                                   :name "emms-mpv-socket"
                                   :family 'local
                                   :service emms-player-mpv-ipc-socket
                                   :coding '(utf-8 . utf-8)
                                   :noquery t
                                   :filter 'emms-player-mpv-ipc-filter
                                   :sentinel 'emms-player-mpv-ipc-sentinel)))
                           (set-process-filter
                            (tq-process mpv--queue)
                            (lambda (_proc string)
                              (ignore-errors
                                (mpv--tq-filter mpv--queue string)))))
                         (run-hooks 'mpv-on-start-hook)
                         (run-hooks 'mpv-started-hook)
                         t)

                       (defun rde-mpv-connect-to-emms-on-startup (data)
                         "Connect to the emms process with DATA."
                         (interactive)
                         (when (string= (alist-get 'event data) "start-file")
                           (rde-mpv-connect-to-emms-proc)))

                       (advice-add 'mpv-kill :override 'rde-mpv-kill)
                       (add-hook 'emms-player-mpv-event-functions
                                 'rde-mpv-connect-to-emms-on-startup))
                   '())

               ,@(if emacs-embark
                     `((with-eval-after-load 'embark
                         (defvar rde-mpv-chapter-embark-actions
                           (let ((map (make-sparse-keymap)))
                             (define-key map "r" 'mpv-set-chapter-ab-loop)))
                         (add-to-list 'embark-keymap-alist
                                      (cons 'mpv-chapter
                                            'rde-mpv-chapter-embark-actions))
                         (defvar rde-mpv-file-embark-actions
                           (let ((map (make-sparse-keymap)))
                             (define-key map "d" 'mpv-remove-playlist-entry)))
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

(define* (feature-youtube-dl
          #:key
          (youtube-dl yt-dlp)
          (youtube-dl-command (file-append youtube-dl "/bin/yt-dlp"))
          (emacs-ytdl emacs-ytdl)
          (music-dl-args '())
          (video-dl-args '())
          (ytdl-key "y"))
  "Configure the youtube-dl program to download videos from YouTube
and various other sites."
  (ensure-pred any-package? youtube-dl)
  (ensure-pred file-like? youtube-dl-command)
  (ensure-pred file-like? emacs-ytdl)
  (ensure-pred list? music-dl-args)
  (ensure-pred list? video-dl-args)
  (ensure-pred string? ytdl-key)

  (define f-name 'youtube-dl)

  (define (get-home-services config)
    "Return home services related to youtube-dl."
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
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,ytdl-key) 'ytdl-show-list))
        (with-eval-after-load 'ytdl
          (require 'env)
          (define-key ytdl--dl-list-mode-map "a" 'ytdl-download)
          (setq ytdl-command ,youtube-dl-command)
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
   (values `((,f-name . ,youtube-dl)
             (emacs-ytdl . ,emacs-ytdl)))
   (home-services-getter get-home-services)))
