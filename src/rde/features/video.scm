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
          (extra-mpv-conf '()))
  "Setup and configure mpv."
  (ensure-pred file-like? mpv)
  (ensure-pred alist? extra-mpv-conf)

  (define (get-home-services config)
    (require-value 'fonts config)
    (define font-sans-serif (font-name (get-value 'font-sans config)))

    (list
     (service
      home-mpv-service-type
      (home-mpv-configuration
       (package mpv)
       (default-options
         `((script . ,(file-append mpv-mpris "/lib/mpris.so"))
           (keep-open . #t)
           (save-position-on-quit . #t)
           (osd-font . ,font-sans-serif)
           (sub-font . ,font-sans-serif)
           ,@extra-mpv-conf))))))

  (feature
   (name 'mpv)
   (values (make-feature-values mpv))
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
          (define-key ytdl--dl-list-mode-map "a" 'ytdl-download)
          (setq ytdl-command ,youtube-dl-command)
          (setq ytdl-download-folder ,download-dir)
          (setq ytdl-music-folder ,music-dir)
          (setq ytdl-video-folder ,video-dir)
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
