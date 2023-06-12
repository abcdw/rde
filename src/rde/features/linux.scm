;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (rde features linux)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services wm)
  #:use-module (guix gexp)

  #:export (feature-backlight
            feature-pipewire))

(define* (feature-backlight
          #:key
          (default-brightness 100)
          (step 10)
          (brightnessctl brightnessctl))
  "Setup and configure brightness for various devices."
  (ensure-pred brightness? default-brightness)
  (ensure-pred any-package? brightnessctl)
  (ensure-pred brightness? step)

  (define (step->symbol op)
    (symbol-append (string->symbol (number->string step)) '% op))
  (define (backlight-home-services config)
    (list
     (simple-service
      'backlight-add-packages
      home-profile-service-type
      (list brightnessctl))
     (when (get-value 'sway config)
       (simple-service
        'backlight-add-brightness-control-to-sway
        home-sway-service-type
        `((bindsym --locked XF86MonBrightnessUp exec
                   ,(file-append brightnessctl "/bin/brightnessctl")
                   set ,(step->symbol '+))
          (bindsym --locked XF86MonBrightnessDown exec
                   ,(file-append brightnessctl "/bin/brightnessctl")
                   set ,(step->symbol '-)))))))

  (define (backlight-system-services config)
    (list
     (simple-service
      'backlight-set-brightness-on-startup
      shepherd-root-service-type
      (list (shepherd-service
             (provision '(startup-brightness))
             (requirement '(term-tty1))
             (start
              #~(lambda ()
                  (invoke #$(file-append brightnessctl "/bin/brightnessctl")
                          "set" (string-append
                                 (number->string #$default-brightness) "%"))))
             (one-shot? #t))))
     (udev-rules-service 'backlight brightnessctl)))

  (feature
   (name 'backlight)
   (values `((backlight . #t)))
   (home-services-getter backlight-home-services)
   (system-services-getter backlight-system-services)))


(define* (feature-pipewire
          #:key
          (pipewire pipewire)
          (wireplumber wireplumber)
          (pulseaudio pulseaudio))
  ""
  (define (home-pipewire-services config)
    (list
     ;; TODO: Make home-alsa-service-type
     (simple-service
      'pipewire-add-asoundrc
      home-xdg-configuration-files-service-type
      `(("alsa/asoundrc"
         ,(mixed-text-file
           "asoundrc"
           #~(string-append
              "<"
              #$(file-append
                 pipewire "/share/alsa/alsa.conf.d/50-pipewire.conf")
              ">\n<"
              #$(file-append
                 pipewire "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
              ">\n"
              "
pcm_type.pipewire {
  lib " #$(file-append
           pipewire "/lib/alsa-lib/libasound_module_pcm_pipewire.so")
  "
}

ctl_type.pipewire {
  lib " #$(file-append
           pipewire "/lib/alsa-lib/libasound_module_ctl_pipewire.so")
  "
}
")))))

     (simple-service
      'pipewire-prevent-pulseaudio-autostart
      home-xdg-configuration-files-service-type
      `(("pulse/client.conf"
         ,(mixed-text-file
           "pulse-client.conf"
           "autospawn = no"))))

     (simple-service
      'pipewire-add-shepherd-daemons
      home-shepherd-service-type
      (list
       (shepherd-service
        (requirement '(dbus))
        (provision '(pipewire))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append pipewire "/bin/pipewire"))
                  #:log-file (string-append
                              (or (getenv "XDG_LOG_HOME")
                                  (format #f "~a/.local/var/log"
                                          (getenv "HOME")))
                              "/pipewire.log")
                  #:environment-variables
                  (append (list "DISABLE_RTKIT=1")
                          (default-environment-variables)))))
       (shepherd-service
        (requirement '(pipewire))
        (provision '(wireplumber))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append wireplumber "/bin/wireplumber"))
                  #:log-file (string-append
                              (or (getenv "XDG_LOG_HOME")
                                  (format #f "~a/.local/var/log"
                                          (getenv "HOME")))
                              "/wireplumber.log")
                  #:environment-variables
                  (append (list "DISABLE_RTKIT=1")
                          (default-environment-variables)))))
       (shepherd-service
        (requirement '(pipewire))
        (provision '(pipewire-pulse))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append pipewire "/bin/pipewire-pulse"))
                  #:log-file (string-append
                              (or (getenv "XDG_LOG_HOME")
                                  (format #f "~a/.local/var/log"
                                          (getenv "HOME")))
                              "/pipewire-pulse.log")
                  #:environment-variables
                  (append (list "DISABLE_RTKIT=1")
                          (default-environment-variables)))))))

     (when (get-value 'sway config)
       (simple-service
        'pipewire-add-volume-and-player-bindings-to-sway
        home-sway-service-type
        `((bindsym --locked XF86AudioRaiseVolume "\\\n"
                   exec ,(file-append pulseaudio "/bin/pactl")
                   set-sink-mute @DEFAULT_SINK@ "false; \\\n"
                   exec ,(file-append pulseaudio "/bin/pactl")
                   set-sink-volume @DEFAULT_SINK@ +5%)
          (bindsym --locked XF86AudioLowerVolume "\\\n"
                   exec ,(file-append pulseaudio "/bin/pactl")
                   set-sink-mute @DEFAULT_SINK@ "false; \\\n"
                   exec ,(file-append pulseaudio "/bin/pactl")
                   set-sink-volume @DEFAULT_SINK@ -5%)
          (bindsym --locked XF86AudioMute exec
                   ,(file-append pulseaudio "/bin/pactl")
                   set-sink-mute @DEFAULT_SINK@ toggle)
          (bindsym --locked XF86AudioMicMute exec
                   ,(file-append pulseaudio "/bin/pactl")
                   set-source-mute @DEFAULT_SOURCE@ toggle))))

     (simple-service
      'pipewire-add-packages
      home-profile-service-type
      (list pipewire wireplumber))))

  (define (system-pipewire-services _)
    (list
     (udev-rules-service 'pipewire pipewire)))

  (feature
   (name 'pipewire)
   (values (make-feature-values pipewire wireplumber))
   (home-services-getter home-pipewire-services)
   (system-services-getter system-pipewire-services)))
