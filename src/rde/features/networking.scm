;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022, 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features networking)
  #:use-module (rde features)
  #:use-module (rde predicates)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (rde home services i2p)
  #:use-module (gnu services networking)
  #:use-module (gnu system nss)
  #:use-module (rde system services networking)
  #:use-module (rde system services accounts)

  #:use-module (gnu packages i2p)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages gnome)
  #:use-module (rde packages)

  #:use-module (guix gexp)

  #:export (feature-i2pd
            feature-yggdrasil
            feature-ssh-proxy
            feature-ssh-tunnel
            feature-networking))

;; TODO: Migrate to iwd
;; iwd: https://git.sr.ht/~akagi/guixrc/tree/master/item/magi/system/services/networking.scm

;; privoxy https://wiki.archlinux.org/title/Privoxy
;; tinyproxy with i2p/tor/ygg upstreams https://youtu.be/8r2bo-EEooM
;; tun2proxy
;; https://youtu.be/wVFox5K4cFg shadowsocks

;; Create an icecat profile
;; broswer.fixup.alternate.enabled = fales
;; browser.fixup.fallback-to-https = false
;;


;;;
;;; I2Pd.
;;;

;; https://github.com/PurpleI2P/i2pd/blob/openssl/contrib/i2pd.conf
;; https://github.com/PurpleI2P/i2pd/wiki/Yggdrasil-only-router-configuration
;; https://habr.com/ru/post/545822/
;; TODO: Set ulimit for i2pd?

(define* (feature-i2pd
          #:key
          (i2pd i2pd)
          (outproxy #f)
          (less-anonymous? #f)
          (extra-i2pd-conf '()))
  "Configure I2Pd."
  (ensure-pred file-like? i2pd)
  (ensure-pred boolean? less-anonymous?)
  (ensure-pred ini-config? extra-i2pd-conf)

  (define (get-home-services config)
    (list
     (service
      home-i2pd-service-type
      (home-i2pd-configuration
       (i2pd i2pd)
       (i2pd-conf
        `((global ((bandwidth . P)))
          (sam ((enabled . #f)))
          (httpproxy
           (,@(if outproxy `((outproxy . ,outproxy)) '())
            ,@(if less-anonymous?
                  '((inbound.length . 1)
                    (outbound.length . 1))
                  '())))))))

     (unless (null? extra-i2pd-conf)
       (simple-service
        'i2pd-add-extra-i2pd-conf
        home-i2pd-service-type
        (home-i2pd-extension
         (i2pd-conf extra-i2pd-conf))))))

  (define (get-system-services _)
    (list))

  (feature
   (name 'i2pd)
   (values `((i2pd . ,i2pd)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))


;;;
;;; Yggdrasil.
;;;

;; https://wiki.archlinux.org/title/Yggdrasil

(define yggdrasil-default-peers
  #(tls://188.225.9.167:18227
    tls://yggno.de:18227
    tls://95.216.5.243:18836
    tls://51.255.223.60:54232
    "tls://[2a01:4f9:2a:60c::2]:18836"))

(define* (feature-yggdrasil
          #:key
          (yggdrasil yggdrasil)
          (peers yggdrasil-default-peers))
  "Configure Yggdrasil."
  (ensure-pred file-like? yggdrasil)
  (ensure-pred vector? peers)

  (define (get-home-services config)
    (list
     ;; MAYBE: It should be installed system-wide?
     (simple-service
      'yggdrasil-add-yggdrasil-package
      home-profile-service-type
      (list yggdrasil))))

  (define (get-system-services _)
    (list
     (simple-service
      'yggdrasil-add-yggdrasil-group-to-user
      rde-account-service-type
      (list "yggdrasil"))
     (service
      yggdrasil-service-type
      (yggdrasil-configuration
       (yggdrasil yggdrasil)
       (yggdrasil-conf
        `((peers . ,peers)))))))

  (feature
   (name 'yggdrasil)
   (values `((yggdrasil . ,yggdrasil)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))


;;;
;;; SSH Tunnels.
;;;

(define* (feature-ssh-proxy
          #:key
          (auto-start? #t)
          (host #f)
          (name host)
          (proxy-port 8123)
          (proxy-string (number->string proxy-port)))
  "Configure SSH SOCKS Proxy. To customize ssh host port and other settings use
feature-ssh."
  (ensure-pred string? host)
  (ensure-pred integer? proxy-port)
  (ensure-pred string? proxy-string)

  (define f-name
    (symbol-append 'ssh-socks-proxy- (string->symbol name)))
  (define (get-home-services config)
    (define ssh (get-value 'ssh config openssh))
    (ensure-pred file-like? ssh)
    (list
     (simple-service
      (symbol-append f-name '-shepherd-service)
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision `(,f-name))
        (auto-start? auto-start?)
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append ssh "/bin/ssh")
                        "-N" "-D" #$proxy-string #$host))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-ssh-tunnel
          #:key
          (auto-start? #t)
          (host #f)
          (name host)
          (reverse? #f)
          (local-address "localhost:8880")
          (remote-address "localhost:80"))
  "Configure SSH tunnel from @code{local-address} to @code{remote-address}.  If
@code{reverse?} is @code{#t} the direction is reversed.  Check out
@url{https://iximiuz.com/en/posts/ssh-tunnels/} to get a better understanding
of ssh tunnels.  To customize ssh host port and other settings use feature-ssh."
  (ensure-pred string? host)
  (ensure-pred string? name)
  (ensure-pred string? local-address)
  (ensure-pred string? remote-address)

  (define f-name
    (symbol-append 'ssh (if reverse? '-reverse- '-) 'tunnel-
                   (string->symbol name)))
  (define (get-home-services config)
    (define ssh (get-value 'ssh config openssh))
    (ensure-pred file-like? ssh)
    (list
     (simple-service
      (symbol-append f-name '-shepherd-service)
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision `(,f-name))
        (auto-start? auto-start?)
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append ssh "/bin/ssh")
                        "-N" #$(if reverse? "-R" "-L")
                        #$(string-join
                           ((if reverse? reverse identity)
                            (list local-address remote-address)) ":")
                        #$host))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; networking
;;;

(define* (feature-networking
          #:key
          (iwd-autoconnect? #t)
          (network-manager network-manager)
          (network-manager-applet network-manager-applet)
          mdns?)
  "Configure iwd and everything."
  (ensure-pred file-like? network-manager)
  (ensure-pred file-like? network-manager-applet)

  (define f-name 'networking)
  (define (get-home-services config)

    (list
     (simple-service 'network-manager-applet-package
                     home-profile-service-type
                     (list network-manager-applet))
     ;; TODO: Disable nm-applet notification by default
     ;; gsettings set org.gnome.nm-applet disable-connected-notifications true
     (simple-service
      'networking-nm-applet-shepherd-service
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision '(nm-applet))
        (requirement '(dbus))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append network-manager-applet "/bin/nm-applet")
                        "--indicator")
                  #:log-file (string-append
                              (getenv "XDG_STATE_HOME") "/log"
                              "/nm-applet.log"))))))))

  (define (get-system-services config)
    (list
     (service network-manager-service-type
              (network-manager-configuration
               (network-manager network-manager)
               (shepherd-requirement '(iwd))))
     (service iwd-service-type
              (iwd-configuration
               (main-conf
                `((Settings ((AutoConnect . ,iwd-autoconnect?)))))))
     (service modem-manager-service-type)
     (service usb-modeswitch-service-type)))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             ,@(if mdns?
                   `((name-service . ,%mdns-host-lookup-nss)
                     (mdns . #t))
                   '())))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
