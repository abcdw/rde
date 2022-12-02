;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (rde features gnupg)
  #:use-module (rde features)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services wm)
  #:use-module (guix gexp)

  #:export (feature-gnupg))

(define* (feature-gnupg
          #:key gpg-primary-key
          (gnupg gnupg)
          (gpg-ssh-agent? #t)
          (ssh-keys '())
          (pinentry-flavor 'qt)
          (default-ttl 86400)
          (gpg-extra-config '())
          (gpg-agent-extra-config '()))
  "Sets up gnupg, if SSH-AGENT? specified also sets up gpg's ssh-agent
and provides GPG-PRIMARY-KEY value for other features."

  (ensure-pred string? gpg-primary-key)
  (ensure-pred boolean? gpg-ssh-agent?)
  (ensure-pred pinentry-flavor? pinentry-flavor)
  (ensure-pred integer? default-ttl)
  (ensure-pred list? gpg-extra-config)
  (ensure-pred list? gpg-agent-extra-config)
  (ensure-pred ssh-keys-list? ssh-keys)
  (ensure-pred file-like? gnupg)

  (define (home-gnupg-services config)
    "Return a list of home-services, required for gnupg to operate."
    (list
     ;; TODO: Move to sway feature
     (when (get-value 'sway config)
       (simple-service
        'gnupg-updatestartuptty-on-sway-launch
        home-sway-service-type
        `((exec ,(file-append gnupg "/bin/gpg-connect-agent")
                updatestartuptty /bye >/dev/null)
          ,@(if (equal? pinentry-flavor 'qt)
                '((for_window "[app_id=\"pinentry-qt\"]" floating enable))
                '())
          (,#~""))))

     ;; <https://github.com/drduh/YubiKey-Guide#harden-configuration>
     ;; <https://raw.githubusercontent.com/drduh/config/master/gpg.conf>
     (service
      home-gnupg-service-type
      (home-gnupg-configuration
       (package gnupg)
       (shepherd? #f)
       (gpg-config
        (home-gpg-configuration
         (extra-config
          `((keyid-format . long)
            (personal-cipher-preferences . (AES256 AES192 AES))
            (personal-digest-preferences . (SHA512 SHA384 SHA256))
            (personal-compress-preferences . (ZLIB BZIP2 ZIP Uncompressed))
            (default-preference-list . (SHA512 SHA384 SHA256
                                        AES256 AES192 AES
                                        ZLIB BZIP2 ZIP Uncompressed))
            (cert-digest-algo . SHA512)
            (s2k-digest-algo . SHA512)
            (s2k-cipher-algo . AES256)
            (charset . utf-8)

            (with-subkey-fingerprint . #t)
            (keyserver . "hkps://keyserver.ubuntu.com:443")
            ;; (keyserver . "hkps://keys.openpgp.org")
            ;; (keyserver . "hkps://pgp.mit.edu")
            ;; (keyserver . "hkps://hkps.pool.sks-keyservers.net")
            ;; (keyserver . "hkps://ha.pool.sks-keyservers.net")
            ;; (keyserver . "hkps://pgp.ocf.berkeley.edu")

            ,@gpg-extra-config))))
       (gpg-agent-config
        (home-gpg-agent-configuration
         (extra-config
          ;; TTL for security-tokens doesn't make sense
          `(,@(if (not (get-value 'security-token config))
                  `((default-cache-ttl . ,default-ttl)
                    (default-cache-ttl-ssh . ,default-ttl)
                    (max-cache-ttl . ,default-ttl)
                    (max-cache-ttl-ssh . ,default-ttl))
                  '())
            ,@gpg-agent-extra-config))
         (ssh-agent? gpg-ssh-agent?)
         (ssh-keys ssh-keys)
         (pinentry-flavor pinentry-flavor)))))))

  (feature
   (name 'gnupg)
   (values (append
            (make-feature-values gpg-primary-key gpg-ssh-agent?)
            (if gpg-ssh-agent?
                '((ssh-agent? . #t))
                '())))
   (home-services-getter home-gnupg-services)))
