(define-module (gnu home-services fontutils)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files)
  #:use-module (gnu packages fontutils)
  #:use-module (guix gexp)

  #:export (home-fontconfig-service-type))

;;; Commentary:
;;;
;;; Services related to fonts.  home-fontconfig service provides
;;; fontconfig configuration, which allows fc-* utilities to find
;;; fonts in Guix Home's profile and regenerates font cache on
;;; activation.
;;;
;;; Code:

(define (add-fontconfig-config-file he-symlink-path)
  `(("config/fontconfig/fonts.conf"
     ,(mixed-text-file
       "fonts.conf"
       "<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
  <dir>~/.guix-home/profile/share/fonts</dir>
</fontconfig>"))))

;; TODO: fc-cache -f is too slow, it can be called only on-change or
;; workarounded some other way.
(define (regenerate-font-cache-gexp _)
  #~(system* "fc-cache" "-f"))

(define home-fontconfig-service-type
  (service-type (name 'home-fontconfig)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-fontconfig-config-file)
                       (service-extension
                        home-activation-service-type
                        regenerate-font-cache-gexp)
                       (service-extension
                        home-profile-service-type
                        (const (list fontconfig)))))
		(default-value #f)
                (description
                 "Provides configuration file for fontconfig and make
fc-* utilities aware of font packages installed in Guix Home's profile.")))
