(define-module (gnu home-services terminals)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services configuration)
  #:export (home-alacritty-configuration
            home-alacritty-service-type))

(define-configuration/no-serialization home-alacritty-configuration
  (package
    (package alacritty)
    "alacritty package to use")
  (config
   (yaml-config '())
   "Association list of key-value pair configuration.  The following configuration:
@lisp
(config
 `((font . ((normal . ((family . Iosevka)
                       (style . Light)))
            (size . 18.0)))
   (key_bindings . #(((key . C)
                      (mods . Alt)
                      (action . Copy))
                     ((key . V)
                      (mods . Alt)
                      (action . Paste))))))
@end lisp

would yield:

@example
font:
  normal:
    family: Iosevka
    style: Light
  size: 18.0
key_bindings:
  -
    key: C
    mods: Alt
    action: Copy
@end example"))

(define (serialize-alacritty-config config)
  #~(string-append #$@(serialize-yaml-config config)))

(define (add-alacritty-configuration config)
  (let ((cfg (home-alacritty-configuration-config config)))
    `(("config/alacritty/alacritty.yml"
       ,(mixed-text-file
         "alacritty.yml"
         (serialize-alacritty-config cfg))))))

(define add-alacritty-package
  (compose list home-alacritty-configuration-package))

(define home-alacritty-service-type
  (service-type
   (name 'home-alacritty)
   (extensions
    (list (service-extension
           home-files-service-type
           add-alacritty-configuration)
          (service-extension
           home-profile-service-type
           add-alacritty-package)))
   (default-value (home-alacritty-configuration))
   (description "Install and configure the Alacritty terminal emulator")))
