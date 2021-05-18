(define-module (rde features system)
  #:use-module (rde features)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (srfi srfi-1)

  #:export (feature-bootloader
	    feature-file-systems))

(define %default-bootloader-configuration
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")))

(define* (feature-bootloader
	  #:key (bootloader-configuration %default-bootloader-configuration))
  "Provides custom bootloader configuration for operating-system.
keyboard-layout will be overriden by feature-keyboard if it present."
  (ensure-pred bootloader-configuration? bootloader-configuration)

  (feature
   (name 'bootloader)
   (values (make-feature-values bootloader-configuration))))


(define (list-of-file-systems? lst)
  (and (list? lst) (every file-system? lst)))

(define* (feature-file-systems
	  #:key
	  (file-systems '())
	  (base-file-systems %base-file-systems))
  "Provides file systems for operating-system.  By default
%base-file-systems will be added to the end of FILE-SYSTEMS, this
behavior can be overriden with BASE-FILE-SYSTEM argument."
  (ensure-pred list-of-file-systems? file-systems)

  (let ((file-systems (append file-systems base-file-systems)))
    (feature
     (name 'file-systems)
     (values (make-feature-values file-systems)))))


(define* (feature-host-info
	  #:key
	  (host-name (operating-system-host-name bare-bone-os))
	  (timezone  (operating-system-timezone  bare-bone-os))
	  (locale    (operating-system-locale    bare-bone-os)))
  "Provides basic information about host."
  (ensure-pred string? host-name)
  (ensure-pred string? timezone)
  (ensure-pred string? locale)

  (feature
   (name 'host-info)
   (values (make-feature-values host-name timezone locale))))
