(define-module (rde features system)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages linux)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system linux-initrd)
  #:use-module (srfi srfi-1)

  #:export (feature-bootloader
            feature-host-info
            feature-file-systems
            feature-kernel))


(define* (feature-host-info
          #:key
          (host-name (operating-system-host-name bare-bone-os))
          (timezone  (operating-system-timezone  bare-bone-os))
          (locale    (operating-system-locale    bare-bone-os))
          (issue     (operating-system-issue     bare-bone-os)))
  "Provides basic information about host."
  (ensure-pred string? host-name)
  (ensure-pred string? timezone)
  (ensure-pred string? locale)
  (ensure-pred string? issue)

  (feature
   (name 'host-info)
   (values (make-feature-values
            host-name timezone locale issue))))


(define %default-bootloader-configuration
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))))

(define* (feature-bootloader
          #:key (bootloader-configuration %default-bootloader-configuration))
  "Provides custom bootloader configuration for operating-system.
keyboard-layout will be overriden by feature-keyboard if it present."
  (ensure-pred bootloader-configuration? bootloader-configuration)

  (feature
   (name 'bootloader)
   (values (make-feature-values bootloader-configuration))))


(define* (feature-file-systems
          #:key
          (mapped-devices '())
          (file-systems '())
          (base-file-systems %base-file-systems))
  "Provides file systems for operating-system.  By default
%base-file-systems will be added to the end of FILE-SYSTEMS, this
behavior can be overriden with BASE-FILE-SYSTEM argument."
  (ensure-pred list-of-mapped-devices? mapped-devices)
  (ensure-pred list-of-file-systems? file-systems)
  (ensure-pred list-of-file-systems? base-file-systems)

  (let ((file-systems (append file-systems base-file-systems)))
    (feature
     (name 'file-systems)
     (values (make-feature-values mapped-devices file-systems)))))


(define* (feature-kernel
	  #:key
	  (kernel linux-libre)
	  (kernel-loadable-modules '())
	  (kernel-arguments '())
	  (default-kernel-arguments %default-kernel-arguments)
          (initrd '())
	  (firmware '())
	  (base-firmware %base-firmware))
  "Provides kernel configuration."
  (ensure-pred any-package? kernel)
  (ensure-pred list-of-packages? kernel-loadable-modules)
  (ensure-pred list-of-string-or-gexps? kernel-arguments)
  (ensure-pred list-of-string-or-gexps? default-kernel-arguments)
  ;(ensure-pred initrd? initrd)
  (ensure-pred list-of-packages? firmware)
  (ensure-pred list-of-packages? base-firmware)
  (ensure-pred procedure? initrd)
  (ensure-pred list-of-strings? initrd-modules)
  (ensure-pred list-of-strings? base-initrd-modules)

  (let ((kernel-arguments (append kernel-arguments default-kernel-arguments))
        (firmware         (append firmware base-firmware))
        (initrd-modules   (append initrd-modules base-initrd-modules)))
    (feature
     (name 'kernel)
     (values (make-feature-values
	      kernel kernel-loadable-modules kernel-arguments firmware initrd)))))
