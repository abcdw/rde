(define-module (rde system bare-bone)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:export (bare-bone-os))

(define bare-bone-os
  (operating-system
   (host-name "antelope")
   (timezone  "Etc/UTC")
   (locale    "en_US.utf8")
   (bootloader (bootloader-configuration
                (bootloader grub-efi-removable-bootloader)
                (targets '("/boot/efi"))))
   (issue "This is RDE.  Welcome.\n")
   (services '())
   (sudoers-file #f)
   (file-systems %base-file-systems)))
