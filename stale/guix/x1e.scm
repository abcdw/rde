(use-modules (gnu) (gnu packages))
(use-service-modules networking ssh)


(operating-system
  (host-name "x1e")
  (timezone "Europe/Moscow")
  (locale "en_US.UTF-8")

  ;; EFI bootloader
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (target "/boot/efi")))

  ;; Specify a mapped device for the encrypted root partition.
  (mapped-devices (list (mapped-device
                         (source "/dev/sda2")
                         ;; (source (uuid "00000000-0000-0000-0000-000000000000"))
                         (target "root")
                         (type luks-device-mapping))))

  (file-systems (cons* (file-system
                         (device "/dev/mapper/root")
                         (mount-point "/")
                         (type "ext4")
                         (dependencies mapped-devices))
                       (file-system
                         (device "/dev/sda1")
                         ;; (device (uuid "0000-0000" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat"))
                       (file-system
                         (mount-point "/tmp")
                         (device "none")
                         (type "tmpfs")
                         (check? #f))
                       %base-file-systems))

  ;; Specify system-wide packages.
  (packages (append (map (compose list specification->package+output)
                         '("nss-certs" "curl" "openssh" "rsync" "tmux"))
                    %base-packages))

  ;; Services
  (services (cons* (service openssh-service-type
                            (openssh-configuration
                             (permit-root-login 'without-password)
                             (authorized-keys
                              `(("root" ,(plain-file "authorized_keys"
                                                     %my-ssh-public-key))))))
                   (service dhcp-client-service-type)
                   %base-services)))
