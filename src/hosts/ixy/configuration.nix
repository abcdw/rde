# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, inputs, ... }: {

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/469716a5-d145-4152-95ed-23ab91923c91";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/8C99-0704";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/dd7d0f7c-3596-4b5e-9bcf-90181c6401af"; }];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  nixpkgs.config = { allowUnfree = true; };

  # powerManagement.enable = true;
  # powerManagement.powertop.enable = true;

  # services.tlp = {
  #   enable = true;
  #   extraConfig = ''
  #     # CPU_SCALING_GOVERNOR_ON_BAT=powersave
  #     # ENERGY_PERF_POLICY_ON_BAT=powersave
  #     START_CHARGE_THRESH_BAT0=85
  #     STOP_CHARGE_THRESH_BAT0=90
  #   '';
  # };

  #######################################################
  # systemd.services.powertop = {                       #
  #   description = ''                                  #
  #     enables powertop's reccomended settings on boot #
  #   '';                                               #
  #   wantedBy = [ "multi-user.target" ];               #
  #   path = with pkgs; [ powertop ];                   #
  #   environment = {                                   #
  #     TERM = "dumb";                                  #
  #   };                                                #
  #  serviceConfig = {                                  #
  #     Type = "idle";                                  #
  #     User = "root";                                  #
  #     ExecStart = ''                                  #
  #       ${pkgs.powertop}/bin/powertop --auto-tune     #
  #     '';                                             #
  #   };                                                #
  # };                                                  #
  #######################################################

  #  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      {
        keys = [ 224 ];
        events = [ "key" "rep" ];
        command = "${pkgs.light}/bin/light -U 4";
      }
      {
        keys = [ 225 ];
        events = [ "key" "rep" ];
        command = "${pkgs.light}/bin/light -A 4";
      }
      {
        keys = [ 113 ];
        events = [ "key" ];
        command =
          "/run/current-system/sw/bin/runuser -l abcdw -c '${pkgs.alsaUtils}/bin/amixer -q set Master toggle'";
      }
      {
        keys = [ 114 ];
        events = [ "key" "rep" ];
        command =
          "/run/current-system/sw/bin/runuser -l abcdw -c '${pkgs.alsaUtils}/bin/amixer -q -c 0 set Master 4- unmute'";
      }
      {
        keys = [ 115 ];
        events = [ "key" "rep" ];
        command =
          "/run/current-system/sw/bin/runuser -l abcdw -c '${pkgs.alsaUtils}/bin/amixer -q -c 0 set Master 4+ unmute'";
      }
    ];
  };

  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
  boot.loader = {
    # systemd-boot.enable = true;
    efi = {
      canTouchEfiVariables = true;
      # efiSysMountPoint = "/boot/efi"; # it's boot now
    };
    grub = {
      enable = true;
      version = 2;
      efiSupport = true;
      # efiInstallAsRemovable = true;
      device = "nodev";
      enableCryptodisk = true;
      gfxmodeEfi = "1024x768";
      useOSProber = true;
    };
  };

  boot.initrd.luks.devices = {
    enc-pv = {
      device = "/dev/disk/by-uuid/26d8adbb-d46d-498d-aa87-b919f40cae94";
      preLVM = true;
      allowDiscards = true;
    };
  };

  # boot.extraModprobeConfig = ''
  #   options snd_hda_intel power_save=1
  #   options iwlwifi power_save=1 d0i3_disable=0 uapsd_disable=0
  #   options iwldvm force_cam=0
  # '';

  environment.pathsToLink = [
    "/share/zsh" # Required for zsh autocomplete for systemctl
  ];

  networking = {
    hostName = "ixy";
    networkmanager.enable = true;
    # firewall.enable = false;
    # firewall.allowedTCPPorts = [3000 5900 5901];
  };
  networking.extraHosts = "127.0.0.1 ${config.networking.hostName}.lan";

  hardware.bluetooth.enable = true;
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  time.timeZone = "Europe/Moscow";

  hardware.pulseaudio = {
    enable = true;
    tcp.enable = true; # need for mpd
    support32Bit = true; # need for steam
    package = pkgs.pulseaudioFull;
  };

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  };
  hardware.steam-hardware.enable = true;

  systemd.services.thinkpad-fix-sound = {
    description = "Fix the sound on X1 Yoga";
    path = [ pkgs.alsaTools ];
    wantedBy = [ "default.target" ];
    after = [ "sound.target" "alsa-store.service" ];
    script = ''
      ${pkgs.alsaTools}/bin/hda-verb /dev/snd/hwC0D0 0x1d SET_PIN_WIDGET_CONTROL 0x0
    '';
  };

  environment.systemPackages = with pkgs;
    [
      # emacs
      # vim
      # git
    ];

  fonts.enableFontDir = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fontconfig.dpi = 192;
  fonts.fonts = with pkgs;
    [
      # corefonts # Micrsoft free fonts
      # iosevka
      # open-sans # need for telegram app
    ];
  fonts.fontconfig.defaultFonts.monospace = [ "Iosevka" ];
  fonts.enableDefaultFonts = true;

  users.defaultUserShell = pkgs.zsh;

  fileSystems."/mnt/flash" = {
    device = "/dev/sda1";
    fsType = "auto";
    options = let
    in [ "noauto,gid=100,uid=1000" ];
  };

  fileSystems."/mnt/olorin/public" = {
    device = "//olorin.lan/public";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts =
        "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in [
      "${automount_opts},vers=3.0,credentials=/home/abcdw/.smbpasswd,gid=100,uid=1000"
    ];
  };

  fileSystems."/mnt/olorin/footage-archive" = {
    device = "//olorin.lan/footage-archive";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts =
        "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in [
      "${automount_opts},vers=3.0,credentials=/home/abcdw/.smbpasswd,gid=100,uid=1000"
    ];
  };

  services.xserver = {

    # resolutions = [{ x = 1600; y = 900; }];
    enable = true;

    displayManager.lightdm.enable = true;
    displayManager.lightdm.autoLogin = {
      enable = true;
      user = "abcdw";
    };
    displayManager.session = [{
      manage = "desktop";
      name = "xsession";
      start = "exec $HOME/.xsession";
    }];
    displayManager.defaultSession = "xsession";
    # https://vid.bina.me/tools/nixos/breaking-down-the-nixos-gui-setup/
    displayManager.job.logToJournal = true;
    
    libinput.enable = true;
  };

  services.picom = {
    enable = true;
    # inactiveOpacity = "0.8";
    backend = "glx";
    vSync = true;
    settings = {
      glx-swap-method = 2;
      paint-on-overlay = true;
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
    };
  };

  programs = { light.enable = true; };

  virtualisation.docker.enable = true;

  users.extraUsers.${config.rde.username} = {
    isNormalUser = true;
    uid = 1000;
    extraGroups =
      [ "users" "wheel" "input" "audio" "networkmanager" "docker" "sway" ];
  };

  system.stateVersion = "20.03"; # Did you read the comment?
}
