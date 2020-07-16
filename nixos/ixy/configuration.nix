# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:

let
  nixos-unstable = import inputs.nixos-unstable {
    config = config.nixpkgs.config;
    localSystem = "x86_64-linux";
  };

in {
  imports =

    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  nixpkgs.config = { allowUnfree = true; };
  # powerManagement.enable = true;
  # powerManagement.powertop.enable = true;

  # services.tlp = {
  #   enable = true;
  #   #      START_CHARGE_THRESH_BAT0=75
  #   #      STOP_CHARGE_THRESH_BAT0=91
  #   extraConfig = ''
  #     CPU_SCALING_GOVERNOR_ON_BAT=powersave
  #     ENERGY_PERF_POLICY_ON_BAT=powersave
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
        command = "/run/current-system/sw/bin/light -U 4";
      }
      {
        keys = [ 225 ];
        events = [ "key" "rep" ];
        command = "/run/current-system/sw/bin/light -A 4";
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

  boot.kernelPackages = nixos-unstable.linuxPackages_latest;
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
      #       useOSProber = true;
      extraEntries = ''
        menuentry "NixOS experimental" {
        chainloader (hd0,2)+1
        }
      '';
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

  # boot.kernel.sysctl = {
  #   "kernel.nmi_watchdog" = 0;
  #   "vm.dirty_writeback_centisecs" = 6000;
  #   "vm.laptop_mode" = 5;
  #   "swappiness" = 1;
  #   "net.ipv4.ip_default_ttl" = 65;
  # };

  #  boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
  #  boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];

  environment.variables = {
    # BROWSER="qutebrowser";
    # EDITOR="emacs";
    BROWSER = "brave";
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    XCURSOR_SIZE = "32";
    # _JAVA_AWT_WM_NONREPARENTING="1";
    # MOZ_ENABLE_WAYLAND="1";
    # QT_QPA_PLATFORM="wayland";
    # QT_WAYLAND_DISABLE_WINDOWDECORATION="1";

  };

  networking = {
    hostName = "ixy";
    networkmanager.enable = true;
    # firewall.enable = false;
    # firewall.allowedTCPPorts = [3000 5900 5901];
  };
  networking.extraHosts = "127.0.0.1 ${config.networking.hostName}.lan";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  hardware.bluetooth.enable = true;
  console.useXkbConfig = true;
  console.font = "sun12x22";

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

  #  sound.mediaKeys.enable = true;

  systemd.services.thinkpad-fix-sound = {
    description = "Fix the sound on X1 Yoga";
    path = [ pkgs.alsaTools ];
    wantedBy = [ "default.target" ];
    after = [ "sound.target" "alsa-store.service" ];
    script = ''
      ${pkgs.alsaTools}/bin/hda-verb /dev/snd/hwC0D0 0x1d SET_PIN_WIDGET_CONTROL 0x0
    '';
  };

  powerManagement.powerUpCommands =
    "${pkgs.alsaTools}/bin/hda-verb /dev/snd/hwC0D0 0x1d SET_PIN_WIDGET_CONTROL 0x0";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # nixos-unstable.cura
    # nixos-unstable.freecad
    # nixos-unstable.openscad
    # nixos-unstable.kicad
    sqlite
    alacritty
    rofi
    nixos-unstable.brave
    nixos-unstable.firefox
    nixos-unstable.chromium
    # nixos-unstable.next
    # qutebrowser

    nixos-unstable.okular
    # libreoffice
    # nixos-unstable.zoom-us
    # nixos-unstable.steam
    # gimp
    # nixos-unstable.krita
    nixos-unstable.tdesktop
    # discord
    gnome3.nautilus
    gnome3.gvfs
    networkmanagerapplet

    lxappearance
    i3lock
    xtitle
    xclip
    xdotool
    maim
    gromit-mpx
    dunst
    nixos-unstable.polybarFull

    # google-cloud-sdk
    # nixos-unstable.kubectl
    # nixos-unstable.kubeseal
    # nixos-unstable.kustomize
    imagemagick
    ffmpeg
    nixos-unstable.youtube-dl
    nixos-unstable.plantuml
    graphviz
    # adoptopenjdk-bin
    # goldendict
    pass
    # broot

    pamixer
    alsaTools
    alsaUtils
    pavucontrol

    nixos-unstable.libmtp
    feh
    exfat-utils
    cifs-utils
    fuse_exfat
    mtpfs
    gnome3.gvfs
    jmtpfs

    ditaa
    # python3
    # zeal
    wget
    neovim
    htop
    direnv
    killall
    powertop
    xorg.xeyes

    gnumake

    # qt5.qtwayland
    unzip
    p7zip
    unrar
    gitFull
    gnupg
    # ranger
    ripgrep
    stow
    mpv
  ];

  fonts.enableFontDir = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fontconfig.dpi = 192;
  fonts.fonts = with pkgs; [
    corefonts # Micrsoft free fonts
    nixos-unstable.font-awesome
    fira-code
    hack-font
    hasklig
    inconsolata
    #    iosevka
    source-code-pro
    open-sans # need for telegram app
    unifont
    nixos-unstable.nerdfonts
  ];
  fonts.fontconfig.defaultFonts.monospace = [ "Iosevka" ];

  users.defaultUserShell = pkgs.zsh;
  fileSystems."/mnt/flash" = {
    device = "/dev/sda1";
    fsType = "auto";
    options = let
      # this line prevents hanging on network split
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
    dpi = 192;
    enable = true;
    layout = "us,ru";
    xkbVariant = "dvorak,";
    xkbOptions = "ctrl:nocaps, grp:win_space_toggle, grp:rctrl_switch";

    #    hardware.opengl.extraPackages = [ pkgs.vaapiIntel pkgs.vaapiVdpau ];
    #    videoDrivers = ["intel"];

    windowManager.bspwm = {
      enable = true;
      package = nixos-unstable.bspwm;
      sxhkd.package = nixos-unstable.sxhkd;
    };
    #    windowManager.default = "bspwm";

    displayManager.lightdm = {
      enable = true;
      #      autoLogin.enable = true;
      #      autoLogin.user = "abcdw";
    };
    displayManager.defaultSession = "none+bspwm";
    libinput.enable = true;
  };

  services.compton = {
    enable = true;
    # inactiveOpacity = "0.8";
    backend = "glx";
    # vSync = "opengl";
    settings = {
      paint-on-overlay = true;
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
    };
  };

  programs = {
    light.enable = true;
    # sway = {
    #   enable = true;

    #   extraPackages = with pkgs; [

    #     nixos-unstable.swaylock
    #     swayidle
    #     xwayland
    #     nixos-unstable.grim
    #     nixos-unstable.slurp
    #     nixos-unstable.mako
    #     libnotify
    #     wl-clipboard
    #     nixos-unstable.clipman
    #     alacritty
    #     rxvt_unicode
    #     rofi
    #     dmenu
    #     i3status
    #     i3status-rust
    #   ];
    # };

    ssh.startAgent = true;

    tmux = {
      enable = true;
      keyMode = "vi";
      shortcut = "t";
      terminal = "screen-256color";
    };

    zsh = {
      promptInit = ''
        export CLOUD_SDK_HOME="${pkgs.google-cloud-sdk}"
        source "$CLOUD_SDK_HOME/google-cloud-sdk/completion.zsh.inc"
      '';
      enable = true;
      enableCompletion = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
    };
  };

  # enable keychain
  services.gnome3.gnome-keyring.enable = true;
  programs.seahorse.enable = true;
  security.pam.enableSSHAgentAuth = true;
  #  security.pam.services.lightdm.enable = true;

  virtualisation.docker.enable = true;

  services.emacs = {
    enable = true;
    package = nixos-unstable.emacs;
    defaultEditor = true;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.jane = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  # };

  users.extraUsers.abcdw = {
    isNormalUser = true;
    uid = 1000;
    extraGroups =
      [ "users" "wheel" "input" "audio" "networkmanager" "docker" "sway" ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

}
