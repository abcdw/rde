# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  nixpkgs.config = { allowUnfree = true; };
  # Use the GRUB 2 boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    #    enableCryptodisk = true;
    gfxmodeEfi = "1024x768";
    extraEntries = ''
      menuentry "NixOS experimental" {
      chainloader (hd0,msdos1)+1
      }
    '';
  };

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.extraModulePackages = with config.boot.kernelPackages;
    [
      # v4l2loopback
      exfat-nofuse
      wireguard
    ];


  time.timeZone = "Europe/Moscow";

  networking.hostName = "xenia";
  networking.useDHCP = false;
  networking.interfaces.enp2s0.useDHCP = true;
  networking.interfaces.enp3s0.useDHCP = true;
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
    allowedTCPPorts = [ 22 ];
  };

  # $ nix search wget
  environment.systemPackages = with pkgs; [
    steam
    ledger
    unstable.chromium
    firefox
    brave
    unstable.next
    unstable.streamlink
    cura

    alacritty
    # emacsGit

    ffmpeg-full
    frei0r
    kdenlive

    emacs
    sqlite
    graphviz
    git
    unstable.tmux
    direnv
    htop

    wireguard
    pass
    unstable.gopass
    unstable.sway

    #    nixos-unstable.steam
    vanilla-dmz
    tdesktop
    gnome3.nautilus
    gromit-mpx
    scrcpy
    feh
    unzip
    ripgrep
    mpv
    imagemagick

    maim
    xclip

    gimp
    obs-studio

    wireguard
    nixfmt
    awscli
    unstable.nix-simple-deploy

    pavucontrol
    alsaUtils
  ];

  hardware.opengl.driSupport32Bit = true;
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
  hardware.pulseaudio = {
    enable = true;
    tcp.enable = true; # need for mpd
    support32Bit = true; # need for steam
    package = pkgs.pulseaudioFull;
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  fonts.enableFontDir = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fontconfig.dpi = 192;
  fonts.fonts = with pkgs; [
    corefonts # Micrsoft free fonts
    unstable.font-awesome
    fira-code
    hack-font
    hasklig
    inconsolata
    iosevka
    source-code-pro
    open-sans # need for telegram app
    unifont
    #    nixos-unstable.nerdfonts
  ];
  fonts.fontconfig.defaultFonts.monospace = [ "Iosevka" ];

  fileSystems."/mnt/olorin/public" = {
    device = "//olorin.lan/public";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts =
        "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in [
      "${automount_opts},vers=3.0,credentials=/home/abcdw/.private/smbpasswd,gid=100,uid=1000"
    ];
  };

  users.extraUsers.abcdw = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "users"
      "wheel"
      "input"
      "audio"
      "networkmanager"
      "docker"
#      "sway"
      "adbusers"
    ];
  };

  services.lorri.enable = true;
  services.xserver = {
    displayManager.lightdm.enable = true;
    displayManager.lightdm.greeters.gtk.cursorTheme = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      size = 128;
    };
    deviceSection = ''Option "MetaModeOrientation" "DP-0 RightOf HDMI-1"'';
    xrandrHeads = [
      {
        output = "DP-0";
        primary = true;
      }
      {
        monitorConfig = ''
          Option "Rotate" "left"
        '';
        output = "HDMI-1";
      }
    ];

    videoDrivers = [ "nvidia" ];
    dpi = 192;
    enable = true;

    desktopManager = {
      xterm.enable = false;
      wallpaper.mode = "fill";
    };
    displayManager.defaultSession = "none+i3";
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [ rofi dmenu i3status i3lock i3blocks ];
      package = pkgs.unstable.i3-gaps;
    };
  };

  services.nginx = {
    enable = true;
    config = "";
  };

  users.defaultUserShell = pkgs.zsh;
  programs.adb.enable = true;
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
  };
  virtualisation.docker.enable = true;

  system.stateVersion = "20.03"; # Did you read the comment?
}
