# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true; # TODO: remove it
  imports = [ ./hardware-configuration.nix ];

  # acpi_call is required for tlp to work properly on x1 carbon
  boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget


  environment = {
    systemPackages = with pkgs; [
      exfat
      # upower
      acpi
      curl
      git
      sudo
      htop
      rxvt_unicode-with-plugins
      tmux
      tmate
      xclip
      scrot
      psmisc

      alock
      networkmanagerapplet
      pavucontrol

      tdesktop
      slack

      qutebrowser
      chromium
      torbrowser

      workrave

      zathura

      mpv
      gmpc
      mpc_cli

      clojure
      leiningen
      ag
      ripgrep
      gnupg
      # xxkb
      kbdd
      libqmi
      libmbim
      modemmanager
      # blueman # bluetoothctl is enough for me
      # bluedevil

      xorg.xev
      xorg.xbacklight
      xkb_switch
      unzip
      p7zip
      unrar

      postgresql
      gparted
      transmission
      libnotify
      wakatime
      # emacs
      powertop

      # teensy-loader-cli
      avrdude
      avrgcclibc
      dfu-util
      dfu-programmer
      # gcc
      gnumake
    ];
    # variables.EDITOR="emacs";
    variables.BROWSER="qutebrowser --backend webengine";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernel.sysctl = { "vm.swappiness" = 1; };

  networking = {
    hostName = "tox1c";
    networkmanager.enable = true;
  };

  # systemd.services.ModemManager.enable = true; # doesn't work

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    tcp.enable = true; # need for mpd
    package = pkgs.pulseaudioFull;
  };
  hardware.bluetooth.enable = true;


  i18n = {
    # consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "dvorak-programmer";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Moscow";

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/2e3f7ad3-c183-41f7-a170-96c22a7780a8";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = false;
    fonts = with pkgs; [
      dejavu_fonts
      iosevka
      source-code-pro
      source-sans-pro
      source-serif-pro
      hack-font
    ];
    # fontconfig.dpi=180;
    fontconfig = {
      # defaultFonts = {
      #   monospace = [ "Iosevka" ];
      #   sansSerif = [ "Source Sans Pro" ];
      #   serif     = [ "Source Serif Pro" ];
      # };
      ultimate = {
        enable = false;
      };
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services = {
    xserver = {
      enable = true;
      videoDrivers = [ "intel" ];
      resolutions = [{x = 1600; y = 900;}];

      layout = "us,ru";
      xkbVariant = "dvp,typewriter";
      xkbOptions = "caps:ctrl_modifier, grp:shift_toggle";
      synaptics = {
        enable = true;
        twoFingerScroll = true;
      };

      displayManager.sddm = {
        enable = true;
        autoLogin.enable = true;
        autoLogin.user = "abcdw";
      };
      desktopManager.default = "none";
      windowManager.default = "awesome";
      windowManager.awesome = {
        enable = true;
        luaModules = [ pkgs.luaPackages.luafilesystem pkgs.luaPackages.cjson ];
      };
    };
    urxvtd.enable = true;

    redshift = {
      enable = true;
      latitude = "55.752675";
      longitude = "48.6919663";
      temperature.day = 4000;
      temperature.night = 3000;
    };

    tlp = {
      enable = true;
      extraConfig = ''
        START_CHARGE_THRESH_BAT0=75
        STOP_CHARGE_THRESH_BAT0=91
      ''; # for better battery lifetime
    };
    acpid.enable = true;
    mpd = {
      enable = true;
      dataDir = "/home/abcdw/.mpd";
      user = "abcdw";
      group = "users";
      extraConfig = ''
      audio_output {
        type "pulse"
        name "MPD pulse audio output"
        server "localhost"
      }
      '';
    };
    nixosManual.showManual = true;
    # startGnuPGAgent = true;
    emacs = {
      defaultEditor = true;
      enable = true;
    };
  };

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };
  users.extraUsers.abcdw = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "users" "wheel" "input" "audio" "networkmanager" "docker" ];
    # initialPassword = "password";
  };
  users.defaultUserShell = pkgs.zsh;

  programs = {
    ssh.startAgent = true;
    zsh.enable = true;
    zsh.promptInit = "";
  };
  security.pam.enableSSHAgentAuth = true;
  virtualisation.docker.enable = true;
  # The NixOS release to be compatible with for stateful data such as databases.
#  system.stateVersion = "17.03";
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = https://nixos.org/channels/nixos-unstable;
  system.copySystemConfiguration = true;

}
