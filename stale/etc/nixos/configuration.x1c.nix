# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  # Import unstable channel.
  # sudo nix-channel --add http://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
  # sudo nix-channel --update nixpkgs-unstable
  unstable = import <nixpkgs-unstable> {};
in

{
  nixpkgs.config.allowUnfree = true; # TODO: remove it
  imports = [ ./hardware-configuration.nix ];
  nixpkgs.overlays = [ (import /home/abcdw/.config/nixpkgs/overlays/rust-overlay.nix) ];
  # acpi_call is required for tlp to work properly on x1 carbon
  boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget


  environment = {
    systemPackages = with pkgs; [
      # < utility apps >
      ## terminal
      acpi curl sudo xclip psmisc gnupg
      rxvt_unicode-with-plugins alacritty
      tmux tmate
      htop powertop
      xkb_switch
      ranger fasd
      unzip p7zip unrar
      ag ripgrep
      ion rlwrap
      nix-index

      unstable.youtube-dl

      exfat ntfs3g exfat exfat-utils

      ## network
      rpPPPoE
      libqmi libmbim modemmanager
      # blueman # bluetoothctl
      # bluedevil
      tigervnc x11vnc autossh

      ## desktop
      zathura libreoffice gimp
      gparted transmission

      tdesktop slack unstable.qtox unstable.utox
      firefox qutebrowser chromium torbrowser
      wakatime workrave
      unstable.neovim

      ## media
      pavucontrol
      ffmpeg mpv gmpc mpc_cli ncmpcpp

      # < dev, langs, apps and libs >

      unstable.clojure clojure python2 python3
      rustracer leiningen
      postgresql git jdk
      # graalvm
      libxkbcommon xorg.libxkbfile xorg.libX11 xorg.libXt xorg.libXtst

      ## < qmk >
      avrdude avrbinutils avrgcc avrlibc
      dfu-util dfu-programmer gnumake
      gcc gdb

      ## cluster/virtualization
      minikube kubernetes kvm
      docker_compose unstable.kompose

      # < wm >
      rofi surfraw
      libnotify
      arandr alock scrot
      xorg.xev
      xorg.xbacklight
      networkmanagerapplet

      compton feh
      unstable.i3blocks-gaps xtitle bc
      unstable.pywal lxappearance


      # < unused stuff >
      # unetbootin
      # kbdd
      # nodejs-8_x
      # awscli
      # (polybar.override {
      # i3GapsSupport = true;
      # iwSupport = true;
      # alsaSupport = true;
      # mpdSupport = true;
      # githubSupport = true;})
    ];
    # variables.EDITOR="emacs";
    # variables.BROWSER="qutebrowser --backend webengine";
    variables.BROWSER="firefox";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernel.sysctl = { "vm.swappiness" = 1; };
  networking = {
    hostName = "tox1c";
    networkmanager.enable = true;
    # firewall.enable = false;
    firewall.allowedTCPPorts = [3000 5900 5901];
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
    consoleKeyMap = "dvorak";
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
      # iosevka
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

  environment.variables._JAVA_OPTIONS = "-Dswing.defaultlaf=javax.swing.plaf.metal.MetalLookAndFeel -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on";
  environment.variables.RUST_SRC_PATH = "${pkgs.latest.rustChannels.stable.rust-src}/lib/rustlib/src/rust/src";
  services = {
    printing.enable = true;
    xserver = {
      enable = true;
      videoDrivers = [ "intel" ];
      resolutions = [{x = 1600; y = 900;}];

      layout = "us,ru";
      xkbVariant = "dvorak,";
      xkbOptions = "caps:ctrl_modifier, grp:win_space_toggle, grp:rctrl_switch";
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
      windowManager.default = "i3";
      windowManager.i3 = {
        enable = true;
        package=unstable.i3-gaps;
      };
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
  system.stateVersion = "17.09";
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = https://nixos.org/channels/nixos-unstable;
  system.copySystemConfiguration = true;

}
