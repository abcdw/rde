# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:

let

  # Import unstable channel.
  # sudo nix-channel --add http://nixos.org/channels/nixos-unstable nixos-unstable
  # sudo nix-channel --update nixos-unstable
  #nixos-unstable = import inputs.nixos-unstable { config = config.nixpkgs.config; localSystem = "x86_64-linux"; };

in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  nix = {
    package = pkgs.unstable.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # TODO: check https://github.com/srid/nix-config
  # https://github.com/colemickens/nixpkgs-wayland
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz))
  # ];
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

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.

  time.timeZone = "Europe/Moscow";

  networking.hostName = "xenia";
  networking.useDHCP = false;
  networking.interfaces.enp2s0.useDHCP = true;
  networking.interfaces.enp3s0.useDHCP = true;
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
    allowedTCPPorts = [ 22 ];
  };

  # networking.wireguard.interfaces = {
  #   # "wg0" is the network interface name. You can name the interface arbitrarily.
  #   wg0 = {
  #     # Determines the IP address and subnet of the client's end of the tunnel interface.
  #     # ips = [ "10.100.0.2/24" ];

  #     # Path to the private key file.
  #     #
  #     # Note: The private key can also be included inline via the privateKey option,
  #     # but this makes the private key world-readable; thus, using privateKeyFile is
  #     # recommended.
  #     privateKeyFile = "../client.privatekey";

  #     peers = [
  #       # For a client configuration, one peer entry for the server will suffice.
  #       {
  #         # Public key of the server (not a file path).
  #         publicKey = "hZroNzudF4v3KgCF0zZqe5ZY3yHt85yjxOOY4vS5bxA=";

  #         # Forward all the traffic via VPN.
  #         allowedIPs = [ "0.0.0.0/0" ];
  #         # Or forward only particular subnets
  #         #allowedIPs = [ "10.100.0.1" "91.108.12.0/22" ];

  #         # Set this to the server IP and port.
  #         endpoint = "13.49.18.164:51820";

  #         # Send keepalives every 25 seconds. Important to keep NAT tables alive.
  #         persistentKeepalive = 25;
  #       }
  #     ];
  #   };
  # };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };
  console.useXkbConfig = true;
  
  environment.sessionVariables.TERMINAL = [ "alacritty" ];
  environment.variables = { BROWSER = "chromium"; };

  # $ nix search wget
  environment.systemPackages = with pkgs; [
    ledger
    unstable.chromium
    firefox
    brave
    unstable.next
    unstable.streamlink
    cura
    python2

    alacritty
    # emacsGit
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
    ffmpeg
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
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

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
    layout = "us,ru";
    xkbVariant = "dvorak,";
    xkbOptions =
      "ctrl:nocaps, grp:win_space_toggle, grp:rctrl_switch, grp:alt_shift_toggle";

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
