{ config, lib, pkgs, ... }:

{
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.abcdw = let
    dev-packages = with pkgs; [ sqlite gnupg ];
    cli-packages = with pkgs; [
      nixfmt
      unzip
      killall
      htop
      ripgrep
      wget
      neovim
      youtube-dl
    ];
    media-packages = with pkgs; [
      okular
      feh
      gimp
      imagemagick
      ffmpeg
      pavucontrol
    ];
    font-packages = with pkgs; [ emacs-all-the-icons-fonts ];
    other-packages = with pkgs; [ tdesktop xfce.thunar ];
    home-packages = dev-packages ++ cli-packages ++ media-packages
      ++ font-packages ++ other-packages;
  in rec {
    home.packages = home-packages;

    home.sessionVariables = {
      LESSHISTFILE = "${xdg.dataHome}/.lesshst";
      HISTFILE = "${xdg.dataHome}/bash/bash_history";
    };

    home.stateVersion = "20.09";
    home.keyboard.layout = "us,ru";
    home.keyboard.variant = "dvorak,";
    home.keyboard.options =
      [ "ctrl:nocaps" "grp:win_space_toggle" "grp:rctrl_switch" ];
    home.homeDirectory = "/home/abcdw";

    xdg.dataHome = "${home.homeDirectory}/.local/share";
    xdg.userDirs = {
      enable = true;
      desktop = "${home.homeDirectory}/desktop";
      documents = "${home.homeDirectory}/docs";
      download = "${home.homeDirectory}/dl";
      music = "${home.homeDirectory}/music";
      pictures = "${home.homeDirectory}/pics";
      publicShare = "${home.homeDirectory}/public";
      templates = "${home.homeDirectory}/templates";
    };

    programs.emacs = {
      enable = true;
      # package = pkgs.emacsGit;
    };

    programs.mpv = {
      enable = true;
      config = {
        save-position-on-quit = true;
        hdr-compute-peak = false; # prevents brightness changes
        keep-open = true;
      };
    };

    programs.firefox = {
      enable = true;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        https-everywhere
        privacy-badger
      ];
    };

    programs.chromium = {
      enable = true;
      extensions = [
        "gcbommkclmclpchllfjekcdonpmejbdp" # https everywhere
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        "pachckjkecffpdphbpmfolblodfkgbhl" # VidIQ
      ];
    };

    programs.alacritty = {
      enable = true;
      settings = {
        draw_bold_text_with_bright_colors = true;

        font = {
          size = 9;
          bold = { style = "Bold"; };
        };

        window.padding = {
          x = 2;
          y = 2;
        };
        colors = {
          primary = {
            background = "0xfafafa";
            foreground = "0x383a42";
          };
          cursor = {
            text = "0xfafafa";
            cursor = "0x383a42";
          };
          normal = {
            black = "0xfafafa";
            red = "0xca1243";
            green = "0x50a14f";
            yellow = "0xc18401";
            blue = "0x4078f2";
            magenta = "0xa626a4";
            cyan = "0x0184bc";
            white = "0x383a42";
          };
          bright = {
            black = "0xa0a1a7";
            red = "0xca1243";
            green = "0x50a14f";
            yellow = "0xc18401";
            blue = "0x4078f2";
            magenta = "0xa626a4";
            cyan = "0x0184bc";
            white = "0x090a0b";
          };
        };
      };
    };

    programs.rofi = {
      enable = true;
      theme = "Arc";
      # extraConfig = "rofi.dpi: ${config.fonts.fontconfig.dpi}";
      extraConfig = "rofi.dpi: 196";
    };

    programs.i3status = {
      enable = true;
      modules."ipv6".enable = false;
      modules."ethernet _first_".enable = false;
      modules."wireless _first_".enable = false;
    };

    xsession.enable = true;
    xsession.windowManager.i3 = rec {
      enable = true;
      # Also disables titlebars https://rycee.gitlab.io/home-manager/options.html#opt-xsession.windowManager.i3.config.window.titlebar
      package = pkgs.i3-gaps;
      config = {
        terminal = "alacritty";
        modifier = "Mod4";
        menu = "rofi -show run";
        # gaps.smartBorders = "on";
        workspaceAutoBackAndForth = true;
        # TODO: revisit and cleanup hotkeys
        # https://github.com/rycee/home-manager/blob/master/modules/services/window-managers/i3-sway/i3.nix#L22
        keybindings = let mod = config.modifier;
        in lib.mkOptionDefault {
          "${mod}+Shift+d" = "exec ${pkgs.rofi}/bin/rofi -show run";

          "${mod}+d" = null;
          "${mod}+w" = null;
          "${mod}+Return" = null;

          "${mod}+t" = "workspace number 1";
          "${mod}+Tab" = "workspace back_and_forth";
          "${mod}+Shift+l" = "exec ${pkgs.i3lock}/bin/i3lock";
          "${mod}+Shift+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
          "${mod}+Shift+c" = "kill";
          "Shift+Print" =
            "exec ${pkgs.maim}/bin/maim -s | ${pkgs.xclip}/bin/xclip -selection clipboard -t image/png";
        };
        bars = [{ position = "top"; }];
      };
    };
    services.random-background = {
      enable = true;
      imageDirectory = "${xdg.userDirs.pictures}/wallpapers";
    };
    services.network-manager-applet.enable = true;

    programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      enableAutosuggestions = true;
      defaultKeymap = "emacs";
      history.path = "${xdg.dataHome}/zsh/zsh_history";
      shellAliases = {
        gis = "git status -s";
        ku = "kubectl";
        ls = "ls --color";
      };
      localVariables = {
        #PROMPT = "${config.rde.username}%F{red}❯%f%F{yellow}❯%f%F{green}❯%f ";
        PROMPT = "%F{red}❯%f%F{yellow}❯%f%F{green}❯%f ";
        WORDCHARS = ""; # Make M-f, M-b jump to slashes in/the/path
      };
      initExtra = ''echo -en "\033[6 q"'';
    };

    programs.tmux = {
      enable = true;
      shortcut = "t";
      terminal = "screen-256color";
      plugins = with pkgs.tmuxPlugins;
        [
          copycat # prefix + C-u to find url, n/N to navigate
        ];
      extraConfig = ''
        bind s split-window -c "#{pane_current_path}"
        bind v split-window -h -c "#{pane_current_path}"
        bind -n M-v copy-mode\; send-keys -X page-up
        set-option -g mouse on
        unbind -T copy-mode MouseDragEnd1Pane

        set-window-option -g window-status-activity-style fg="black",bg="red"

        set -g status-bg black
        set -g status-fg brightcyan
        set -g status-right-length 100
        set -g status-right '#[fg=brightcyan,bold][#(cd #{pane_current_path}; git rev-parse --abbrev-ref HEAD)] #[fg=white]#(echo "#{pane_current_path}"|sed "s;$HOME;~;") #[fg=white,bg=brightcyan] %d/%m #[fg=cyan,bg=brightwhite,bold] #(hostname) '
        set -g window-status-current-format "#[fg=black,bg=brightcyan]#I#[fg=black,bg=brightcyan,nobold,noitalics,nounderscore]:#[fg=black,bg=brightcyan]#W#F"
      '';
    };

    programs.direnv.enable = true;
    services.lorri.enable = true;

    programs.git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      userName = "Andrew Tropin";
      userEmail = "andrew@trop.in";
      signing = {
        gpgPath = "${pkgs.gnupg}/bin/gpg2";
        key = "6D941396BE823A85";
        signByDefault = true;
      };
    };
    programs.ssh = {
      enable = true;
      matchBlocks = {
        "ti.wtf" = {
          hostname = "ec2-13-49-75-141.eu-north-1.compute.amazonaws.com";
          user = "root";
          forwardAgent = true;
        };
      };
    };
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 34560;
      defaultCacheTtlSsh = 34560;
      maxCacheTtl = 34560;
      maxCacheTtlSsh = 34560;
    };
  };

}
