{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [bat gimp];
  home.keyboard.layout = "us,ru";
  home.keyboard.variant = "dvorak,";
  home.keyboard.options =
    [ "ctrl:nocaps" "grp:win_space_toggle" "grp:rctrl_switch" ];
  home.homeDirectory = "/home/abcdw";

  xdg.userDirs = {
    enable = true;
    desktop = "${config.home.homeDirectory}/desktop";
    documents = "${config.home.homeDirectory}/docs";
    download = "${config.home.homeDirectory}/dl";
    music = "${config.home.homeDirectory}/music";
    pictures = "${config.home.homeDirectory}/pics";
    publicShare = "${config.home.homeDirectory}/public";
    templates = "${config.home.homeDirectory}/templates";
  };

  programs.rofi = {
    enable = true;
    theme = "Arc";
    extraConfig = "rofi.dpi: 192";
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
    package = pkgs.i3-gaps; # Also disables titlebars https://rycee.gitlab.io/home-manager/options.html#opt-xsession.windowManager.i3.config.window.titlebar
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
    imageDirectory = "${config.xdg.userDirs.pictures}/wallpapers";
  };
  services.network-manager-applet.enable = true;

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    enableAutosuggestions = true;
    history.path = "${config.xdg.dataHome}/zsh/zsh_history";
    shellAliases = {
      gis = "git status -s";
      ku = "kubectl";
      ls = "ls --color";
    };
    localVariables = {
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
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 34560;
    defaultCacheTtlSsh = 34560;
    maxCacheTtl = 34560;
    maxCacheTtlSsh = 34560;
  };
}
