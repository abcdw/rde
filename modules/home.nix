{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [bat gimp];
  xdg.userDirs = {
    enable = true;
    desktop = "\$HOME/desktop";
    documents = "\$HOME/docs";
    download = "\$HOME/dl";
    music = "\$HOME/music";
    pictures = "\$HOME/pics";
    publicShare = "\$HOME/public";
    templates = "\$HOME/templates";
  };
  
  xsession.enable = true;
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      terminal = "alacritty";
      modifier = "Mod4";
      menu = "rofi -show run";
      keybindings =
        let
          mod = config.xsession.windowManager.i3.config.modifier;
        in lib.mkOptionDefault {
          "${mod}+Shift+d" = "exec ${pkgs.rofi}/bin/rofi -show run";
          "${mod}+d" = null;
          "${mod}+Return" = null;
          "${mod}+Shift+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
          "${mod}+Shift+c" = "kill";
          "Shift+Print" = "exec ${pkgs.maim}/bin/maim -s | ${pkgs.xclip}/bin/xclip -selection clipboard -t image/png";
        };
      bars = [{position = "top";}];
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
      PROMPT="%F{red}❯%f%F{yellow}❯%f%F{green}❯%f ";
    };
    initExtra = ''echo -en "\033[6 q"'';
  };

  programs.tmux = {
    enable = true;
    shortcut = "t";
    terminal = "screen-256color";
    plugins = with pkgs.tmuxPlugins; [
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
