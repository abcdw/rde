{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [bat];
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
  
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    enableAutosuggestions = true;
    history.path = "${config.xdg.dataHome}/zsh/zsh_history";
    shellAliases = {
      gis = "git status -s";
      ku = "kubectl";
    };
    localVariables = {
      PROMPT="%F{red}❯%f%F{yellow}❯%f%F{green}❯%f ";
    };
  };
}
