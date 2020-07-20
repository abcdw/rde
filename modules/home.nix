{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [bat];
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
    # environment.pathsToLink = [ "/share/zsh" ] in configuration.nix for autocomplete?
  };
}
