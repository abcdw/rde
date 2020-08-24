{ config, lib, pkgs, ... }:

{
  imports = [
    ../../default.nix
    ./configuration.nix
  ];

  # rde = {
  #   username = "abcdw";
  #   zsh.enable = true;
  #   i3.enable = true;
  # };

}
