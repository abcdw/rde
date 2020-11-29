{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.rg = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.rg ];
    systemPackages = [ pkgs.ripgrep ];
  };
}
