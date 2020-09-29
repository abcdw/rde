{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.modus-themes = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.modus-operandi-theme ];
  };
}
