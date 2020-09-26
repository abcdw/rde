{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.keycast = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.keycast epkgs.moody];
  };
}
