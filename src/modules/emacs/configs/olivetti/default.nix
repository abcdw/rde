{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.olivetti = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.olivetti ];
  };
}
