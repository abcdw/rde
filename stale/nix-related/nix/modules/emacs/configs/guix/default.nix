{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.guix = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.guix ];
  };
}
