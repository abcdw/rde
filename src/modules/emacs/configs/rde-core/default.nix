{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.rde-core = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.delight ];
  };
}
