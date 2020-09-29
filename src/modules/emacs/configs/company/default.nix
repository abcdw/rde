{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.company = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.company ];
  };
}
