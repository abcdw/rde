{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.project = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.project ];
  };
}
