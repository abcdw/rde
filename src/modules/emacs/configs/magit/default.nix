{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.magit = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.magit ];
  };
}
