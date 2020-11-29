{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.org = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.org ];
  };
}
