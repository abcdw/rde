{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.volatile-highlights = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.volatile-highlights ];
  };
}
