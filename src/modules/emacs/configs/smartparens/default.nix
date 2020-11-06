{ config, lib, pkgs, inputs, ... }:

with lib; {
  config = {
    rde.emacs.configs = {
      smartparens = {
        config = readFile ./config.el;
        emacsPackages = epkgs: [ epkgs.smartparens ];
      };
    };
  };
}
