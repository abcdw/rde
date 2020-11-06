{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.markdown = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.markdown ];
  };
}
