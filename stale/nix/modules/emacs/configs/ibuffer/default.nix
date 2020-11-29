{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.ibuffer = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.ibuffer-vc ];
  };
}
