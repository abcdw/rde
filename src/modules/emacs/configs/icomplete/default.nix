{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.icomplete = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.orderless ];
  };
}
