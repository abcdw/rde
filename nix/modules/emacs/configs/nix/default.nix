{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.nix = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.nix-mode ];
  };
}
