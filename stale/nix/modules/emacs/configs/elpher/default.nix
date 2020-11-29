{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.elpher = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.elpher ];
  };
}
