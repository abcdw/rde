{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.geiser = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.geiser ];
  };
}
