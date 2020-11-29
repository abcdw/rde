{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.telega = {
    config = readFile ./config.el;
    emacsPackages = epkgs: [ epkgs.telega ];
    systemPackages = [ pkgs.unstable.tdlib ];
  };
}
