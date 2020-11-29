{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.eshell = {
    config = readFile ./config.el;
  };
}
