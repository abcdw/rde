{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.rde-defaults = { config = readFile ./config.el; };
}
