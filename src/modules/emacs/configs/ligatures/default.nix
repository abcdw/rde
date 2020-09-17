{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.ligatures = { config = readFile ./config.el; };
}
