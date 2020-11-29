{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.faces = { config = readFile ./config.el; };
}
