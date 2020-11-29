{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.monocle = {
    config = readFile ./config.el;
  };
}
