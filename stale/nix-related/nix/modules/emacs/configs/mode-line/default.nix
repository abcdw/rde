{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.mode-line = {
    config = readFile ./config.el;
  };
}
