{ config, lib, pkgs, inputs, ... }:

with lib; {
  config.rde.emacs.configs.dired = {
    config = readFile ./config.el;
  };
}
