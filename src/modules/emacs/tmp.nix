{ config, lib, pkgs, inputs, ... }:
let
  hm = config.home-manager.users.${config.rde.username};
in
{
  config = {
    home-manager.users.${config.rde.username} = {
      home.file."${hm.xdg.configHome}/emacs/init.el".text = '';; hello'';
    };
  };
}
