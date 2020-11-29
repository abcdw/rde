{ config, lib, pkgs, ... }:

{
  rde.enable = true;
  rde.username = "abcdw";
  rde.name = "Andrew Tropin";
  rde.font = "Iosevka";
  rde.fontSize = 10;
  rde.workDir = "/home/abcdw/work";
  rde.security.yubikey.enable = true;
  rde.direnv.enable = true;
  rde.emacs.enable = true;
  rde.emacs.preset.tropin.enable = true;
}
