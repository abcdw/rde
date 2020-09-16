{ config, lib, pkgs, ... }:

{
  rde.enable = true;
  rde.username = "abcdw";
  rde.name = "Andrew Tropin";
  rde.font = "Iosevka";
  rde.fontSize = 28;
  rde.workDir = "/home/abcdw/work";
  rde.direnv.enable = true;
  rde.emacs.enable = true;
}
