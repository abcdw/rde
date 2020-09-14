{ config, lib, pkgs, ... }:

{
  rde.enable = true;
  rde.username = "abcdw";
  rde.name = "Andrew Tropin";

  rde.direnv.enable = true;
  rde.emacs.enable = true;
}
