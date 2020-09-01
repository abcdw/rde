{ config, lib, pkgs, ... }:

{
  rde.username = "abcdw";
  rde.name = "Andrew Tropin";
  rde.browserpass.enable = true;
  rde.direnv.enable = true;
}
