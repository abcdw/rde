{ config, lib, inputs, pkgs, ... }:

{
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.${config.rde.username} = {
    programs.direnv = {
      enable = true;
      #enableNixDirenvIntegration = true;
      stdlib = builtins.readFile "${inputs.nix-direnv}/direnvrc";
    };
  };
}
