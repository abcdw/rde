{ config, lib, pkgs, inputs, ... }:

with lib; {
  options = {
    rde.direnv = { enable = mkEnableOption "direnv with flakes support"; };
  };
  config = mkIf config.rde.direnv.enable {
    rde.home-manager = {
      programs.direnv = {
        enable = true;
        # enableNixDirenvIntegration = true;
        stdlib = builtins.readFile "${inputs.nix-direnv}/direnvrc";
      };
    };
  };
}
