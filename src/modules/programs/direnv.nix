{ config, lib, pkgs, inputs, ... }:

with lib; {
  options = {
    rde.direnv = { enable = mkEnableOption "direnv with flakes support"; };
  };
  config = mkIf config.rde.direnv.enable {
    home-manager.users.${config.rde.username} = {
      programs.direnv = {
        enable = true;
        # enableNixDirenvIntegration = true;
        stdlib = builtins.readFile "${inputs.nix-direnv}/direnvrc";
      };
    };
  };
}
