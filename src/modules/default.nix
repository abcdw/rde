{ config, lib, pkgs, inputs, ... }:

with lib; {
  imports = [ ./programs/direnv.nix ./emacs ./security/yubikey];

  options = {
    rde = {
      enable =
        mkEnableOption "Enable reproducible development environment module";
      name = mkOption {
        type = types.str;
        description = "Person full name, will be used across configs";
      };

      username = mkOption {
        type = types.str;
        description = "Username, will be used across configs";
      };
      
      email = mkOption {
        type = types.str;
        description = "Email address, will be used across configs";
      };

      workDir = mkOption {
        type = types.path;
        description = "Path to directory with projects.";
      };

      font = mkOption {
        type = types.str;
      };

      fontSize = mkOption {
        default = 12;
        type = types.int;
      };

      zsh = { enable = mkEnableOption "zsh simple configuration"; };
    };
  };

  config = mkIf config.rde.enable {
    _module.args.username = "${config.rde.username}";
  };
}
