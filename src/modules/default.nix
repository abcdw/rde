{ config, lib, pkgs, inputs, ... }:

with lib; {
  imports = [ ./programs/direnv.nix ./emacs ];
  options = {
    rde = {
      name = mkOption {
        type = types.str;
        description = ''
          Person full name, will be used across configs
        '';
      };
      username = mkOption {
        type = types.str;
        description = ''
          Username, will be used across configs
        '';
      };
      email = mkOption {
        type = types.str;
        description = ''
          Email address, will be used across configs
        '';
      };
      fontSize = mkOption {
        default = 12;
        type = types.int;
      };

      browserpass = { enable = mkEnableOption "browserpass plugin"; };

      zsh = { enable = mkEnableOption "zsh simple configuration"; };
    };
  };

  config = mkMerge [
    (mkIf config.rde.browserpass.enable {
      home-manager.users.${config.rde.username} = {
        programs.browserpass.enable = true;
        programs.chromium.extensions = [
          "naepdomgkenhinolocfifgehidddafch" # browserpass
        ];
      };
    })
  ];
}
