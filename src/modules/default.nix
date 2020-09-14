{ config, lib, pkgs, inputs, ... }:

with lib; {
  imports = [ ./programs/direnv.nix ./emacs ];

  options = {
    rde = {
      enable =
        mkEnableOption "Enable reproducible development environment module";
      name = mkOption {
        type = types.str;
        description = ''
          Person full name, will be used across
             configs '';
      };
      username = mkOption {
        type = types.str;
        description = "Username, will be used across configs ";
      };
      email = mkOption {
        type = types.str;
        description = ''
          Email address, will be used
             across configs '';
      };
      fontSize = mkOption {
        default = 12;
        type = types.int;
      };

      home-manager = mkOption {
        type = types.attrs;
        default = { };
        description = ''
          It's a proxy object, which just copies everything to
                    home-manager.users.${config.rde.username}'';
      };
      browserpass = { enable = mkEnableOption "browserpass plugin"; };

      zsh = { enable = mkEnableOption "zsh simple configuration"; };
    };
  };

  config = mkIf config.rde.enable (mkMerge [
    { home-manager.users.${config.rde.username} = config.rde.home-manager; }
    (mkIf config.rde.browserpass.enable {
      home-manager.users.${config.rde.username} = {
        programs.browserpass.enable = true;
        programs.chromium.extensions = [
          "naepdomgkenhinolocfifgehidddafch" # browserpass
        ];
      };
    })
  ]);
}
