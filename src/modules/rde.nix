{ config, lib, pkgs, ... }:

with lib; {
  options = {
    rde = {
      name = mkOption {
        default = "Andrew Tropin";
        type = types.str;
        description = ''
          Person full name, will be used across configs
        '';
      };
      username = mkOption {
        default = "abcdw";
        type = types.str;
      };
      fontSize = mkOption {
        default = 12;
        type = types.int;
      };
    };
  };
}
