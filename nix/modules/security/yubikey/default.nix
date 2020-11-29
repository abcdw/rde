{ config, lib, pkgs, inputs, ... }:
with lib; {
  options = {
    rde.security.yubikey.enable = mkEnableOption "Enable support for yubikey.";
  };
  config = mkIf config.rde.security.yubikey.enable {
    services.udev.packages = [ pkgs.yubikey-personalization ];
    security.pam.services.sudo.yubicoAuth = true;
    security.pam.yubico = {
      # enable = true; # enables system-wide
      # control = "required";
      # debug = true;
      mode = "challenge-response";
    };
  };
}
