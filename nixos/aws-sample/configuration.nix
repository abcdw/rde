{ config, pkgs, inputs, ... }:
let nixos-ec2 = import inputs.nixpkgs { system = "x86_64-linux"; };
in {
  # imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];

  boot.extraModulePackages = [ nixos-ec2.config.boot.kernelPackages.wireguard ];

  ec2.hvm = true;

  environment.systemPackages = with nixos-ec2.pkgs; [ wireguard git htop ];

  networking.hostName = "wg-server";
}
