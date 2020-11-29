{ config, lib, pkgs, inputs, ... }:
{
  imports = [ "${inputs.stable}/nixos/modules/virtualisation/amazon-image.nix" ];
  ec2.hvm = true;

}
