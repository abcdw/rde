{ inputs, config, lib, pkgs, ... }:

{
  nix = {
    package = pkgs.unstable.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    registry.rde.flake = inputs.self;
    registry.nixpkgs.flake = inputs.stable;
    registry.stable.flake = inputs.stable;
    registry.unstable.flake = inputs.unstable;
    registry.emacs.flake = inputs.emacs;
  };
}
