{
  description = "Reproducible dev environment flakes";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-20.03;
    nixos-unstable.url = github:nixos/nixpkgs/nixos-unstable;
    #emacs.url = github:nix-community/emacs-overlay;
  };
  outputs = { self, nixpkgs, ... }@inputs: {

    # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
   
   nixosConfigurations = with nixpkgs.lib;
     {
       xenia =
        nixosSystem {
          system = "x86_64-linux";
          modules = [(import ./nixos/xenia/configuration.nix)];
          specialArgs = { inherit inputs; };
        };};
   defaultPackage.x86_64-linux = (builtins.head (builtins.attrValues self.nixosConfigurations)).pkgs;
   # nixosConfigurations = with nixpkgs.lib;
   #    let
   #      hosts = map (fname: builtins.head (builtins.match "(.*)\\.nix" fname))
   #        (builtins.attrNames (builtins.readDir ./hardware-configuration));
   #      mkHost = name:
   #        nixosSystem {
   #          system = "x86_64-linux";
   #          modules = [ (import ./default.nix) ];
   #          specialArgs = { inherit inputs name; };
   #        };
   #    in genAttrs hosts mkHost;

  };
}
