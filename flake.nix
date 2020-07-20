{
  description = "Reproducible dev environment flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:rycee/home-manager/bqv-flakes";

    #emacs.url = github:nix-community/emacs-overlay;
  };
  outputs = { self, nixpkgs, ... }@inputs: {

    # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

    nixosConfigurations = {
      xenia = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ (import ./nixos/xenia/configuration.nix) ];
        specialArgs = { inherit inputs; };
      };
      ixy = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
                    inputs.home-manager.nixosModules.home-manager
                    (import ./nixos/ixy/configuration.nix)
                    nixpkgs.nixosModules.notDetected
                  ];
        specialArgs = { inherit inputs; };
      };

      # aws-sample = nixpkgs.lib.nixosSystem {
      #   system = "x86_64-linux";
      #   modules = [
      #     (import ./nixos/aws-sample/configuration.nix)
      #     import "${inputs.nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
      #     #inputs.nixpkgs.legacyPackages.x86_64-linux.nixos.modules.virtualisation.amazon-image.config
      #   ];
      #   specialArgs = { inherit inputs; };
      # };
    };

    xenia = self.nixosConfigurations.xenia.config.system.build.toplevel;
    ixy = self.nixosConfigurations.ixy.config.system.build.toplevel;

    # aws-sample =
    #   self.nixosConfigurations.aws-sample.config.system.build.toplevel;

    # defaultPackage.x86_64-linux =
    #   (builtins.head (builtins.attrValues self.nixosConfigurations)).pkgs;
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
