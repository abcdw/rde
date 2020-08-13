{
  description = "Reproducible dev environment flakes";

  inputs = {
    stable.url = "github:NixOS/nixpkgs/nixos-20.03";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:rycee/home-manager/bqv-flakes";
      inputs.nixpkgs.follows = "stable";
    };
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";

    # secrets = {
    #   type = "indirect";
    #   id = "secrets";
    #   flake = false;
    # };
  };

  outputs = inputs:
    let
      lib = inputs.stable.lib;

      system = "x86_64-linux";
      overlays = {
        unstable = final: prev: {
          unstable = (import inputs.unstable { inherit system; });
        };
      };
    in {

      template = { };
      # TODO: Create template repo
      # TODO: Write setup instruction
      inpts = inputs;

      devShell."${system}" = import ./shell.nix {
        pkgs = inputs.stable.legacyPackages.${system};
          #import inputs.stable { inherit system; };
      };

      nixosModules = {
            rde = (import ./src/modules/rde.nix);
      };
      nixosConfigurations = {
        xenia = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ (import ./nixos/xenia/configuration.nix) ];
          specialArgs = { inherit inputs; };
        };
        ixy = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            {
              nixpkgs.overlays = [
                inputs.nur.overlay
                overlays.unstable
                inputs.emacs.overlay
              ];
            }

            (import ./src/hosts/ixy)
            (import ./src/home.nix)
            (import ./src/config.nix)
            inputs.self.nixosModules.rde
            inputs.home-manager.nixosModules.home-manager
            inputs.stable.nixosModules.notDetected
          ];
          specialArgs = { inherit inputs; };
        };
        aws = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            # { nixpkgs.overlays = [ overlays.unstable ]; }
            (import ./src/devices/aws.nix)
            ({ pkgs, ... }: {
              networking.hostName = "aws-proxy";
              environment.systemPackages = [ pkgs.htop ];
              users.users.root.openssh.authorizedKeys.keyFiles = [ ./files/keys/id_rsa.pub ];
            })
          ];
          specialArgs = { inherit inputs; };
        };

      };

      xenia =
        inputs.self.nixosConfigurations.xenia.config.system.build.toplevel;
      ixy = inputs.self.nixosConfigurations.ixy.config.system.build.toplevel;
      aws = inputs.self.nixosConfigurations.aws.config.system.build.toplevel;
    };
}
