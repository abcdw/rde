{
  description = "Reproducible development environment with reasonable defaults";

  # TODO: Add default apps for mimes setup https://github.com/balsoft/nixos-config/blob/master/modules/applications.nix
  # TODO: Source color scheme from inputs https://github.com/balsoft/nixos-config/blob/master/modules/themes.nix

  # TODO: Add notifications
  # TODO: Setup GTK theme
  # TODO: Create template repo
  # TODO: Write setup instruction

  # TODO: Add doom emacs https://github.com/vlaci/nix-doom-emacs/
  # TODO: Desktop entry for roam protocol https://www.orgroam.com/manual/Installation-_00281_0029.html#Installation-_00281_0029
  # TODO: Chromium extra options https://github.com/jollheef/localhost/blob/master/desktop.nix#L126
  # TODO: Configure self flake registry entry https://github.com/balsoft/nixos-config/blob/8b73315235c690a95cb8b83feb42d1eba0fd4122/modules/packages.nix
  # TODO: Secrets from another flake
  # TODO: i3 icons for ws 

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

      inpts = inputs;

      devShell."${system}" = import ./shell.nix {
        pkgs = inputs.stable.legacyPackages.${system};
        #import inputs.stable { inherit system; };
      };

      nixosModules = { rde = (import ./src/modules/rde.nix); };
      nixosConfigurations = {
        xenia = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            {
              nixpkgs.overlays =
                [ inputs.nur.overlay overlays.unstable inputs.emacs.overlay ];
            }

            (import ./src/hosts/xenia)
            inputs.stable.nixosModules.notDetected
          ];
          specialArgs = { inherit inputs; };
        };
        ixy = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            {
              nixpkgs.overlays =
                [ inputs.nur.overlay overlays.unstable inputs.emacs.overlay ];
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
              users.users.root.openssh.authorizedKeys.keyFiles =
                [ ./files/keys/id_rsa.pub ];
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
