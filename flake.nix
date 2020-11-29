{
  description = "Reproducible development environment with reasonable defaults";

  # TODO: Add default apps for mimes setup https://github.com/balsoft/nixos-config/blob/master/modules/applications.nix
  # TODO: Source color scheme from inputs https://github.com/balsoft/nixos-config/blob/master/modules/themes.nix

  # NOTE: XDG vairables collection https://github.com/jwiegley/nix-config/blob/master/config/home.nix#L48
  # TODO: Add notifications
  # TODO: Setup GTK theme
  # TODO: Write setup instruction

  # TODO: Desktop entry for roam protocol https://www.orgroam.com/manual/Installation-_00281_0029.html#Installation-_00281_0029

  inputs = rec {
    stable.url = "github:NixOS/nixpkgs/nixos-20.03";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:rycee/home-manager";
      inputs.nixpkgs.follows = "stable";
    };
    # nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    nix-direnv = {
      url = "github:nix-community/nix-direnv";
      flake = false;
    };
    # doom-emacs = {
    #   url = "github:hlissner/doom-emacs";
    #   flake = false;
    # };
    # secrets = {
    #   type = "indirect";
    #   id = "secrets";
    #   flake = false;
    # };
  };

  outputs = inputs:
    let
      lib = inputs.stable.lib;
      inputs-with-rde = inputs // { rde = inputs.self; };
      system = "x86_64-linux";
      overlays = {
        unstable = final: prev: {
          unstable = (import inputs.unstable {
            overlays = [ inputs.emacs.overlay ];
            inherit system;
          });
        };
      };
    in {

      templates = {
        python.poetry = {
          path = ./nix/templates/python/poetry;
          description = "Project with poetry2nix, nix devel and nix build.";
        };
        rde = { };
      };
      defaultTemplate = inputs.self.templates.python.poetry;

      devShell."${system}" = import ./shell.nix {
        pkgs = inputs.stable.legacyPackages.${system};
        #import inputs.stable { inherit system; };
      };

      nixosModules = { rde = (import ./nix/modules/default.nix); };
      nixosModule = inputs.self.nixosModules.rde;

      nixosConfigurations = {
        xenia = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            { nixpkgs.overlays = [ overlays.unstable ]; }

            (import ./nix/hosts/xenia)

            inputs.self.nixosModules.rde
            (import ./nix/config.nix)

            inputs.home-manager.nixosModules.home-manager
            (import ./nix/home-splitted.nix)

            inputs.stable.nixosModules.notDetected
          ];
          specialArgs = { inputs = inputs-with-rde; };
        };
        ixy = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            { nixpkgs.overlays = [ overlays.unstable ]; }

            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-7th-gen
            (import ./nix/hosts/ixy)

            inputs.home-manager.nixosModules.home-manager
            (import ./nix/home.nix)

            inputs.self.nixosModules.rde
            (import ./nix/config.nix)

            inputs.stable.nixosModules.notDetected
          ];
          specialArgs = { inputs = inputs-with-rde; };
        };
        aws = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            # { nixpkgs.overlays = [ overlays.unstable ]; }
            (import ./nix/devices/aws.nix)
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
