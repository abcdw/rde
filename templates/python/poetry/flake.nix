{
  description = "Description of your project goes here.";
  inputs = {
    stable.url = "github:NixOS/nixpkgs/nixos-20.03";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs = inputs:
    let
      lib = inputs.stable.lib;
      system = "x86_64-linux";
      unstable-pkgs = inputs.unstable.legacyPackages.${system};
      pkgs = inputs.stable.legacyPackages.${system};
      runtimeDeps = with pkgs; [ ]; # Any system packages goes here
      config = { projectDir = ./.; };
      app = unstable-pkgs.poetry2nix.mkPoetryApplication config // {
        propagatedBuildInputs = runtimeDeps;
      };
      # env = app.dependencyEnv;
      env = pkgs.poetry2nix.mkPoetryEnv config;
    in {
      devShell."${system}" = pkgs.mkShell {
        buildInputs = with pkgs;
          [ poetry ] ++ runtimeDeps
          ++ (if (builtins.pathExists ./poetry.lock) then [ env ] else [ ]);
        shellHook = ''
          cat README
        '';
      };
      defaultPackage."${system}" = app;
    };
}
