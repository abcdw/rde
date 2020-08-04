{ pkgs ? import <nixpkgs> {} , ...}:

with pkgs;
mkShell {
  buildInputs = [ gnumake stow nix-deploy nixFlakes];
}
