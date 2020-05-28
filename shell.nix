
let
  pkgs = import <nixpkgs> {};
in
with pkgs;
mkShell {
  buildInputs = [ gnumake stow nix-deploy];
}
