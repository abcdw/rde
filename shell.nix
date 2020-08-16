{ pkgs ? import <nixpkgs> {} , ...}:

with pkgs;
mkShell {
  buildInputs = [ gnumake stow nix-deploy nixFlakes];
  # TODO: Add some proper doc to shellHook or similiar mechanism
  # To make it easily explorable
  shellHook = ''
    echo -e "Some super cool docs about current project!!!!!!!!!!!!!!!!!"
    echo -e "Some help about available aliases!!!!!!!!!!!!!!!!!"
    echo -e "and make targets!!!!!!!!!!!!!!!!!"
  '';
}
