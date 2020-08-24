{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
mkShell {
  buildInputs = [ gnumake stow nix-deploy nixFlakes ];
  # TODO: Add some proper doc to shellHook or similiar mechanism
  # To make it easily explorable
  shellHook = ''
    echo
    echo -e "To activate new configuration:"
    echo -e "> make switch"
    echo
  '';
}
