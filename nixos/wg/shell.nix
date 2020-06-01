let
  pkgs = import <nixpkgs> {};
in
with pkgs;
pkgs.mkShell {
  buildInputs = [
    gnumake
    nix-deploy
    wireguard
    qrencode
  ];
}
