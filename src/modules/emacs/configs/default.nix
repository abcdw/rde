{ lib, ... }: {
  imports =
    lib.mapAttrsToList (k: v: builtins.toString ./. + "/${k}")
    (lib.filterAttrs (k: v: v == "directory") (builtins.readDir ./.));
}
