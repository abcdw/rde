{ config, lib, pkgs, inputs, ... }:
let
  emacs-init = ./init.el;
  emacs-early-init = ./early-init.el;
in with lib; {
  options = {
    rde.emacs = {
      enable = mkEnableOption "Enable rde emacs";
      layers = { };
    };
  };
  config = mkIf config.rde.emacs.enable {
    home-manager.users.${config.rde.username} = {
      home.packages = [ pkgs.emacs-all-the-icons-fonts ];

      home.file.".emacs.d/init.el".source = emacs-init;
      home.file.".emacs.d/early-init.el".source = emacs-early-init;
      programs.emacs = {
        enable = true;
        package = pkgs.emacsGit;
        extraPackages = epkgs:
          with epkgs; [
            use-package
            nix-mode
            magit
            modus-operandi-theme
            org
            org-roam
            company-org-roam
            ivy
          ];
      };
    };
  };
}
