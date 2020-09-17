{ config, lib, pkgs, inputs, ... }:

with lib; {
  config = {
    rde.emacs.configs = {
      org-roam = {
        vars = {
          "rde/org-roam-directory".value =
            "${config.rde.workDir}/org-files/notes";
        };
        config = readFile ./config.el;
        emacsPackages = epkgs: [ epkgs.org-roam ];
        systemPackages = [ pkgs.sqlite ];
      };
    };
  };
}
