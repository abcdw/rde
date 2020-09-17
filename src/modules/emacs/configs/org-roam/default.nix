{ config, lib, pkgs, inputs, ... }:

with lib; {
  config = {
    rde.emacs.configs = {
      org-roam = {
        vars = {
          "rde/org-roam-directory".value =
            "${config.rde.workDir}/org-files/notes";
        };
        config = ''
          (use-package org-roam
            :hook (after-init-hook . org-roam-mode)
            :config (setq org-roam-directory rde/org-roam-directory)
            :bind (
                   :map org-roam-mode-map
                   (("C-c n l" . org-roam)
                    ("C-c n f" . org-roam-find-file)
                    ("C-c n g" . org-roam-graph-show))
                	 :map	org-mode-map
                   (("C-c n i" . org-roam-insert))
                   (("C-c n I" . org-roam-insert-immediate)))) 
        '';
        packages = epkgs: [ epkgs.org-roam ];
        systemPackages = [ pkgs.sqlite ];
      };
    };
  };
}
