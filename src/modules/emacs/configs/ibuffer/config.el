(use-package ibuffer
  :config
  (global-set-key [remap list-buffers] 'ibuffer-jump))

(use-package ibuffer-vc
  :after ibuffer
  :bind (:map ibuffer-mode-map
	      ;;/ \ to remove grouping by filter-group
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root))
  :hook (ibuffer-hook . ibuffer-vc-set-filter-groups-by-vc-root))
