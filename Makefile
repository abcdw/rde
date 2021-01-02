iso:
	guix time-machine  -C rde/channels-lock -- system -L ./ disk-image -t iso9660 rde/system/install.scm

home-reconfigure:
	guix package -m rde/home/manifest.scm

home-reconfigure-local:
	guix package -L ./ -m rde/home/manifest.scm

env:
	guix time-machine -C rde/channels-lock -- environment --ad-hoc make

channels-update:
	guix pull -C rde/channels

channels-lock:
	guix describe -f channels > rde/channels-lock
