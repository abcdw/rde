iso:
	guix time-machine  -C rde/channels-lock -- system -L ./ disk-image -t iso9660 rde/system/install.scm

home-reconfigure:
	guix package -L ./ -m rde/home/manifest.scm

env:
	guix time-machine -C rde/channels-lock -- environment --ad-hoc make
