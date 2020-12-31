iso:
	guix time-machine  -C rde/channels.scm -- system -L ./ disk-image -t iso9660 rde/system/install.scm

home-reconfigure:
	guix package -L ./ -m rde/home/manifest.scm

env:
	guix time-machine -C rde/channels.scm -- environment make
