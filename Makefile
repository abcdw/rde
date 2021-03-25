iso:
	guix time-machine  -C rde/channels-lock -- system -L ./ disk-image -t iso9660 rde/system/install.scm

home-reconfigure:
	GUILE_LOAD_PATH=./ guix home reconfigure ../rde/rde/config.scm \
	&& guile ~/.guix-home-environment/on-reconfigure

env:
	guix time-machine -C rde/channels-lock -- environment --ad-hoc make

channels-update:
	guix pull -C rde/channels

channels-lock:
	guix describe -f channels > rde/channels-lock
