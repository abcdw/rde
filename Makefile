home:
	RDE_TARGET=ixy-home \
	GUILE_LOAD_PATH=./ guix \
	home reconfigure ./rde/examples/abcdw/configs.scm

# home-reconfigure-local:
# 	GUILE_LOAD_PATH=./ ../gnu/guix/pre-inst-env guix \
# 	home reconfigure ../rde/rde/config.scm

env:
	guix time-machine -C rde/guix/channels-lock -- \
	environment --ad-hoc make

channels-pull:
	guix pull -C rde/guix/channels-lock

channels-update-lock:
	guix time-machine -C rde/guix/channels -- \
	describe -f channels > rde/guix/channels-lock

iso:
	guix time-machine  -C rde/guix/channels-lock -- \
	system -L ./ image -t iso9660 rde/system/install.scm
