# pipefail is not POSIX complaint
home:
	set -o pipefail; \
	RDE_TARGET=ixy-home \
	GUILE_LOAD_PATH=./ \
	guix home reconfigure ./rde/examples/abcdw/configs.scm \
	3>&2 2>&1 1>&3- | grep -v "^;;;" 3>&2 2>&1 1>&3-

# home-reconfigure-local:
# 	GUILE_LOAD_PATH=./ ../gnu/guix/pre-inst-env guix \
# 	home reconfigure ../rde/rde/config.scm

# Rewrite to glakes
env:
	guix time-machine -C stale/guix-related/guix/channels-lock -- \
	environment --ad-hoc make

channels-pull:
	guix pull -C stale/guix-related/guix/channels-lock

channels-update-lock:
	guix time-machine -C stale/guix-related/guix/channels -- \
	describe -f channels > stale/guix-related/guix/channels-lock

iso:
	guix time-machine  -C stale/guix-related/guix/channels-lock -- \
	system -L ./ image -t iso9660 stale/guix-related/system/install.scm
