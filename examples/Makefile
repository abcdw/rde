GUIX_PROFILE=./rde/profiles/guix
GUIX=./pre-inst-env ${GUIX_PROFILE}/bin/guix

ABCDW_DIR=./src/abcdw
CONFIGS=${ABCDW_DIR}/configs.scm

ixy/home/reconfigure:
	RDE_TARGET=ixy-home ${GUIX} home \
	reconfigure --no-grafts \
	${CONFIGS}

rde/profiles:
	mkdir -p rde/profiles

rde/profiles/guix: rde/profiles rde/channels-lock.scm
	guix pull -C rde/channels-lock.scm -p ${GUIX_PROFILE}

rde/channels-lock.scm: rde/channels.scm
	echo -e "(use-modules (guix channels))\n" > ./rde/channels-lock-tmp.scm
	guix time-machine -C ./rde/channels.scm -- \
	describe -f channels >> ./rde/channels-lock-tmp.scm
	cat ./rde/channels-lock-tmp.scm > ./rde/channels-lock.scm
	rm ./rde/channels-lock-tmp.scm

clean:
	rm ./rde/guix-profile
