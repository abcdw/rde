# Also defined in .envrc
GUIX_PROFILE=rde/profiles/guix
GUIX=./pre-inst-env ${GUIX_PROFILE}/bin/guix

SRC_DIR=./src
CONFIGS=${SRC_DIR}/abcdw/configs.scm
PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_POINT=/mnt

ixy/home/build:
	RDE_TARGET=ixy-home ${GUIX} home \
	build ${CONFIGS}

ixy/home/reconfigure:
	RDE_TARGET=ixy-home ${GUIX} home \
	reconfigure ${CONFIGS}

ixy/system/reconfigure:
	RDE_TARGET=ixy-system ${GUIX} system \
	reconfigure ${CONFIGS}

cow-store:
	sudo herd start cow-store ${ROOT_MOUNT_POINT}

ixy/system/init:
	RDE_TARGET=ixy-system ${GUIX} system \
	init ${CONFIGS} ${ROOT_MOUNT_POINT}

target:
	mkdir -p target

live/image/build:
	RDE_TARGET=live-system ${GUIX} system image --image-type=iso9660 \
	${CONFIGS}

target/rde.iso: target
	RDE_TARGET=live-system ${GUIX} system image --image-type=iso9660 \
	${CONFIGS} -r target/rde-tmp.iso
	mv -f target/rde-tmp.iso target/rde.iso

rde/profiles:
	mkdir -p rde/profiles

rde/profiles/guix: rde/profiles rde/channels-lock.scm
	guix pull -C rde/channels-lock.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

rde/profiles/guix-local: rde/profiles rde/channels-lock-local.scm
	guix pull -C rde/channels-lock-local.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

# .PHONY: rde/channels-lock.scm rde/channels-lock-local.scm

rde/channels-lock.scm: rde/channels.scm
	echo -e "(use-modules (guix channels))\n" > ./rde/channels-lock-tmp.scm
	guix time-machine -C ./rde/channels.scm -- \
	describe -f channels >> ./rde/channels-lock-tmp.scm
	mv ./rde/channels-lock-tmp.scm ./rde/channels-lock.scm

rde/channels-lock-local.scm: rde/channels-local.scm
	echo -e "(use-modules (guix channels))\n" > ./rde/channels-lock-tmp.scm
	guix time-machine -C ./rde/channels-local.scm -- \
	describe -f channels >> ./rde/channels-lock-tmp.scm
	mv ./rde/channels-lock-tmp.scm ./rde/channels-lock-local.scm

clean-target:
	rm -rf ./target

clean-profiles:
	rm ./rde/profiles

clean: clean-target clean-profiles
