VERSION=latest
# Also defined in .envrc
GUIX_PROFILE=target/profiles/guix
GUIX=./pre-inst-env ${GUIX_PROFILE}/bin/guix

SRC_DIR=./src
CONFIGS=${SRC_DIR}/abcdw/configs.scm
PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_POINT=/mnt

ixy/home/build: guix
	RDE_TARGET=ixy-home ${GUIX} home \
	build ${CONFIGS}

ixy/home/reconfigure: guix
	RDE_TARGET=ixy-home ${GUIX} home \
	reconfigure ${CONFIGS}

ixy/system/reconfigure: guix
	RDE_TARGET=ixy-system ${GUIX} system \
	reconfigure ${CONFIGS}

cow-store:
	sudo herd start cow-store ${ROOT_MOUNT_POINT}

ixy/system/init: guix
	RDE_TARGET=ixy-system ${GUIX} system \
	init ${CONFIGS} ${ROOT_MOUNT_POINT}

target:
	mkdir -p target

live/image/build: guix
	RDE_TARGET=live-system ${GUIX} system image --image-type=iso9660 \
	${CONFIGS}

target/rde-live.iso: guix target
	RDE_TARGET=live-system ${GUIX} system image --image-type=iso9660 \
	${CONFIGS} -r target/rde-live-tmp.iso
	mv -f target/rde-live-tmp.iso target/rde-live.iso

target/release:
	mkdir -p target/release

# TODO: Prevent is rebuilds.
release/rde-live-x86_64: target/rde-live.iso target/release
	cp -df $< target/release/rde-live-${VERSION}-x86_64.iso
	gpg -ab target/release/rde-live-${VERSION}-x86_64.iso


#
# Profiles
#

# Store items doesn't have useful mtime, so we rely on guix.lock to prevent
# unecessary rebuilds
guix: target/profiles/guix.lock

target/profiles:
	mkdir -p target/profiles

target/profiles/guix.lock: rde/channels-lock.scm
	make target/profiles/guix

target/profiles/guix: target/profiles rde/channels-lock.scm
	guix pull -C rde/channels-lock.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

target/profiles/guix-local: target/profiles rde/channels-lock-local.scm
	guix pull -C rde/channels-lock-local.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

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

clean: clean-target
