CHANNELS_FILE=./env/guix/rde-configs/env/guix/channels.scm
CHANNELS_FILE_LOCAL_RDE=./env/guix/rde-configs/env/guix/channels-ci.scm
GUIXTM=GUILE_LOAD_PATH="../src:../tests:./src" guix time-machine -L ./env/guix -C ${CHANNELS_FILE}
GUIXTM_LOCAL_RDE=guix time-machine -L ./env/guix -C ${CHANNELS_FILE_LOCAL_RDE}

GUIX=$(GUIXTM) --

SRC_DIR=./src
CONFIGS=${SRC_DIR}/rde-configs/configs.scm
PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_POINT=/mnt

VERSION=latest

repl: ares

ares: env/sync
	${GUIX} shell -L ./env/guix -L ./env/dev guile-next guile-ares-rs \
	-e '(@ (rde-configs env dev packages) guix-package)' \
	-e '(@ (rde-configs env dev packages) channels-package)' \
	-- guile \
	-L ./env/guix \
	-L ./env/dev \
	-c \
"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
((@ (ares server) run-nrepl-server)))"

env/guix/rde/env/guix/channels.scm: ../env/guix/rde/env/guix/channels.scm
	mkdir -p $(@D)
	cat $< > $@
	echo -e "\n;;; This is an automatic copy of RDE's channels.scm" >> $@
	echo -e ";;; Do not edit it manually" >> $@

env/sync: env/guix/rde/env/guix/channels.scm
guix: env/sync

guix-pull:
	guix pull -L ./env/guix -C ${CHANNELS_FILE}

ixy/home/build: guix
	RDE_TARGET=ixy-home ${GUIX} home \
	build ${CONFIGS}

ixy/home/reconfigure: guix
	RDE_TARGET=ixy-home ${GUIX} home \
	reconfigure ${CONFIGS}

ixy/system/build: guix
	RDE_TARGET=ixy-system ${GUIX} system \
	build ${CONFIGS}

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

minimal-emacs: guix
	${GUIX} shell --pure -Df ./src/rde-configs/minimal-emacs.scm \
	-E '.*GTK.*|.*XDG.*|.*DISPLAY.*' \
	--rebuild-cache -- emacs -q \
	--eval "(load \"~/.config/emacs/early-init.el\")"
	#--eval "(require 'feature-loader-portable)"

minimal/home/build: guix
	${GUIX} home build ./src/rde-configs/minimal.scm

minimal/home/build/ci:
	${GUIXTM_LOCAL_RDE} -- \
	home build ./src/rde-configs/minimal.scm

minimal/home/build/ci-no-auth:
	${GUIXTM_LOCAL_RDE} \
	--disable-authentication -- \
	home build ./src/rde-configs/minimal.scm

cloud-rde: guix
	${GUIX} system image -e '(@ (rde-configs hosts cloud) cloud-image)'

clean-target:
	rm -rf ./target

clean: clean-target
