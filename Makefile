# pipefail is not POSIX complaint

ABCDW_DIR=./examples/abcdw
HOME_CONFIG=${ABCDW_DIR}/configs.scm
RDE_TARGET=ixy-home
export RDE_TARGET
# GUIX=guix
GUIX=./pre-inst-env guix
# GUIX=./pre-inst-env ../../gnu/guix/pre-inst-env guix
QEMU_BASE_ARGS= \
-m 4096 -smp 1 -enable-kvm \
-device virtio-gpu-pci -vga qxl

all: doc/rde.info
	@echo default target

install:
	@echo some installation will happen here

check:
	guix time-machine -C ./examples/rde/channels-lock.scm -- \
	home --fallback build ./src/gnu/home/examples/minimal.tmpl

target:
	mkdir target

rde/abcdw/home/reconfigure:
	${GUIX} home --fallback reconfigure --no-grafts --allow-downgrades \
	${HOME_CONFIG}

rde/abcdw/home:
	${GUIX} home --fallback build --no-grafts --allow-downgrades \
	${HOME_CONFIG}

rde/livecd/system:
	guix system --no-grafts \
	-e '(@ (rde system install) rde-livecd)'

rde/livecd/iso: target
	guix system image -t iso9660 --no-grafts \
	-e '(@ (rde system install) rde-livecd)' \
	-r target/rde.iso

rde/channels/pull-latest:
	guix pull -C ./examples/channels.tmpl

rde/channels/pull-locked:
	guix pull -C ./examples/channels-lock.tmpl

rde/channels/update-locked:
	guix time-machine -C ./examples/channels.tmpl -- \
	describe -f channels > ./examples/channels-lock.tmpl


guix/livecd/iso: target
	guix system image -t iso9660 \
	-e '(@ (rde system install) guix-with-substitute-mirror)' \
	-r target/guix.iso


qemu/1/run:
	qemu-system-x86_64 \
	${QEMU_BASE_ARGS} \
	-net user,hostfwd=tcp::10021-:22 -net nic -boot menu=on,order=d \
	-drive file=tmp/system.img

qemu/1/deploy:
	guix deploy tmp/config.scm --no-grafts

qemu/2/run-from-rde-iso: target/rde.iso
	qemu-system-x86_64 \
	${QEMU_BASE_ARGS} \
	-net user,hostfwd=tcp::10022-:22 -net nic -boot menu=on,order=d \
	-drive media=cdrom,file=target/rde.iso


system:
	make -C ../nonrde install


doc/rde-tool-list.texi: doc/rde-tool-list.org
	pandoc doc/rde-tool-list.org -f org -t texinfo \
	-o doc/rde-tool-list.texi
	sed -i '1,3d' doc/rde-tool-list.texi

doc/rde.info: doc/rde.texi
	makeinfo -o doc/rde.info doc/rde.texi

doc/rde.html: doc/rde.texi
	makeinfo --html --no-split \
	--css-ref=/assets/manual.css \
	-c "EXTRA_HEAD=<meta name=\"viewport\" \
content=\"width=device-width, initial-scale=1\" />" \
	-o doc/rde.html doc/rde.texi

doc/rde.pdf: doc/rde.texi
	makeinfo --pdf -o doc/rde.pdf doc/rde.texi

clean:
	rm -rf target
	rm -f doc/rde.html
	rm -f doc/rde.pdf
	rm -f doc/rde.info
	rm -f doc/rde-tool-list.texi
