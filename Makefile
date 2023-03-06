# pipefail is not POSIX complaint

QEMU_BASE_ARGS= \
-m 4096 -smp 1 -enable-kvm \
-vga none -device virtio-gpu-pci
# -vga qxl

all: doc/rde.info
	@echo default target

install:
	@echo some installation will happen here

check:
	./pre-inst-env guile -L ./tests -c \
	'((@ (rde test-runners) run-project-tests-cli))'

repl:
	./pre-inst-env examples/target/profiles/guix/bin/guix repl -L ./tests \
	--listen=tcp:37146

examples/ixy/home/reconfigure:
	make -C examples ixy/home/reconfigure

examples/ixy/home/build:
	make -C examples ixy/home/build

.PHONY: examples/target/rde-live.iso

examples/target/rde-live.iso:
	make -C examples target/rde-live.iso

qemu/1/run:
	qemu-system-x86_64 \
	${QEMU_BASE_ARGS} \
	-net user,hostfwd=tcp::10021-:22 -net nic -boot menu=on,order=d \
	-drive file=tmp/system.img

qemu/1/deploy:
	guix deploy tmp/config.scm --no-grafts

qemu/live/run-from-rde-iso: examples/target/rde-live.iso
	qemu-system-x86_64 \
	${QEMU_BASE_ARGS} \
	-net user,hostfwd=tcp::10022-:22 -net nic -boot menu=on,order=d \
	-drive media=cdrom,file=examples/target/rde-live.iso

doc/rde-tool-list.texi: doc/rde-tool-list.org
	pandoc doc/rde-tool-list.org -f org -t texinfo \
	-o doc/rde-tool-list.texi
	sed -i '1,3d' doc/rde-tool-list.texi

doc/rde.texi: doc/rde-tool-list.texi doc/getting-started.texi

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
