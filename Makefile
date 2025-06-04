# pipefail is not POSIX complaint

GUIXTM=guix time-machine -C ./env/guix/rde/env/guix/channels.scm
GUIX=$(GUIXTM) --
EMACS=$(GUIX) shell emacs emacs-ox-html-stable-ids -- emacs
HUT=$(GUIX) shell hut -- hut

EXAMPLES_SRC_DIR=./examples/src
CONFIGS=${EXAMPLES_SRC_DIR}/rde-configs/configs.scm

DEV_ENV_LOAD_PATH=-L ./env/guix -L ./env/dev -L ./src
RDE_SRC_LOAD_PATH=-L ./env/guix -L ./env/dev \
-L ./src \
-L ./tests \
-L ./files/emacs/gider/src

ALL_SRC_LOAD_PATH=${RDE_SRC_LOAD_PATH} \
-L ${EXAMPLES_SRC_DIR}

QEMU_BASE_ARGS= \
-m 8192 -smp 1 -enable-kvm \
-display gtk,zoom-to-fit=on \
-vga qxl
# -vga none -device virtio-gpu-pci
# -vga vmware
# -vga none -device qxl-vga,vgamem_mb=32


all: ares
	@echo default target

check:
	guile -L ./src -L ./tests -L ./files/emacs/gider/src -c \
	'((@ (rde test-runners) run-project-tests-cli))'

guix:
	make -C examples guix

ares:
	${GUIX} shell ${DEV_ENV_LOAD_PATH} \
	guile-next guile-ares-rs \
	-e '(@ (rde env dev packages) guix-package)' \
	-- guile \
	${ALL_SRC_LOAD_PATH} \
	-c \
"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
((@ (ares server) run-nrepl-server)))"

repl: ares

examples/ixy/home/reconfigure:
	make -C examples ixy/home/reconfigure

examples/ixy/home/build:
	make -C examples ixy/home/build


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
	${GUIX} shell texinfo -- \
	makeinfo --html --no-split \
	--css-ref=/assets/manual.css \
	-c "EXTRA_HEAD=<meta name=\"viewport\" \
content=\"width=device-width, initial-scale=1\" />" \
	-o doc/rde.html doc/rde.texi

doc/rde.pdf: doc/rde.texi
	makeinfo --pdf -o doc/rde.pdf doc/rde.texi

README.html: README
	${EMACS} -Q --batch -l doc/html-export-config.el README \
	--funcall org-html-export-to-html

deploy-README.html: README.html
	${HUT} git update --readme README.html \
	--repo https://git.sr.ht/~abcdw/rde

clean:
	rm -rf target
	rm -f doc/rde.html
	rm -f doc/rde.pdf
	rm -f doc/rde.info
	rm -f doc/rde-tool-list.texi
