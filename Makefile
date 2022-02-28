# pipefail is not POSIX complaint
all: doc/rde.info

home:
	RDE_TARGET=ixy-home \
	GUILE_LOAD_PATH=./ \
	guix home reconfigure ./rde/examples/abcdw/configs.scm

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

doc/rde-tool-list.texi: doc/rde-tool-list.org
	pandoc doc/rde-tool-list.org -f org -t texinfo \
	-o doc/rde-tool-list.texi
	sed -i '1,3d' doc/rde-tool-list.texi

doc/rde.info: doc/rde.texi
	makeinfo -o doc/rde.info doc/rde.texi

doc/rde.html: doc/rde.texi
	makeinfo --html --no-split \
	--css-ref=https://www.gnu.org/software/gnulib/manual.css \
	-c "EXTRA_HEAD=<meta name=\"viewport\" \
content=\"width=device-width, initial-scale=1\" />" \
	-o doc/rde.html doc/rde.texi

doc/rde.pdf: doc/rde.texi
	makeinfo --pdf -o doc/rde.pdf doc/rde.texi

clean:
	rm -f doc/rde.html
	rm -f doc/rde.pdf
	rm -f doc/rde.info
	rm -f doc/rde-tool-list.texi
