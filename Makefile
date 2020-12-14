iso:
	guix time-machine  -C rde/channels.scm -- system -L ./ disk-image -t iso9660 rde/system/install.scm
env:
	guix time-machine -C rde/channels.scm -- environment make
