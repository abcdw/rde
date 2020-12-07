iso:
	guix time-machine -C rde/channels.scm -- system disk-image rde/system/install.scm
env:
	guix time-machine -C rde/channels.scm -- environment make
