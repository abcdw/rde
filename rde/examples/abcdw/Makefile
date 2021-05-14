HOSTS = (ixy huixy)

ixy-home-build:
	RDE_TARGET=ixy-home guix home build ./configs.scm

ixy-home-reconfigure:
	RDE_TARGET=ixy-home guix home reconfigure ./configs.scm

ixy-system-reconfigure:
	RDE_TARGET=ixy-system guix system reconfigure ./configs.scm

ixy-reconfigure: ixy-home-reconfigure ixy-system-reconfigure
