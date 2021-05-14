HOSTS = (ixy huixy)

ixy-home-build:
	RDE_TARGET=ixy-home guix home build ./configs.tmpl

ixy-home-reconfigure:
	RDE_TARGET=ixy-home guix home reconfigure ./configs.tmpl

ixy-system-reconfigure:
	RDE_TARGET=ixy-system guix system reconfigure ./configs.tmpl

ixy-reconfigure: ixy-home-reconfigure ixy-system-reconfigure
