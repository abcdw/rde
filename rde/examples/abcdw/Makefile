CONFIG_FILE = ./configs.scm
HOSTS = ixy
GLP = ../../../

targets-home = $(addsuffix -home, $(HOSTS))
targets-system = $(addsuffix -system, $(HOSTS))

# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
%-home-build:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-home \
	guix home build $(CONFIG_FILE)

%-home-reconfigure:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-home \
	guix home reconfigure $(CONFIG_FILE)

%-system-build:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-system \
	guix system build $(CONFIG_FILE)

%-system-reconfigure:
	GUILE_LOAD_PATH=$(GLP) RDE_TARGET=$*-system \
	guix system reconfigure $(CONFIG_FILE)

$(targets-home): %-home-reconfigure
$(targets-system): %-system-reconfigure

.PHONY: help

help:
	$(info The targets are '$(targets-home)' and '$(targets-system)')
