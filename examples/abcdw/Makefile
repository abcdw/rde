GUILE_LOAD_PATH=../../..

minimal-emacs:
	guix shell --pure -Df minimal-emacs.scm \
	-E '.*GTK.*|.*XDG.*|.*DISPLAY.*' \
	--rebuild-cache -- emacs -q
	#--eval "(require 'feature-loader-portable)"

.PHONY: help

help:
	$(info help expected here)
