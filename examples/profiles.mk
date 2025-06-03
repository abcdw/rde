
#
# Profiles
#

# Store items doesn't have useful mtime, so we rely on guix.lock to prevent
# unecessary rebuilds
guix: env/sync target/guix-time-marker

target/profiles:
	mkdir -p target/profiles

target/guix-time-marker: ./env/rde-configs/env/channels.scm
	make target/profiles/guix
	touch $@

target/profiles/guix: target/profiles ./env/rde-configs/env/channels.scm
	guix pull -C ./env/rde-configs/env/channels.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}
