
#
# Profiles
#

# Store items doesn't have useful mtime, so we rely on guix.lock to prevent
# unecessary rebuilds
guix: target/guix-time-marker

target/profiles:
	mkdir -p target/profiles

target/guix-time-marker: rde/channels-lock.scm
	make target/profiles/guix
	touch $@

target/profiles/guix: target/profiles rde/channels-lock.scm
	guix pull -C ../env/rde/env/channels.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}
