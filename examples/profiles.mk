
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
	guix pull -C rde/channels-lock.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

target/profiles/guix-local: target/profiles rde/channels-lock-local.scm
	guix pull -C rde/channels-lock-local.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

rde/channels-lock.scm: rde/channels.scm
	echo -e "(use-modules (guix channels))\n" > ./rde/channels-lock-tmp.scm
	guix time-machine -C ./rde/channels.scm -- \
	describe -f channels >> ./rde/channels-lock-tmp.scm
	mv ./rde/channels-lock-tmp.scm ./rde/channels-lock.scm

rde/channels-lock-local.scm: rde/channels-local.scm
	echo -e "(use-modules (guix channels))\n" > ./rde/channels-lock-tmp.scm
	guix time-machine -C ./rde/channels-local.scm -- \
	describe -f channels >> ./rde/channels-lock-tmp.scm
	mv ./rde/channels-lock-tmp.scm ./rde/channels-lock-local.scm
