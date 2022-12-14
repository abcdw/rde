GUIX=./pre-inst-env guix

ABCDW_DIR=./abcdw
HOME_CONFIG=${ABCDW_DIR}/configs.scm

ixy/home/reconfigure:
	RDE_TARGET=ixy-home ${GUIX} home \
	--fallback reconfigure --no-grafts --allow-downgrades \
	${HOME_CONFIG}
