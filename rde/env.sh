export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
# export GPG_TTY=$(tty)

gpgconf --launch gpg-agent

light -S 100

if [ "$(tty)" = "/dev/tty1" ]; then
	exec sway
fi
