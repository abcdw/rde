dotfiles-install:
	stow dotfiles -v 2

dotfiles-uninstall:
	stow -D dotfiles -v 2

etc-install:
	sudo stow -t /etc/ etc

etc-uninstall:
	sudo stow -t /etc/ etc -D

install: etc-install dotfiles-install
	echo "yay!"
