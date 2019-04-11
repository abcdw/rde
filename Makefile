
dotfiles-install:
	stow dotfiles -v 2

dotfiles-uninstall:
	stow -D dotfiles -v 2
