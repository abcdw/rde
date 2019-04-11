
dotfiles-install:
	cd dotfiles && \
	stow -t ~/.config config -v 2

dotfiles-uninstall:
	cd dotfiles && \
	stow -t ~/.config -D config -v 2
