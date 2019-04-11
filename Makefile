spacemacs-install:
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

dotfiles-install:
	stow dotfiles -v 2

dotfiles-uninstall:
	stow -D dotfiles -v 2

install: dotfiles-install spacemacs-install
	echo "yay!"
