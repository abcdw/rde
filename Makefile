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

ixy.out:
	nix build .#ixy -o ixy.out --experimental-features "flakes nix-command"

ixy/switch: ixy.out
	sudo ./ixy.out/bin/switch-to-configuration switch
	unlink ixy.out
