install: switch
	echo "yay!"

ixy.out:
	nix build .#ixy -o ixy.out --experimental-features "flakes nix-command"

ixy/switch: ixy.out
	sudo ./ixy.out/bin/switch-to-configuration switch
	unlink ixy.out

switch:
	sudo nixos-rebuild switch --flake .

repl:
	nix repl src/flake-repl.nix

clean:
	unlink ./result
