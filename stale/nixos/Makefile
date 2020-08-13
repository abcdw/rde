build_command = nix-build ec2.nix --no-out-link

ec2:
	${build_command}

deploy:
	nix-deploy system --switch --path `${build_command}` --to root@13.49.18.164 --noSign

xenia.out:
	nix-build -E "(import <nixpkgs/nixos> { configuration = ./xenia/configuration.nix; }).system" -o xenia.out

deploy/xenia: xenia.out
	sudo ./xenia.out/bin/switch-to-configuration switch
	rm xenia.out
