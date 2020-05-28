REMOTE_HOST = 13.49.18.164
build_command = nix-build config.nix -o wg.out

wg.out:
	${build_command}

deploy: wg.out
	nix-deploy system --switch --path ./wg.out --to root@${REMOTE_HOST} --noSign
