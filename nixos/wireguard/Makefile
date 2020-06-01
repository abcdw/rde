REMOTE_HOST = 13.49.74.86

.PRECIOUS: keys/%.privkey keys/%.pubkey


clean:
	unlink wireguard.out || true

clean-keys:
	rm -rf keys

clean-clients:
	rm -rf clients


keys/%.privkey:
	mkdir -p keys
	umask 077 && wg genkey > $@

keys/%.pubkey: keys/%.privkey
	cat $< | wg pubkey | tr -d '\n' > $@

clients/%.conf: keys/%.privkey keys/server.pubkey
	echo $^
	mkdir -p clients
	./scripts/genconf $* $^ ${REMOTE_HOST}:51820 > $@

clients/%.qr: clients/%.conf
	qrencode -t ansiutf8 < $<

wireguard.out: keys/server.privkey keys/1.pubkey keys/2.pubkey keys/3.pubkey
	nix-build -o wireguard.out

deploy: wireguard.out
	nix-deploy system --switch --path `readlink $<` --to root@${REMOTE_HOST} --noSign

deploy-reboot: wireguard.out
	nix-deploy system --reboot --path `readlink $<` --to root@${REMOTE_HOST} --noSign

ssh:
	ssh root@${REMOTE_HOST}
