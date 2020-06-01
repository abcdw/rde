let
  nixos-ec2 = import <nixpkgs/nixos> {
    system = "x86_64-linux";
    configuration = {
      imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
      boot.extraModulePackages =
        [ nixos-ec2.config.boot.kernelPackages.wireguard ];

      ec2.hvm = true;

      environment.systemPackages = with nixos-ec2.pkgs; [ wireguard git htop ];

      networking.hostName = "wg-server";

      networking.nat = {
        enable = true;
        externalInterface = "ens5";
        internalInterfaces = [ "wg0" ];
      };
      networking.firewall = {
        allowedUDPPorts = [ 51820 ];
        allowedTCPPorts = [ 22 8081 ];
        extraCommands = ''
          iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o ens5 -j MASQUERADE
        '';
      };

      networking.wireguard.interfaces = {
        wg0 = {
          ips = [ "10.100.0.1/24" ];
          listenPort = 51820;
          privateKey = builtins.readFile ./server.privatekey;
          peers = [
            {
              publicKey = builtins.readFile ./client.pubkey;
              allowedIPs = [ "10.100.0.2/32" "10.100.0.3/32" ];
            }
          ];
        };
      };
    };
  };
in nixos-ec2.system
