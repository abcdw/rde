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
        externalInterface = "eth0";
        internalInterfaces = [ "wg0" ];
      };
      networking.firewall = {
        allowedUDPPorts = [ 51820 ];
        allowedTCPPorts = [ 22 8081 ];
        extraCommands = ''
          iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';
      };

      networking.wireguard.interfaces = {
        wg0 = {
          ips = [ "10.100.0.1/24" ];
          listenPort = 51820;
          privateKey = builtins.readFile ./keys/server.privkey;
          peers = [
            {
              publicKey = builtins.readFile ./keys/1.pubkey;
              allowedIPs = [ "10.100.0.101/32" ];
            }
            {
              publicKey = builtins.readFile ./keys/2.pubkey;
              allowedIPs = [ "10.100.0.102/32" ];
            }
            {
              publicKey = builtins.readFile ./keys/3.pubkey;
              allowedIPs = [ "10.100.0.103/32" ];
            }
          ];
        };
      };
    };
  };
in nixos-ec2.system
