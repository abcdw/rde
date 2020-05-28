let
  nixos-ec2 = import <nixos-20.03/nixos> {
    system = "x86_64-linux";

    configuration = {
      imports = [ <nixos-20.03/nixos/modules/virtualisation/amazon-image.nix> ];
      boot.extraModulePackages = [ nixos-ec2.config.boot.kernelPackages.wireguard ];

      ec2.hvm = true;

      users.users.abcdw = {
        group = "users";
        extraGroups = [
          "wheel"
          "disk"
          "audio"
          "video"
          "vboxusers"
          "networkmanager"
          "systemd-journal"
        ];
      };

      environment.systemPackages = with nixos-ec2.pkgs; [
        wireguard
        git
        htop
      ];

      networking.hostName = "nixos-system";

      networking.nat = {
        enable = true;
        externalInterface = "ens5";
        internalInterfaces = [ "wg0" ];
      };
      networking.firewall = {
        allowedUDPPorts = [ 51820 ];
        allowedTCPPorts = [ 22 8081 ];
        # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
        # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
        extraCommands = ''
          iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o ens5 -j MASQUERADE
        '';
      };

      networking.wireguard.interfaces = {
        # "wg0" is the network interface name. You can name the interface arbitrarily.
        wg0 = {
          # Determines the IP address and subnet of the server's end of the tunnel interface.
          ips = [ "10.100.0.1/24" ];

          # The port that Wireguard listens to. Must be accessible by the client.
          listenPort = 51820;

          # Path to the private key file.
          #
          # Note: The private key can also be included inline via the privateKey option,
          # but this makes the private key world-readable; thus, using privateKeyFile is
          # recommended.
          # privateKeyFile = "path to private key file";

          privateKey = builtins.readFile ./server.privatekey;
          peers = [
            # List of allowed peers.
            { # Feel free to give a meaning full name
              # Public key of the peer (not a file path).
              publicKey = "6xk0aXHm4Bfx6oZhn9ovcNhJEto5yGfJAofABstD11c=";
              # List of IPs assigned to this peer within the tunnel subnet. Used to configure routing.
              allowedIPs = [ "10.100.0.2/32" ];
            }
          ];
        };
      };
    };
  };
in nixos-ec2.system
