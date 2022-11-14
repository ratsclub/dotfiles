{ config, inputs, lib, pkgs, ... }:

{
  age = {
    secrets.wireguard = {
      file = ../../secrets/wireguard.age;
    };
  };

  networking.nat.enable = true;
  networking.nat.externalInterface = "eno1";
  networking.nat.internalInterfaces = [ "wg0" ];
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
  };

  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.1/24" ];
      listenPort = 51820;
      privateKeyFile = config.age.secrets.wireguard.path;

      peers = [
        {
          # victor
          publicKey = "GDE/HOK4t1qUdWfRPkMnraxVAAkKsHjuuVciqeb40l8=";
          allowedIps = [ "10.100.0.2/32" ];
        }
      ];
    };

  };

}
