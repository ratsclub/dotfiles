{ config, lib, inputs, pkgs, ... }:

let
  user = "victor";
in
{
  imports = [
    ./hardware-configuration.nix

    ../../modules/common/autoUpgrade.nix
    ../../modules/common/nix.nix
    ../../modules/common/user.nix
  ];

  networking = {
    domain = "gluer.org";
    hostName = "magnus";

    firewall.allowedTCPPorts = [ 80 443 ];

    nameservers = [ "8.8.8.8" ];
    defaultGateway = "172.31.1.1";
    defaultGateway6 = {
      address = "fe80::1";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address = "128.140.88.168"; prefixLength = 32; }
        ];
        ipv6.addresses = [
          { address = "2a01:4f8:c010:819a::1"; prefixLength = 64; }
          { address = "fe80::9400:2ff:fe2d:de13"; prefixLength = 64; }
        ];
        ipv4.routes = [{ address = "172.31.1.1"; prefixLength = 32; }];
        ipv6.routes = [{ address = "fe80::1"; prefixLength = 128; }];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="96:00:02:2d:de:13", NAME="eth0"
  '';

  systemd.tmpfiles.rules = [
    # create the websites directories
    "d /var/lib/www/${config.networking.domain} 770 ${user} nginx - -"
  ];
  services.nginx = {
    enable = true;
    virtualHosts =
      {
        "${config.networking.domain}" = {
          enableACME = true;
          forceSSL = true;
          root = "/var/lib/www/${config.networking.domain}";
        };
        "glorifiedgluer.com" = {
          enableACME = true;
          forceSSL = true;
          globalRedirect = config.networking.domain;
        };
      };
  };

  security.acme.defaults.email = "victor@freire.dev.br";
  security.acme.acceptTerms = true;

  services.tailscale.enable = true;

  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = true;

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
    };
  };

  system.stateVersion = "23.11";
}
