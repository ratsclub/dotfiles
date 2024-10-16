{ config, lib, inputs, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./networking.nix

    ./forgejo.nix

    ../../modules/common/autoUpgrade.nix
    ../../modules/common/nix.nix
    ../../modules/common/user.nix
  ];

  age.secrets = {
    forgejomail.file = ../../secrets/forgejomail.age;
    forgejomail.owner = config.services.forgejo.user;
    forgejomail.group = config.services.forgejo.group;

    appriseconfig.file = ../../secrets/appriseconfig.age;
  };

  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = true;

  networking = {
    domain = "dev";
    hostName = "capivaras";
    firewall.allowedTCPPorts = [ 22 80 443 ];
  };

  security.acme.defaults.email = "victor@freire.dev.br";
  security.acme.acceptTerms = true;

  services.caddy.enable = true;

  services.tailscale.enable = true;

  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = false;

  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql_16;
  services.postgresqlBackup.enable = true;

  system.stateVersion = "23.11";
}
