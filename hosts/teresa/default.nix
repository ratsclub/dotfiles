{ config, inputs, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./webserver.nix
      ./synapse.nix
      ./tailscale.nix

      ../../modules/common/autoUpgrade.nix
      ../../modules/common/nix.nix
      ../../modules/common/user.nix

      inputs.agenix.nixosModules.age
      inputs.ermo.nixosModules.default
    ];

  boot.loader.grub.device = "/dev/sda";

  networking = {
    hostName = "teresa";
    domain = "glorifiedgluer.com";
    networkmanager.enable = true;
  };

  time.timeZone = "America/Sao_Paulo";

  programs.htop.enable = true;

  services = {
    openssh = {
      enable = true;
      settings.PermitRootLogin = "yes";
    };
  };

  ermo.meta = {
    username = config.users.users.victor.name;
    authorizedKeys = config.users.users.victor.openssh.authorizedKeys.keys;
    domain = "gluer.org";
  };

  ermo.services.cgit = {
    enable = true;
    repositories = {
      "personal/nix-config" = { };
      "public/ermo".extraConfig = ''
        desc=tailored nixos modules for single-user usages
      '';
      "websites/capivaras.dev" = { };
      "websites/gluer.org" = { };
    };
  };

  ermo.services.webserver = {
    enable = true;
    webserver = "caddy";
    websites = [
      { domain = "capivaras.dev"; }
    ];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  services.caddy.enable = true;

  security.acme = {
    acceptTerms = true;
    defaults.email = "victor+acme@freire.dev.br";
  };

  system.stateVersion = "22.05";
}
