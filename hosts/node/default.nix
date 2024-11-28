{ config, lib, inputs, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/common/autoUpgrade.nix
    ../../modules/common/nix.nix
    ../../modules/common/openssh.nix
    ../../modules/common/user.nix
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  networking.hostName = "node";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Sao_Paulo";

  networking.firewall.enable = true;

  nix-bitcoin.security.dbusHideProcessInformation = true;
  nix-bitcoin.generateSecrets = true;

  # Use doas instead of sudo
  security.doas.enable = true;
  security.sudo.enable = false;
  environment.shellAliases.sudo = "doas";

  environment.systemPackages = with pkgs; [
    jq
  ];

  services.bitcoind = {
    enable = true;
    listen = true;
    dbCache = 1000;
  };

  services.liquidd = {
    # Enable `validatepegin` to verify that a transaction sending BTC into
    # Liquid exists on Bitcoin. Without it, a malicious liquid federation can
    # make the node accept a sidechain that is not fully backed.
    validatepegin = true;
    listen = true;
  };

  nix-bitcoin.nodeinfo.enable = true;

  services.backups.frequency = "daily";

  # operator
  nix-bitcoin.operator.enable = true;
  nix-bitcoin.operator.name = "victor";
}
