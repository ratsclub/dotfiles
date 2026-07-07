{
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    # Generated on the VM with `nixos-generate-config`. Replace the placeholder
    # in this directory with the real one before installing.
    ./hardware-configuration.nix

    inputs.agenix.nixosModules.default

    ../../modules/common/nix.nix
    ../../modules/common/openssh.nix
    ../../modules/common/user.nix
    ../../modules/node-exporter.nix

    ./blocky.nix
    ./forgejo.nix
    ./monitoring.nix
  ];

  # Legacy BIOS boot: GRUB on the first disk.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "catarina";
  networking.networkmanager.enable = true;

  # postgresql
  services.postgresql.package = pkgs.postgresql_17;

  # tailscale
  services.tailscale.enable = true;

  time.timeZone = "America/Sao_Paulo";

  system.stateVersion = "25.11";
}
