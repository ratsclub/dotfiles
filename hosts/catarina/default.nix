{
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix

    inputs.agenix.nixosModules.default
    inputs.nixflix.nixosModules.default

    ../../modules/common/nix.nix
    ../../modules/common/openssh.nix
    ../../modules/common/user.nix
    ../../modules/node-exporter.nix
    ../../modules/tunarr.nix

    ./blocky.nix
    ./forgejo.nix
    ./media.nix
    ./monitoring.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "catarina";
  networking.networkmanager.enable = true;

  services.postgresql.package = pkgs.postgresql_17;

  services.tailscale.enable = true;

  time.timeZone = "America/Sao_Paulo";

  system.stateVersion = "26.05";
}
