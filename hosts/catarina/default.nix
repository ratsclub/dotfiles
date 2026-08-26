{
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix

    inputs.agenix.nixosModules.default
    inputs.microvm.nixosModules.host
    inputs.nixflix.nixosModules.default

    ../../modules/capivaras.nix
    ../../modules/common/nix.nix
    ../../modules/common/openssh.nix
    ../../modules/common/user.nix
    ../../modules/node-exporter.nix

    ./auth.nix
    ./blocky.nix
    ./forgejo.nix
    ./forgejo-runner.nix
    ./karakeep.nix
    ./media.nix
    ./miniflux.nix
    ./monitoring.nix
  ];

  # shared by every restic job on this host
  age.secrets.restic-password.file = ../../secrets/catarina/restic/password.age;
  age.secrets.restic-env.file = ../../secrets/catarina/restic/env.age;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "catarina";
  networking.networkmanager.enable = true;

  services.postgresql.package = pkgs.postgresql_17;

  services.tailscale.enable = true;

  time.timeZone = "America/Sao_Paulo";

  system.stateVersion = "26.05";
}
