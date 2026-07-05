{
  inputs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix

    inputs.agenix.nixosModules.default

    ../../modules/common/nix.nix
    ../../modules/common/openssh.nix
    ../../modules/common/user.nix
    ../../modules/forgejo-runner.nix
    ../../modules/node-exporter.nix

    ./forgejo-runner.nix
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  networking.hostName = "joan";
  networking.networkmanager.enable = true;

  services.tailscale.enable = true;

  time.timeZone = "America/Sao_Paulo";

  system.stateVersion = "25.11";
}
