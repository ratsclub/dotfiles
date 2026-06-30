{
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

    ./forgejo-runner.nix
  ];

  # Legacy BIOS boot: GRUB on the first disk.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";

  # Build aarch64-linux derivations on this x86_64 host via qemu user-mode
  # emulation. This registers the qemu interpreter with binfmt and adds
  # aarch64-linux to nix's extra-platforms, so the runner can produce ARM Linux
  # outputs (slow, but no extra machine needed). There is no equivalent for
  # darwin — macOS targets require a real Mac as a remote builder.
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  networking.hostName = "joan";
  networking.networkmanager.enable = true;

  # tailscale
  services.tailscale.enable = true;

  time.timeZone = "America/Sao_Paulo";

  system.stateVersion = "25.11";
}
