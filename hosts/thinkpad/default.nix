{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../nixos/user.nix
      ../../nixos/nix.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.plymouth.enable = true;

  networking.hostName = "t495";

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  networking.useDHCP = false;
  networking.interfaces.enp3s0f0.useDHCP = true;
  networking.interfaces.enp4s0.useDHCP = true;
  networking.interfaces.wlp1s0.useDHCP = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" ];

  # Enable the GNOME 3 Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
  };

  system.stateVersion = "21.05";
}

