{ config, pkgs, inputs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../nixos/nix.nix
      ../../nixos/user.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.plymouth.enable = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "earth";

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  networking.useDHCP = false;
  networking.interfaces.enp3s0f0.useDHCP = true;
  networking.interfaces.enp4s0.useDHCP = true;
  networking.interfaces.wlp1s0.useDHCP = true;

  services.xserver = {
    enable = true;
    videoDrivers = [ "amdgpu" ];

    desktopManager = {
      gnome.enable = true;
      xterm.enable = false;
    };

    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
  };

  environment = {
    etc = {
      "nix/channels/nixpkgs".source = inputs.nixpkgs;
      "nix/channels/home-manager".source = inputs.home-manager;
    };
  };

  programs.nm-applet.enable = true;

  virtualisation.docker.enable = true;

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "21.05";
}
