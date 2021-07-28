{ config, pkgs, ... }:

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

    # Enable i3 window manager
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu #application launcher most people use
        i3status # gives you the default i3 status bar
        i3blocks
        i3lock #default i3 screen locker
        i3blocks #if you are planning on using i3blocks over i3status
      ];
    };
  };

  programs.nm-applet.enable = true;

  virtualisation.docker.enable = true;

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "21.05";
}
