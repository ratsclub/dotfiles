{ config, inputs, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../modules/common/nix.nix
      ../../modules/common/user.nix
      ../../modules/services/readarr.nix
      ./media.nix
      ./monitoring

      inputs.agenix.nixosModules.age
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];

  networking = {
    hostName = "teresa";
    hostId = "649e39eb";
    networkmanager.enable = true;

    # caddy ports
    firewall.allowedTCPPorts = [ 80 443 ];
  };

  time.timeZone = "America/Sao_Paulo";

  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.dnsname.enable = true;
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  system.stateVersion = "22.05";
}
