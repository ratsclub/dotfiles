{ config, inputs, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../modules/common/nix.nix
      ../../modules/common/user.nix
      inputs.agenix.nixosModules.age
    ];

  boot.loader.grub.device = "/dev/sda";

  networking = {
    hostName = "teresa";
    networkmanager.enable = true;

    # caddy ports
    firewall.allowedTCPPorts = [ 80 443 ];
  };

  time.timeZone = "America/Sao_Paulo";

  # nfs
  environment.systemPackages = with pkgs; [ nfs-utils ];
  boot.initrd = {
    supportedFilesystems = [ "nfs" ];
    kernelModules = [ "nfs" ];
  };
  fileSystems."/mnt/backup" = {
    device = "10.0.0.2:/volume1/backup";
    fsType = "nfs";
  };

  # postgres
  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
    };
    postgresqlBackup = {
      enable = true;
      backupAll = true;
      compression = "zstd";
      location = "/mnt/backup/postgresql";
      startAt = "daily";
    };
  };

  programs.htop.enable = true;
  services.openssh.enable = true;

  system.stateVersion = "22.05";
}
