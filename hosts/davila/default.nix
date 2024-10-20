# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, inputs, pkgs, ... }:

let
  capivarasdevCfg = inputs.self.nixosConfigurations.capivaras.config;
in
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix

      ../../modules/common/autoUpgrade.nix
      ../../modules/common/nix.nix
      ../../modules/common/openssh.nix
      ../../modules/common/user.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  networking.hostName = "davila";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Sao_Paulo";

  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
    autoPrune.dates = "weekly";
  };

  age.secrets = {
    forgejo-runner-token.file = ../../secrets/services/forgejo/runner-token.age;
  };

  services.gitea-actions-runner = {
    package = pkgs.forgejo-runner;
    instances.capivarasdev = {
      enable = true;
      name = "Global Docker Forgejo Actions Runner";
      url = capivarasdevCfg.services.forgejo.settings.server.ROOT_URL;
      tokenFile = config.age.secrets.forgejo-runner-token.path;
      labels = [
        "nix:host"
        "docker:docker://node:current-bookworm"
        "ubuntu-latest:docker://node:current-bookworm"
      ];
      hostPackages = with pkgs; [
        # default ones
        bash
        coreutils
        curl
        gawk
        git
        gnused
        nodejs
        wget

        # useful to have in path
        jq
        which
        dpkg
        zip
        git-lfs

        # used in deployments
        nix
        openssh
        sudo
      ];
    };
  };

  services.tailscale.enable = true;

  system.stateVersion = "24.05";
}
