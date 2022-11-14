{ config, lib, pkgs, ... }:

{
  users.users.victor = {
    isNormalUser = true;
    initialPassword = "changeme";
    extraGroups = [
      "wheel"
      "video"
      "audio"
      "networkmanager"
      "podman"
      "libvirtd"
    ];

    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIEpBZXIOn5Eeq2peV4gH3jSf2fqinRnTPHd1NHlscLZ"
    ];
  };
}
