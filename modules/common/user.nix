{ config, lib, pkgs, ... }:

let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIEpBZXIOn5Eeq2peV4gH3jSf2fqinRnTPHd1NHlscLZ"

    # yubikeys
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIDWji4AKnNF0O1Y4BZqP5fbkFwuzSt0CS8qEY+fwsXGOAAAABHNzaDo="
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIIijs6xqu+aad4tMclFUDZFHTBoi7W/W+At6ouKvX7DdAAAABHNzaDo="
  ];
in
{
  users.users = {
    root.openssh.authorizedKeys.keys = keys;

    victor = {
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

      openssh.authorizedKeys.keys = keys;
    };
  };
}
