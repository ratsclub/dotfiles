{ config, ... }:

let
  inherit (config.meta) username;
in
{
  users.mutableUsers = true;

  users.users."${username}" = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "docker"
      "kvm"
      "libvirtd"
      "networkmanager"
      "wheel"
    ];
    initialPassword = "changeme";
  };
}


