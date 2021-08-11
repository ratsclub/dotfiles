{ config, lib, pkgs, ... }:

let
  username = "ratsclub";
in
{
  users = {
    users.${username} = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "docker" "networkmanager" ];
      home = "/home/${username}";
      password = "changeme";
    };
  };
}
