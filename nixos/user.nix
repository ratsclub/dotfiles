{ config, lib, pkgs, ... }:

{
  users = {
    users.ratsclub = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "docker" "networkmanager" ];
      home = "/home/ratsclub";
      password = "changeme";
    };
  };
}
