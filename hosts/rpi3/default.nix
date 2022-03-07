{ config, pkgs, ... }:

{
  hardware = {
    enableRedistributableFirmware = pkgs.lib.mkForce false;
    firmware = [ pkgs.raspberrypiWirelessFirmware ];
  };

  networking.networkmanager.enable = true;
  services.openssh.enable = true;
}
