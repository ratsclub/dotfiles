{ pkgs, inputs, ... }:

{
  imports = [
    "${inputs.nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
  ];

  boot.kernelPackages = pkgs.linuxPackages_rpi3;
  hardware.enableRedistributableFirmware = true;

  networking.networkmanager.enable = true;

  services.openssh = {
    enable = true;
  };
}
