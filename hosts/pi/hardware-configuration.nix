{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "usbhid"
    "uas"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/305ef5bb-7f87-4b35-ad0f-1302f155fcd4";
    fsType = "ext4";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/a780e585-2dfd-4ee1-b3c7-e14929015645"; }
  ];

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
}
