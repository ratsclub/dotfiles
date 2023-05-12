{ config, ... }:

{
  system.autoUpgrade = {
    enable = true;
    flake = "sourcehut:~glorifiedgluer/nix-config";
    dates = "daily";
  };
}
