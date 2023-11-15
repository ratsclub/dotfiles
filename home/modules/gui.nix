{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    jetbrains.rider
    signal-desktop
  ];
}
