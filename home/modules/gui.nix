{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    jetbrains.rider
    jetbrains.pycharm
    jetbrains.goland
    jetbrains.rust-rover
    jetbrains.idea
    jetbrains.webstorm
    jetbrains.datagrip

    signal-desktop
  ];
}
