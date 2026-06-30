{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    jetbrains.rider
    jetbrains.pycharm
    jetbrains.goland
    jetbrains.rust-rover
    jetbrains.idea-ultimate
    jetbrains.webstorm
    jetbrains.datagrip

    signal-desktop
  ];
}
