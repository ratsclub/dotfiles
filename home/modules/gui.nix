{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    jetbrains.rider
    jetbrains.pycharm-professional
    jetbrains.goland
    jetbrains.rust-rover
    jetbrains.idea-ultimate
    jetbrains.webstorm

    signal-desktop
  ];
}
