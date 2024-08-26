{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    jetbrains.rider
    jetbrains.pycharm-professional
    jetbrains.goland
    jetbrains.rust-rover
    signal-desktop
  ];
}
